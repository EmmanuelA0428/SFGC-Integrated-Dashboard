# internal/modules/auth_io.R
# Dashboard user accounts (Neon app.users) and shinymanager login bridge.

library(sodium)

# CLI identity is separate from web sessions. Never share web-user roles globally.
.app_user_state <- new.env(parent = emptyenv())

auth_session <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) return(NULL)
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session) && is.function(session$rootScope)) session <- session$rootScope()
  session
}

auth_state_cell <- function(session = auth_session()) {
  if (is.null(session)) return(NULL)
  if (is.null(session$userData$sfgc_auth_state)) {
    session$userData$sfgc_auth_state <- shiny::reactiveVal(
      list(username = "", role = "viewer", is_admin = FALSE)
    )
  }
  session$userData$sfgc_auth_state
}

auth_current_state <- function() {
  cell <- auth_state_cell()
  if (is.null(cell)) return(as.list(.app_user_state))
  # Normal reads retain reactive dependencies; isolated reads support CLI/QA callers.
  tryCatch(cell(), error = function(e) shiny::isolate(cell()))
}

USER_ROLES <- c("admin", "editor", "viewer")
USER_ROLE_LABELS <- c(
  admin  = "Administrator",
  editor = "Editor",
  viewer = "Viewer"
)

auth_normalize_username <- function(username) {
  tolower(trimws(as.character(username %||% "")))
}

auth_admin_flag <- function(adm) {
  if (isTRUE(adm)) return(TRUE)
  if (isFALSE(adm) || is.na(adm)) return(FALSE)
  tolower(trimws(as.character(adm))) %in% c("true", "t", "1", "yes")
}

auth_normalize_role <- function(role, is_admin = NULL) {
  r <- tolower(trimws(as.character(role %||% "")))
  if (nzchar(r) && r %in% USER_ROLES) return(r)
  if (!is.null(is_admin) && auth_admin_flag(is_admin)) return("admin")
  "viewer"
}

auth_role_label <- function(role) {
  r <- auth_normalize_role(role)
  out <- USER_ROLE_LABELS[r]
  if (is.na(out)) r else out
}

auth_can_admin <- function(role) {
  identical(auth_normalize_role(role), "admin")
}

auth_can_edit <- function(role) {
  auth_can_admin(role) || identical(auth_normalize_role(role), "editor")
}

set_app_user <- function(username, is_admin = NULL, role = NULL) {
  username <- trimws(as.character(username %||% ""))
  role <- if (!nzchar(username)) "viewer" else auth_normalize_role(role, is_admin)
  state <- list(username = username, role = role, is_admin = auth_can_admin(role))
  cell <- auth_state_cell()
  if (!is.null(cell)) {
    cell(state)
  } else {
    .app_user_state$username <- state$username
    .app_user_state$role <- state$role
    .app_user_state$is_admin <- state$is_admin
  }
  invisible(state)
}

get_app_is_admin <- function() auth_can_admin(get_app_role())

get_app_role <- function() {
  state <- auth_current_state()
  auth_normalize_role(state$role, state$is_admin)
}

get_app_can_edit <- function() auth_can_edit(get_app_role())

get_app_user <- function() {
  state <- auth_current_state()
  u <- as.character(state$username %||% "")
  if (nzchar(trimws(u))) return(trimws(u))
  if (!is.null(auth_session())) return("")
  Sys.info()[["user"]]
}

# Demo accounts require explicit opt-in AND caller-supplied local passwords.
# They are never a fallback for a configured or unavailable production database.
local_auth_enabled <- function() {
  identical(tolower(Sys.getenv("SFGC_DEMO_MODE")), "true") &&
    !nzchar(Sys.getenv("DATABASE_URL"))
}

# Local-only account labels. No passwords are shipped with the application.
.auth_demo_users <- data.frame(
  username      = c("admin", "editor", "viewer"),
  display_name  = c("Admin User", "Editor User", "Viewer User"),
  is_admin      = c(TRUE, FALSE, FALSE),
  role          = c("admin", "editor", "viewer"),
  stringsAsFactors = FALSE
)

neon_ensure_users_role_column <- function(con = get_db_con()) {
  cols <- DBI::dbGetQuery(
    con,
    "SELECT column_name FROM information_schema.columns
     WHERE table_schema = 'app' AND table_name = 'users'"
  )$column_name
  if (!"role" %in% cols) {
    DBI::dbExecute(
      con,
      "ALTER TABLE app.users ADD COLUMN role TEXT NOT NULL DEFAULT 'editor'"
    )
    DBI::dbExecute(
      con,
      "UPDATE app.users SET role = 'admin' WHERE is_admin = TRUE"
    )
  }
}

neon_ensure_users_table <- function(con = get_db_con()) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS app.users (
      id            SERIAL PRIMARY KEY,
      username      TEXT NOT NULL UNIQUE,
      password_hash TEXT NOT NULL,
      display_name  TEXT NOT NULL DEFAULT '',
      is_admin      BOOLEAN NOT NULL DEFAULT FALSE,
      role          TEXT NOT NULL DEFAULT 'editor',
      active        BOOLEAN NOT NULL DEFAULT TRUE,
      created_at    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      updated_at    TIMESTAMPTZ NOT NULL DEFAULT NOW()
    )
  ")
  neon_ensure_users_role_column(con)
}

neon_seed_admin_user <- function(con = get_db_con(), password = NULL) {
  neon_ensure_users_table(con)
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*)::int AS n FROM app.users")$n
  if (n > 0L) return(invisible(FALSE))
  pwd <- password %||% Sys.getenv("BOOTSTRAP_ADMIN_PASSWORD", unset = "")
  if (nchar(trimws(pwd)) < 12L) {
    stop("Set a unique BOOTSTRAP_ADMIN_PASSWORD of at least 12 characters before initializing an empty database.")
  }
  hash <- sodium::password_store(pwd)
  q_u <- DBI::dbQuoteString(con, "admin")
  q_h <- DBI::dbQuoteString(con, hash)
  DBI::dbExecute(
    con,
    sprintf(
      "INSERT INTO app.users (username, password_hash, display_name, is_admin, role, active)
       VALUES (%s, %s, 'Administrator', TRUE, 'admin', TRUE)",
      q_u, q_h
    )
  )
  message("Seeded initial admin user (username: admin). Change password after first login.")
  invisible(TRUE)
}

neon_auth_fetch_user <- function(username, active_only = TRUE) {
  username <- auth_normalize_username(username)
  if (!nzchar(username)) return(data.frame())
  con <- get_db_con()
  q_u <- DBI::dbQuoteString(con, username)
  active_clause <- if (isTRUE(active_only)) " AND active = TRUE" else ""
  DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT id, username, password_hash, display_name, is_admin, role, active,
              created_at, updated_at
       FROM app.users WHERE LOWER(username) = %s%s",
      q_u, active_clause
    )
  )
}

neon_auth_fetch_user_retry <- function(username, active_only = TRUE, attempts = 4L) {
  username <- trimws(as.character(username))
  for (i in seq_len(attempts)) {
    row <- tryCatch(
      neon_auth_fetch_user(username, active_only = active_only),
      error = function(e) {
        message("[auth] user fetch failed (attempt ", i, "): ", conditionMessage(e))
        db_disconnect_all()
        NULL
      }
    )
    if (!is.null(row) && nrow(row) == 1L) return(row)
    if (i < attempts) Sys.sleep(0.15 * (2^(i - 1L)))
  }
  data.frame()
}

neon_auth_verify_login <- function(username, password) {
  username <- auth_normalize_username(username)
  password <- as.character(password %||% "")
  if (!nzchar(username) || !nzchar(password)) return(NULL)
  row <- neon_auth_fetch_user_retry(username, active_only = TRUE)
  if (nrow(row) != 1L) return(NULL)
  if (!sodium::password_verify(row$password_hash[[1]], password)) return(NULL)
  list(
    username     = row$username[[1]],
    display_name = row$display_name[[1]],
    is_admin     = auth_can_admin(row$role[[1]] %||% row$is_admin[[1]]),
    role         = auth_normalize_role(row$role[[1]], is_admin = row$is_admin[[1]])
  )
}

neon_auth_list_users <- function(include_inactive = FALSE) {
  con <- get_db_con()
  clause <- if (isTRUE(include_inactive)) "" else "WHERE active = TRUE"
  df <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT id, username, display_name, is_admin, role, active, created_at, updated_at
       FROM app.users %s ORDER BY username",
      clause
    )
  )
  if (nrow(df)) {
    df$created_at <- as.character(df$created_at)
    df$updated_at <- as.character(df$updated_at)
  }
  df
}

neon_auth_count_active_admins <- function(exclude_username = NULL) {
  con <- get_db_con()
  ex <- trimws(as.character(exclude_username %||% ""))
  sql <- "SELECT COUNT(*)::int AS n FROM app.users WHERE active = TRUE AND role = 'admin'"
  if (nzchar(ex)) {
    sql <- paste0(
      sql, " AND username <> ",
      DBI::dbQuoteString(con, ex)
    )
  }
  DBI::dbGetQuery(con, sql)$n
}

neon_auth_create_user <- function(username, password, display_name,
                                  is_admin = FALSE, role = NULL) {
  username <- auth_normalize_username(username)
  display_name <- trimws(as.character(display_name))
  want_role <- auth_normalize_role(role, is_admin = is_admin)
  if (!nzchar(username)) return(list(ok = FALSE, error = "Username is required."))
  if (!nzchar(password) || nchar(password) < 8) {
    return(list(ok = FALSE, error = "Password must be at least 8 characters."))
  }
  existing <- neon_auth_fetch_user(username, active_only = FALSE)
  if (nrow(existing) > 0L) {
    return(list(ok = FALSE, error = "That username already exists."))
  }
  con <- get_db_con()
  hash <- sodium::password_store(password)
  tryCatch({
    DBI::dbExecute(
      con,
      sprintf(
        "INSERT INTO app.users (username, password_hash, display_name, is_admin, role, active)
         VALUES (%s, %s, %s, %s, %s, TRUE)",
        DBI::dbQuoteString(con, username),
        DBI::dbQuoteString(con, hash),
        DBI::dbQuoteString(con, display_name %||% username),
        if (auth_can_admin(want_role)) "TRUE" else "FALSE",
        DBI::dbQuoteString(con, want_role)
      )
    )
    list(ok = TRUE)
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
}

neon_auth_deactivate_user <- function(username, actor_username) {
  username <- auth_normalize_username(username)
  actor_username <- auth_normalize_username(actor_username)
  if (!nzchar(username)) return(list(ok = FALSE, error = "Username is required."))
  if (identical(username, actor_username)) {
    return(list(ok = FALSE, error = "You cannot deactivate your own account."))
  }
  row <- neon_auth_fetch_user(username, active_only = FALSE)
  if (nrow(row) != 1L) return(list(ok = FALSE, error = "User not found."))
  if (!isTRUE(row$active[[1]])) return(list(ok = TRUE))
  if (auth_can_admin(row$role[[1]] %||% row$is_admin[[1]]) &&
      neon_auth_count_active_admins(exclude_username = username) < 1L) {
    return(list(ok = FALSE, error = "Cannot deactivate the last active admin."))
  }
  con <- get_db_con()
  DBI::dbExecute(
    con,
    sprintf(
      "UPDATE app.users SET active = FALSE, updated_at = NOW()
       WHERE LOWER(username) = %s",
      DBI::dbQuoteString(con, username)
    )
  )
  list(ok = TRUE)
}

neon_auth_delete_user <- function(username, actor_username) {
  username <- auth_normalize_username(username)
  actor_username <- auth_normalize_username(actor_username)
  if (!nzchar(username)) return(list(ok = FALSE, error = "Username is required."))
  if (identical(username, actor_username)) {
    return(list(ok = FALSE, error = "You cannot delete your own account."))
  }
  row <- neon_auth_fetch_user(username, active_only = FALSE)
  if (nrow(row) != 1L) return(list(ok = FALSE, error = "User not found."))
  if (auth_can_admin(row$role[[1]] %||% row$is_admin[[1]]) && isTRUE(row$active[[1]]) &&
      neon_auth_count_active_admins(exclude_username = username) < 1L) {
    return(list(ok = FALSE, error = "Cannot delete the last active administrator."))
  }
  con <- get_db_con()
  DBI::dbExecute(
    con,
    sprintf(
      "DELETE FROM app.users WHERE LOWER(username) = %s",
      DBI::dbQuoteString(con, username)
    )
  )
  list(ok = TRUE)
}

neon_auth_reactivate_user <- function(username, actor_username) {
  username <- auth_normalize_username(username)
  if (!nzchar(username)) return(list(ok = FALSE, error = "Username is required."))
  row <- neon_auth_fetch_user(username, active_only = FALSE)
  if (nrow(row) != 1L) return(list(ok = FALSE, error = "User not found."))
  if (isTRUE(row$active[[1]])) return(list(ok = TRUE))
  con <- get_db_con()
  DBI::dbExecute(
    con,
    sprintf(
      "UPDATE app.users SET active = TRUE, updated_at = NOW()
       WHERE LOWER(username) = %s",
      DBI::dbQuoteString(con, username)
    )
  )
  list(ok = TRUE)
}

neon_auth_set_user_role <- function(username, role, actor_username) {
  username <- auth_normalize_username(username)
  actor_username <- auth_normalize_username(actor_username)
  want_role <- auth_normalize_role(role)
  if (!nzchar(username)) return(list(ok = FALSE, error = "Username is required."))
  row <- neon_auth_fetch_user(username, active_only = FALSE)
  if (nrow(row) != 1L) return(list(ok = FALSE, error = "User not found."))
  if (!isTRUE(row$active[[1]])) {
    return(list(ok = FALSE, error = "Only active users can change role."))
  }
  current_role <- auth_normalize_role(row$role[[1]], is_admin = row$is_admin[[1]])
  if (identical(current_role, want_role)) return(list(ok = TRUE))
  if (auth_can_admin(current_role) && !auth_can_admin(want_role) &&
      identical(username, actor_username)) {
    return(list(ok = FALSE, error = "You cannot remove your own administrator access."))
  }
  if (auth_can_admin(current_role) && !auth_can_admin(want_role) &&
      neon_auth_count_active_admins(exclude_username = username) < 1L) {
    return(list(ok = FALSE, error = "Cannot remove the last active administrator."))
  }
  con <- get_db_con()
  DBI::dbExecute(
    con,
    sprintf(
      "UPDATE app.users SET role = %s, is_admin = %s, updated_at = NOW()
       WHERE LOWER(username) = %s",
      DBI::dbQuoteString(con, want_role),
      if (auth_can_admin(want_role)) "TRUE" else "FALSE",
      DBI::dbQuoteString(con, username)
    )
  )
  list(ok = TRUE)
}

neon_auth_set_user_admin <- function(username, is_admin, actor_username) {
  neon_auth_set_user_role(
    username,
    if (isTRUE(is_admin)) "admin" else "editor",
    actor_username
  )
}

neon_auth_reset_password <- function(username, new_password, actor_username) {
  actor_username <- trimws(as.character(actor_username))
  actor <- neon_auth_fetch_user(actor_username, active_only = TRUE)
  if (nrow(actor) != 1L ||
      !auth_can_admin(actor$role[[1]] %||% actor$is_admin[[1]])) {
    return(list(ok = FALSE, error = "Only administrators can reset passwords."))
  }
  neon_auth_change_password(username, new_password, require_current = FALSE)
}

neon_auth_update_display_name <- function(username, display_name) {
  username <- auth_normalize_username(username)
  display_name <- trimws(as.character(display_name))
  if (!nzchar(username)) return(list(ok = FALSE, error = "Username is required."))
  if (!nzchar(display_name)) return(list(ok = FALSE, error = "Display name is required."))
  con <- get_db_con()
  DBI::dbExecute(
    con,
    sprintf(
      "UPDATE app.users SET display_name = %s, updated_at = NOW()
       WHERE LOWER(username) = %s AND active = TRUE",
      DBI::dbQuoteString(con, display_name),
      DBI::dbQuoteString(con, username)
    )
  )
  list(ok = TRUE, display_name = display_name)
}

neon_auth_change_password <- function(username, new_password,
                                      current_password = NULL,
                                      require_current = TRUE) {
  username <- auth_normalize_username(username)
  if (!nzchar(username)) return(list(ok = FALSE, error = "Username is required."))
  if (!nzchar(new_password) || nchar(new_password) < 8) {
    return(list(ok = FALSE, error = "New password must be at least 8 characters."))
  }
  row <- neon_auth_fetch_user(username, active_only = isTRUE(require_current))
  if (nrow(row) != 1L) return(list(ok = FALSE, error = "User not found."))
  if (isTRUE(require_current)) {
    if (is.null(current_password) || !nzchar(current_password)) {
      return(list(ok = FALSE, error = "Current password is required."))
    }
    if (!sodium::password_verify(row$password_hash[[1]], current_password)) {
      return(list(ok = FALSE, error = "Current password is incorrect."))
    }
  }
  hash <- sodium::password_store(new_password)
  con <- get_db_con()
  DBI::dbExecute(
    con,
    sprintf(
      "UPDATE app.users SET password_hash = %s, updated_at = NOW()
       WHERE LOWER(username) = %s",
      DBI::dbQuoteString(con, hash),
      DBI::dbQuoteString(con, username)
    )
  )
  list(ok = TRUE)
}

local_auth_verify <- function(username, password) {
  if (!local_auth_enabled()) return(NULL)
  username <- auth_normalize_username(username)
  row <- .auth_demo_users[.auth_demo_users$username == username, , drop = FALSE]
  if (nrow(row) != 1L) return(NULL)
  expected <- Sys.getenv(paste0("SFGC_DEMO_", toupper(username), "_PASSWORD"), unset = "")
  if (nchar(expected) < 8L || !identical(as.character(password), expected)) return(NULL)
  list(
    username     = row$username[[1]],
    display_name = row$display_name[[1]],
    is_admin     = auth_can_admin(row$role[[1]] %||% row$is_admin[[1]]),
    role         = auth_normalize_role(row$role[[1]], is_admin = row$is_admin[[1]])
  )
}

auth_verify_login <- function(username, password) {
  if (use_neon_db()) neon_auth_verify_login(username, password)
  else if (nzchar(Sys.getenv("DATABASE_URL"))) NULL
  else local_auth_verify(username, password)
}

auth_list_users <- function(include_inactive = FALSE) {
  if (use_neon_db()) neon_auth_list_users(include_inactive)
  else if (local_auth_enabled()) .auth_demo_users else .auth_demo_users[FALSE, ]
}

auth_create_user <- function(username, password, display_name,
                             is_admin = FALSE, role = NULL) {
  if (!use_neon_db()) {
    return(list(ok = FALSE, error = "User management requires Neon (DATABASE_URL)."))
  }
  neon_auth_create_user(username, password, display_name, is_admin, role)
}

auth_deactivate_user <- function(username, actor_username) {
  if (!use_neon_db()) {
    return(list(ok = FALSE, error = "User management requires Neon (DATABASE_URL)."))
  }
  neon_auth_deactivate_user(username, actor_username)
}

auth_set_user_admin <- function(username, is_admin, actor_username) {
  if (!use_neon_db()) {
    return(list(ok = FALSE, error = "User management requires Neon (DATABASE_URL)."))
  }
  neon_auth_set_user_admin(username, is_admin, actor_username)
}

auth_set_user_role <- function(username, role, actor_username) {
  if (!use_neon_db()) {
    return(list(ok = FALSE, error = "User management requires Neon (DATABASE_URL)."))
  }
  neon_auth_set_user_role(username, role, actor_username)
}

auth_reactivate_user <- function(username, actor_username) {
  if (!use_neon_db()) {
    return(list(ok = FALSE, error = "User management requires Neon (DATABASE_URL)."))
  }
  neon_auth_reactivate_user(username, actor_username)
}

auth_reset_user_password <- function(username, new_password, actor_username) {
  if (!use_neon_db()) {
    return(list(ok = FALSE, error = "User management requires Neon (DATABASE_URL)."))
  }
  neon_auth_reset_password(username, new_password, actor_username)
}

auth_delete_user <- function(username, actor_username) {
  if (!use_neon_db()) {
    return(list(ok = FALSE, error = "User management requires Neon (DATABASE_URL)."))
  }
  neon_auth_delete_user(username, actor_username)
}

auth_update_display_name <- function(username, display_name) {
  if (use_neon_db()) neon_auth_update_display_name(username, display_name)
  else list(ok = TRUE, display_name = display_name)
}

auth_change_password <- function(username, new_password, current_password = NULL,
                                 require_current = TRUE) {
  if (use_neon_db()) {
    neon_auth_change_password(
      username, new_password, current_password, require_current
    )
  } else {
    row <- local_auth_verify(username, current_password)
    if (is.null(row)) return(list(ok = FALSE, error = "Current password is incorrect."))
    list(ok = TRUE)
  }
}

# shinymanager credential check (user + password args).
auth_check_credentials <- function(user, password) {
  user <- auth_normalize_username(user)
  password <- as.character(password %||% "")
  if (!nzchar(user) || !nzchar(password)) {
    return(list(result = FALSE))
  }
  row <- NULL
  for (attempt in seq_len(3L)) {
    row <- tryCatch(
      auth_verify_login(user, password),
      error = function(e) {
        message("[auth] verify error (attempt ", attempt, "): ", conditionMessage(e))
        db_disconnect_all()
        NULL
      }
    )
    if (!is.null(row)) break
    if (attempt < 3L) Sys.sleep(0.2 * attempt)
  }
  if (is.null(row)) return(list(result = FALSE))
  list(
    result = TRUE,
    user_info = list(
      user  = row$username,
      admin = isTRUE(row$is_admin),
      role  = row$role,
      name  = row$display_name
    )
  )
}
