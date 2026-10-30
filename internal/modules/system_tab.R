# internal/modules/system_tab.R
# System tab — health snapshot, pipeline runner, dataset catalog, activity log.

# Human-friendly labels for upload_log `action` values.
SYS_ACTION_LABELS <- c(
  "save"                      = "Saved rows",
  "replace"                   = "Replaced file",
  "add"                       = "Added rows",
  "edit"                      = "Edited rows",
  "upload"                    = "Uploaded file",
  "archive"                   = "Archived",
  "archive_undo"              = "Restored from archive",
  "metadata_update"           = "Edited metadata",
  "dataset_register"          = "Registered dataset",
  "dataset_refresh_request"   = "Refresh requested",
  "pipeline_run"              = "Pipeline run",
  "fetch"                     = "Fetched data",
  "account"                   = "Account change",
  "restore_backup"            = "Restored from backup"
)

sys_action_category <- function(action) {
  a <- tolower(as.character(action %||% ""))
  dplyr::case_when(
    grepl("^account$", a)                             ~ "Accounts",
    grepl("pipeline|fetch", a)                       ~ "Pipeline",
    grepl("archive", a)                              ~ "Archive",
    grepl("metadata", a)                             ~ "Metadata",
    grepl("replace|upload", a)                       ~ "Upload",
    grepl("save|add|edit", a)                        ~ "Save",
    a %in% c("dataset_register")                     ~ "Catalog",
    TRUE                                             ~ "Other"
  )
}

sys_action_label <- function(action) {
  a <- as.character(action %||% "")
  out <- SYS_ACTION_LABELS[a]
  ifelse(is.na(out), a, out)
}

# Small status-stat card (used in the health strip).
sys_stat_card <- function(label, value, subtext, colour, icon_name, tip = NULL) {
  div(
    class = "sys-stat-card",
    title = tip %||% "",
    div(
      class = "sys-stat-icon",
      style = paste0("background:", colour, "22; color:", colour, ";"),
      icon(icon_name)
    ),
    div(
      class = "sys-stat-body",
      div(class = "sys-stat-value", value),
      div(class = "sys-stat-label", label),
      if (!is.null(subtext) && nzchar(subtext))
        div(class = "sys-stat-subtext", subtext)
    )
  )
}

# Info icon with native tooltip (no extra JS needed).
sys_info <- function(text) {
  tags$span(
    class = "text-muted ms-1",
    style = "cursor:help;",
    title = text,
    icon("circle-question")
  )
}

coerce_logical_col <- function(x) {
  if (is.logical(x)) return(x)
  tolower(trimws(as.character(x))) %in% c("t", "true", "1", "yes")
}

SYSTEM_USERS_NAV_VALUE <- "users_admin"

system_users_nav_panel <- function(ns) {
  nav_panel(
    title = tagList(icon("users"), " Users"),
    value = SYSTEM_USERS_NAV_VALUE,
    div(
      class = "sys-help-banner mb-3",
      icon("circle-info"),
      "Create accounts and assign roles: ",
      tags$strong("Administrator"), " (full access), ",
      tags$strong("Editor"), " (update data), ",
      tags$strong("Viewer"), " (read-only). ",
      "Deactivate blocks sign-in; delete removes the row from ",
      tags$code("app.users"),
      "."
    ),
    div(
      class = "d-flex justify-content-end mb-3",
      actionButton(
        ns("btn_add_user"),
        tagList(icon("user-plus"), " Add user"),
        class = "btn btn-sm btn-success"
      )
    ),
    uiOutput(ns("users_table"))
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────

systemUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML("
      /* System tab look */
      .sys-section-intro { color:#5a6570; font-size:0.88rem; margin-bottom:0.75rem; }
      .sys-stats-row { display:grid; grid-template-columns: repeat(4, minmax(0,1fr));
                       gap:12px; margin-bottom:16px; }
      @media (max-width: 900px){ .sys-stats-row { grid-template-columns: repeat(2, minmax(0,1fr)); } }
      .sys-stat-card { display:flex; align-items:center; gap:12px;
                       background:#ffffff; border:1px solid #e3e8e5; border-radius:10px;
                       padding:12px 14px; box-shadow:0 1px 2px rgba(0,0,0,0.03); }
      .sys-stat-icon { width:40px; height:40px; border-radius:10px;
                       display:flex; align-items:center; justify-content:center;
                       font-size:1.05rem; flex-shrink:0; }
      .sys-stat-value { font-size:1.35rem; font-weight:700; line-height:1.1; color:#1f3a28; }
      .sys-stat-label { font-size:0.78rem; color:#5a6570; text-transform:uppercase;
                        letter-spacing:0.04em; font-weight:600; }
      .sys-stat-subtext { font-size:0.75rem; color:#8a949c; margin-top:2px; }
      .sys-filter-pills .btn { padding:2px 10px; font-size:0.78rem; }
      .sys-pipeline-card { background:#f6faf7; border:1px solid #d7e6dd; }
      .sys-divider { border-top:1px dashed #d7e1da; margin:10px 0; }
      .sys-help-banner { background:#eef5f1; border:1px solid #d7e6dd; border-radius:8px;
                         padding:10px 12px; font-size:0.85rem; color:#2C6E49; }
      .sys-help-banner .fa { margin-right:6px; }
      [data-bs-theme=\"dark\"] .sys-section-intro { color:#a8b0ab; }
      [data-bs-theme=\"dark\"] .sys-stat-card {
        background:#252b28; border-color:#3d4540; box-shadow:none;
      }
      [data-bs-theme=\"dark\"] .sys-stat-value { color:#d4e8dc; }
      [data-bs-theme=\"dark\"] .sys-stat-label { color:#9aa5a0; }
      [data-bs-theme=\"dark\"] .sys-stat-subtext { color:#7a8480; }
      [data-bs-theme=\"dark\"] .sys-help-banner {
        background:#1e2a24; border-color:#3d5248; color:#8fd4a8;
      }
      [data-bs-theme=\"dark\"] .sys-pipeline-card {
        background:#1e2421; border-color:#3d4540;
      }
    "))),
    div(
      class = "mb-3",
      h4(
        class = "dashboard-page-title mb-0",
        icon("gear"), " System & Diagnostics"
      ),
      tags$p(
        class = "sys-section-intro mb-0",
        "Health snapshot, dataset catalog, and activity log. ",
        "Use this tab to check whether the dashboard's data is fresh and ",
        "see a full history of who changed what."
      )
    ),

    # ── Top health strip ──
    uiOutput(ns("health_row")),

    # ── Tabbed sections ──
    navset_card_tab(
      id = ns("sys_nav"),
      nav_panel(
        title = tagList(icon("database"), " Datasets"),
        div(
          class = "sys-help-banner mb-3",
          icon("circle-info"),
          "Every dataset the dashboard knows about. Use the filter pills to focus on ",
          "what needs attention (stale, never updated, archived).",
          " Columns: ", tags$strong("Status"), " (traffic-light), ",
          tags$strong("Last Updated"), ", and ", tags$strong("Actions"), " (restore archived)."
        ),
        div(
          class = "d-flex flex-wrap gap-2 align-items-center mb-3",
          div(
            class = "sys-filter-pills btn-group",
            role  = "group",
            `aria-label` = "Filter datasets",
            actionButton(ns("ds_filter_all"),      "All",     class = "btn btn-sm btn-primary"),
            actionButton(ns("ds_filter_active"),   "Active",  class = "btn btn-sm btn-outline-primary"),
            actionButton(ns("ds_filter_stale"),    "Stale",   class = "btn btn-sm btn-outline-primary"),
            actionButton(ns("ds_filter_never"),    "Never updated", class = "btn btn-sm btn-outline-primary"),
            actionButton(ns("ds_filter_archived"), "Archived", class = "btn btn-sm btn-outline-primary")
          ),
          tags$span(class = "flex-grow-1"),
          actionButton(ns("sync_meta"),
                       tagList(icon("rotate"), " Sync file dates"),
                       class = "btn btn-sm btn-outline-secondary",
                       title = "For datasets with no recorded last_updated, set it to the file's modification date on disk."),
          sys_info("Sync file dates: fills in missing Last Updated values from file modification times. Does not change file contents.")
        ),
        DTOutput(ns("status_table"))
      ),
      nav_panel(
        title = tagList(icon("clock-rotate-left"), " Activity log"),
        div(
          class = "sys-help-banner mb-3",
          icon("circle-info"),
          "Every save, upload, archive, metadata edit, and pipeline run is logged here ",
          "with a timestamp and user. Useful for auditing changes and debugging data issues."
        ),
        div(
          class = "d-flex flex-wrap gap-2 align-items-center mb-3",
          div(
            class = "sys-filter-pills btn-group",
            actionButton(ns("log_filter_all"),      "All",      class = "btn btn-sm btn-primary"),
            actionButton(ns("log_filter_save"),     "Saves",    class = "btn btn-sm btn-outline-primary"),
            actionButton(ns("log_filter_upload"),   "Uploads",  class = "btn btn-sm btn-outline-primary"),
            actionButton(ns("log_filter_archive"),  "Archive",  class = "btn btn-sm btn-outline-primary"),
            actionButton(ns("log_filter_metadata"), "Metadata", class = "btn btn-sm btn-outline-primary"),
            actionButton(ns("log_filter_pipeline"), "Pipeline", class = "btn btn-sm btn-outline-primary"),
            actionButton(ns("log_filter_accounts"), "Accounts", class = "btn btn-sm btn-outline-primary")
          ),
          tags$span(class = "flex-grow-1"),
          downloadButton(ns("log_export"), "Export CSV",
                           class = "btn btn-sm btn-outline-secondary"),
          tags$small(class = "text-muted ms-2",
                     icon("circle-info"), " Newest first. Timestamps in local time.")
        ),
        DTOutput(ns("upload_log_table"))
      )
    )
  )
}

# ── SERVER ────────────────────────────────────────────────────────────────────

systemServer <- function(id, meta, upload_log, meta_loading = NULL, res_auth = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    is_admin <- reactive({
      if (get_app_is_admin()) return(TRUE)
      if (is.null(res_auth)) return(FALSE)
      u <- trimws(as.character(res_auth$user %||% ""))
      if (!nzchar(u)) return(FALSE)
      auth_admin_flag(res_auth$admin)
    })

    can_edit <- reactive({
      get_app_can_edit()
    })

    users_tab_visible <- reactiveVal(FALSE)

    observe({
      admin <- is_admin()
      if (admin && !users_tab_visible()) {
        nav_insert(
          id = "sys_nav",
          nav = system_users_nav_panel(ns),
          session = session
        )
        users_tab_visible(TRUE)
      } else if (!admin && users_tab_visible()) {
        nav_remove(
          id = "sys_nav",
          target = SYSTEM_USERS_NAV_VALUE,
          session = session
        )
        users_tab_visible(FALSE)
      }
    })

    current_username <- reactive({
      u <- trimws(as.character(get_app_user() %||% ""))
      if (nzchar(u)) return(u)
      if (is.null(res_auth)) return("")
      trimws(as.character(res_auth$user %||% ""))
    })

    users_refresh <- reactiveVal(0L)

    ds_filter  <- reactiveVal("all")
    log_filter <- reactiveVal("all")

    # ── Helpers: derive per-row status fields ───────────────────────────────
    meta_with_status <- reactive({
      if (!is.null(meta_loading) && isTRUE(meta_loading())) return(NULL)
      m <- meta()
      if (is.null(m) || !nrow(m)) {
        return(data.frame())
      }
      upd    <- trimws(as.character(m$last_updated))
      no_upd <- is.na(upd) | upd == ""
      dt     <- suppressWarnings(as.Date(upd))
      stale  <- !no_upd & !is.na(dt) & (as.numeric(Sys.Date() - dt) > STALE_DAYS)
      is_arch <- if ("archived" %in% names(m))
        tolower(trimws(as.character(m$archived))) %in% c("yes", "true", "1")
      else
        rep(FALSE, nrow(m))
      status_key <- ifelse(is_arch, "archived",
                           ifelse(no_upd, "never",
                                  ifelse(stale, "stale", "current")))
      m$Status    <- c(current = "Current", stale = "Stale",
                       never = "Never updated", archived = "Archived")[status_key]
      m$StatusKey <- status_key
      m
    })

    # ── Health row ─────────────────────────────────────────────────────────
    output$health_row <- renderUI({
      if (!is.null(meta_loading) && isTRUE(meta_loading())) {
        return(loading_placeholder_ui("Loading registry..."))
      }
      m <- meta_with_status()
      if (is.null(m)) return(loading_placeholder_ui("Loading registry..."))
      if (!nrow(m)) return(div(class = "alert alert-light border small p-3",
                               icon("circle-info"), " No datasets registered yet."))
      n_total    <- nrow(m)
      n_current  <- sum(m$StatusKey == "current")
      n_stale    <- sum(m$StatusKey == "stale")
      n_never    <- sum(m$StatusKey == "never")
      n_archived <- sum(m$StatusKey == "archived")

      div(
        class = "sys-stats-row",
        sys_stat_card("Current", n_current,
                      sprintf("of %d datasets", n_total - n_archived),
                      "#2C6E49", "circle-check",
                      tip = "Updated within the last ~13 months (STALE_DAYS threshold)."),
        sys_stat_card("Stale",   n_stale,
                      if (n_stale > 0) "needs refresh" else "all fresh",
                      "#c9962b", "triangle-exclamation",
                      tip = "Last update is older than the staleness threshold. Refresh from Overview or upload new data."),
        sys_stat_card("Never updated", n_never,
                      if (n_never > 0) "awaiting first upload" else "\u2014",
                      "#b4472a", "hourglass-half",
                      tip = "Dataset is registered but has no recorded last_updated. Upload data or click Sync file dates."),
        sys_stat_card("Archived", n_archived,
                      if (n_archived > 0) "hidden from Overview" else "\u2014",
                      "#6a7078", "box-archive",
                      tip = "Archived datasets are hidden from Overview and Report Mode. You can restore them below.")
      )
    })

    # (Pipeline runner was removed — per-dataset fetchers live on Overview now.)
    if (FALSE) {
      log <- upload_log()
      empty_msg <- tags$p(class = "text-muted small mb-0",
                          icon("hourglass-half"),
                          " Pipeline has not been run from this dashboard yet.")
      if (nrow(log) == 0 || !"action" %in% names(log)) return(empty_msg)
      pipeline_rows <- log[grepl("pipeline|fetch",
                                 as.character(log$action),
                                 ignore.case = TRUE), , drop = FALSE]
      if (nrow(pipeline_rows) == 0) return(empty_msg)

      ts_parsed <- suppressWarnings(as.POSIXct(
        as.character(pipeline_rows$timestamp),
        tz = Sys.timezone(),
        tryFormats = c("%Y-%m-%d %H:%M:%S",
                       "%Y-%m-%d %H:%M",
                       "%Y-%m-%d",
                       "%m/%d/%Y %H:%M:%S",
                       "%m/%d/%Y %H:%M")
      ))
      idx <- if (any(!is.na(ts_parsed))) which.max(ts_parsed) else nrow(pipeline_rows)
      last <- pipeline_rows[idx, , drop = FALSE]

      ts_val   <- as.character(last$timestamp)
      user_val <- if ("user" %in% names(last)) as.character(last$user) else ""
      note_val <- if ("note" %in% names(last)) as.character(last$note) else ""
      note_ok  <- length(note_val) == 1L && !is.na(note_val) && nzchar(trimws(note_val))

      tagList(
        tags$p(class = "mb-1",
               icon("calendar"), " ",
               tags$strong(if (length(ts_val)) ts_val else "\u2014")),
        tags$p(class = "mb-1 small",
               icon("user"), " ",
               if (length(user_val) && nzchar(user_val)) user_val else tags$em("unknown")),
        if (note_ok)
          tags$p(class = "small text-muted mb-0", tags$em(note_val))
      )
    }  # end if (FALSE) — dead pipeline-last-run renderer

    # ── Sync metadata from files ────────────────────────────────────────────
    observeEvent(input$sync_meta, {
      req(can_edit())
      path <- here::here("data", "metadata", "datasets.csv")
      if (!file.exists(path)) {
        showNotification("datasets.csv not found.", type = "error", duration = 5)
        return()
      }
      m      <- read.csv(path, stringsAsFactors = FALSE)
      synced <- 0
      for (i in seq_len(nrow(m))) {
        fpath <- here::here("data", m$folder[i], m$dataset[i])
        if (file.exists(fpath) && !has_val(m$last_updated[i])) {
          m$last_updated[i] <- as.character(as.Date(file.info(fpath)$mtime))
          synced <- synced + 1
        }
      }
      write.csv(m, path, row.names = FALSE)
      meta(load_meta())
      showNotification(
        paste0("Synced ", synced, " dataset(s) from file modification dates."),
        type = "message", duration = 6
      )
    })

    # ── Filter pill wiring ─────────────────────────────────────────────────
    set_ds_filter <- function(v) {
      ds_filter(v)
      for (k in c("all", "active", "stale", "never", "archived")) {
        shinyjs::removeClass(id = paste0("ds_filter_", k), class = "btn-primary")
        shinyjs::addClass(   id = paste0("ds_filter_", k), class = "btn-outline-primary")
      }
      shinyjs::removeClass(id = paste0("ds_filter_", v), class = "btn-outline-primary")
      shinyjs::addClass(   id = paste0("ds_filter_", v), class = "btn-primary")
    }
    observeEvent(input$ds_filter_all,      set_ds_filter("all"))
    observeEvent(input$ds_filter_active,   set_ds_filter("active"))
    observeEvent(input$ds_filter_stale,    set_ds_filter("stale"))
    observeEvent(input$ds_filter_never,    set_ds_filter("never"))
    observeEvent(input$ds_filter_archived, set_ds_filter("archived"))

    set_log_filter <- function(v) {
      log_filter(v)
      for (k in c("all", "save", "upload", "archive", "metadata", "pipeline", "accounts")) {
        shinyjs::removeClass(id = paste0("log_filter_", k), class = "btn-primary")
        shinyjs::addClass(   id = paste0("log_filter_", k), class = "btn-outline-primary")
      }
      shinyjs::removeClass(id = paste0("log_filter_", v), class = "btn-outline-primary")
      shinyjs::addClass(   id = paste0("log_filter_", v), class = "btn-primary")
    }
    observeEvent(input$log_filter_all,      set_log_filter("all"))
    observeEvent(input$log_filter_save,     set_log_filter("save"))
    observeEvent(input$log_filter_upload,   set_log_filter("upload"))
    observeEvent(input$log_filter_archive,  set_log_filter("archive"))
    observeEvent(input$log_filter_metadata, set_log_filter("metadata"))
    observeEvent(input$log_filter_pipeline, set_log_filter("pipeline"))
    observeEvent(input$log_filter_accounts, set_log_filter("accounts"))

    output$log_export <- downloadHandler(
      filename = function() {
        paste0("activity-log-", format(Sys.time(), "%Y%m%d-%H%M"), ".csv")
      },
      content = function(file) {
        log <- upload_log()
        if (!nrow(log)) {
          write.csv(data.frame(), file, row.names = FALSE)
          return()
        }
        write.csv(log, file, row.names = FALSE)
      }
    )

    # ── Dataset catalog table ──────────────────────────────────────────────
    status_badge_html <- function(key) {
      switch(key,
             current  = '<span class="badge text-bg-success">Current</span>',
             stale    = '<span class="badge text-bg-warning">Stale</span>',
             never    = '<span class="badge text-bg-danger">Never updated</span>',
             archived = '<span class="badge text-bg-secondary">Archived</span>',
             '<span class="badge text-bg-secondary">Unknown</span>'
      )
    }

    output$status_table <- renderDT({
      if (!is.null(meta_loading) && isTRUE(meta_loading())) {
        return(datatable(data.frame(Note = "Loading..."), options = list(dom = "t"), rownames = FALSE))
      }
      m <- meta_with_status()
      if (is.null(m) || !nrow(m)) {
        return(datatable(data.frame(), options = list(dom = "t"), rownames = FALSE))
      }
      f <- ds_filter()
      if (f == "active")   m <- m[m$StatusKey %in% c("current", "stale", "never"), , drop = FALSE]
      if (f == "stale")    m <- m[m$StatusKey == "stale", , drop = FALSE]
      if (f == "never")    m <- m[m$StatusKey == "never", , drop = FALSE]
      if (f == "archived") m <- m[m$StatusKey == "archived", , drop = FALSE]

      if (!nrow(m)) {
        return(datatable(
          data.frame(Info = "No datasets match this filter."),
          rownames = FALSE, options = list(dom = "t", paging = FALSE),
          class = "table table-sm"
        ))
      }

      goal_lbl <- function(g) {
        lbl <- GOAL_LABELS[as.character(g)][[1]]
        if (is.null(lbl) || is.na(lbl)) as.character(g) else sub("^Goal [0-9]+ \u2014 ", "", lbl)
      }

      action_html <- ifelse(
        m$StatusKey == "archived",
        sprintf(
          '<button class="btn btn-sm btn-outline-success sys-restore-btn" onclick="Shiny.setInputValue(\'%s\', \'%s\', {priority:\'event\'})"><i class="fa fa-rotate-left"></i> Restore</button>',
          ns("sys_restore"), dataset_key(m)
        ),
        ""
      )

      has_val_vec <- function(x) {
        s <- as.character(x)
        !is.na(s) & nzchar(trimws(s))
      }

      df <- data.frame(
        Status          = vapply(m$StatusKey, status_badge_html, character(1)),
        Dataset         = ifelse(has_val_vec(m$display_name),
                                 as.character(m$display_name),
                                 as.character(m$dataset)),
        Goal            = vapply(m$goal, goal_lbl, character(1)),
        Fetcher         = vapply(
          as.character(m$fetcher %||% ""),
          function(h) {
            if (!nzchar(h) || is.na(h))
              return('<span class="text-muted small">\u2014</span>')
            if (exists("FETCHER_REGISTRY", inherits = TRUE) &&
                h %in% names(FETCHER_REGISTRY)) {
              sprintf(
                '<span class="badge text-bg-success" title="%s">%s</span>',
                htmltools::htmlEscape(FETCHER_REGISTRY[[h]]$label %||% h),
                htmltools::htmlEscape(h)
              )
            } else {
              sprintf(
                '<span class="badge text-bg-light border" title="Handle not registered">%s</span>',
                htmltools::htmlEscape(h)
              )
            }
          },
          character(1)
        ),
        `Last Updated`  = ifelse(has_val_vec(m$last_updated),
                                 as.character(m$last_updated),
                                 "\u2014"),
        File            = as.character(m$dataset),
        Actions         = action_html,
        check.names     = FALSE,
        stringsAsFactors = FALSE
      )

      datatable(
        df,
        rownames = FALSE,
        escape   = FALSE,
        selection = "none",
        options  = list(
          paging   = TRUE,
          pageLength = 15,
          lengthChange = FALSE,
          dom      = "ftip",
          order    = list(list(4, "desc")),
          columnDefs = list(list(className = "dt-center", targets = c(0, 3, 6))),
          language = list(search = "Search datasets:")
        ),
        class = "table table-sm table-hover"
      )
    }, server = FALSE)

    # ── Manage users (admin tab) ────────────────────────────────────────────
    users_df <- reactive({
      users_refresh()
      if (!is_admin()) return(data.frame())
      df <- tryCatch(
        auth_list_users(include_inactive = TRUE),
        error = function(e) {
          message("[users] auth_list_users failed: ", conditionMessage(e))
          data.frame()
        }
      )
      if (!nrow(df)) return(df)
      if (!"active" %in% names(df)) df$active <- TRUE
      df$is_admin <- coerce_logical_col(df$is_admin)
      df$active   <- coerce_logical_col(df$active)
      if (!"role" %in% names(df)) df$role <- ifelse(df$is_admin, "admin", "editor")
      df$role <- vapply(
        seq_len(nrow(df)),
        function(i) auth_normalize_role(df$role[[i]], is_admin = df$is_admin[[i]]),
        character(1)
      )
      df
    })

    role_btn <- function(u, want_role, label, icn, input_id, current_role) {
      active <- identical(current_role, want_role)
      tags$button(
        type = "button",
        class = if (active) "btn btn-sm btn-primary" else "btn btn-sm btn-outline-secondary",
        `data-user` = u,
        `data-role` = want_role,
        onclick = sprintf(
          "Shiny.setInputValue('%s', this.getAttribute('data-user') + '|' + this.getAttribute('data-role'), {priority: 'event'});",
          input_id
        ),
        tagList(icon(icn), " ", label)
      )
    }

    output$users_table <- renderUI({
      if (!is_admin()) {
        return(div(
          class = "text-muted small py-2",
          "Sign in as an administrator to manage users."
        ))
      }
      df <- users_df()
      if (!nrow(df)) {
        return(div(
          class = "text-muted small py-2",
          "No users in database yet."
        ))
      }
      cur <- current_username()
      deact_input_id <- ns("sys_deact_user")
      delete_input_id <- ns("sys_delete_user")
      reactivate_input_id <- ns("sys_reactivate_user")
      set_role_input_id <- ns("sys_set_role")
      reset_pwd_input_id <- ns("sys_reset_pwd")
      rows <- lapply(seq_len(nrow(df)), function(i) {
        u <- as.character(df$username[[i]])
        user_role <- auth_normalize_role(df$role[[i]], is_admin = df$is_admin[[i]])
        role_label <- auth_role_label(user_role)
        status <- if (df$active[[i]]) "Active" else "Deactivated"
        action_cell <- if (identical(u, cur)) {
          tags$span(class = "text-muted small", "(you)")
        } else {
          btns <- list()
          if (df$active[[i]]) {
            btns <- c(
              btns,
              list(
                div(
                  class = "d-flex flex-wrap gap-1 justify-content-center mb-1",
                  role_btn(u, "admin", "Admin", "user-shield", set_role_input_id, user_role),
                  role_btn(u, "editor", "Editor", "pen", set_role_input_id, user_role),
                  role_btn(u, "viewer", "Viewer", "eye", set_role_input_id, user_role)
                )
              ),
              list(tags$button(
                type = "button",
                class = "btn btn-sm btn-outline-secondary",
                `data-user` = u,
                onclick = sprintf(
                  "Shiny.setInputValue('%s', this.getAttribute('data-user'), {priority: 'event'});",
                  reset_pwd_input_id
                ),
                tagList(icon("key"), " Reset password")
              )),
              list(tags$button(
                type = "button",
                class = "btn btn-sm btn-outline-warning",
                `data-user` = u,
                onclick = sprintf(
                  "Shiny.setInputValue('%s', this.getAttribute('data-user'), {priority: 'event'});",
                  deact_input_id
                ),
                tagList(icon("user-slash"), " Deactivate")
              ))
            )
          } else {
            btns <- c(
              btns,
              list(tags$button(
                type = "button",
                class = "btn btn-sm btn-outline-success",
                `data-user` = u,
                onclick = sprintf(
                  "Shiny.setInputValue('%s', this.getAttribute('data-user'), {priority: 'event'});",
                  reactivate_input_id
                ),
                tagList(icon("user-check"), " Reactivate")
              ))
            )
          }
          btns <- c(
            btns,
            list(tags$button(
              type = "button",
              class = "btn btn-sm btn-outline-danger",
              `data-user` = u,
              onclick = sprintf(
                "Shiny.setInputValue('%s', this.getAttribute('data-user'), {priority: 'event'});",
                delete_input_id
              ),
              tagList(icon("trash"), " Delete")
            ))
          )
          div(class = "d-flex flex-wrap gap-1 justify-content-center", btns)
        }
        tags$tr(
          tags$td(u),
          tags$td(as.character(df$display_name[[i]])),
          tags$td(class = "text-center", role_label),
          tags$td(class = "text-center", status),
          tags$td(class = "text-center", action_cell)
        )
      })
      tags$table(
        class = "table table-sm table-hover",
        tags$thead(
          tags$tr(
            tags$th("Username"),
            tags$th("Name"),
            tags$th(class = "text-center", "Role"),
            tags$th(class = "text-center", "Status"),
            tags$th(class = "text-center", "Actions")
          )
        ),
        tags$tbody(rows)
      )
    })

    outputOptions(output, "users_table", suspendWhenHidden = FALSE)

    observeEvent(input$sys_nav, {
      users_refresh(users_refresh() + 1L)
    }, ignoreInit = TRUE)

    observeEvent(input$btn_add_user, {
      req(is_admin())
      showModal(modalDialog(
        title = tagList(icon("user-plus"), " Add user"),
        size = "m",
        textInput(ns("new_user_username"), "Username", width = "100%"),
        textInput(ns("new_user_display"), "Display name", width = "100%"),
        passwordInput(ns("new_user_password"), "Password", width = "100%"),
        selectInput(
          ns("new_user_role"), "Role",
          choices = c(
            "Editor (can update data)" = "editor",
            "Viewer (read-only)" = "viewer",
            "Administrator" = "admin"
          ),
          selected = "editor",
          width = "100%"
        ),
        tags$p(class = "small text-muted", "Password must be at least 8 characters."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("new_user_save"), "Create user",
                       class = "btn btn-success", icon = icon("user-plus"))
        ),
        easyClose = TRUE
      ))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    observeEvent(input$new_user_save, {
      req(is_admin())
      res <- auth_create_user(
        username     = input$new_user_username,
        password     = input$new_user_password,
        display_name = input$new_user_display,
        role         = input$new_user_role
      )
      if (!isTRUE(res$ok)) {
        showNotification(res$error %||% "Could not create user.", type = "error", duration = 8)
        return()
      }
      removeModal()
      users_refresh(users_refresh() + 1L)
      append_upload_log("", "account", note = paste("created user", input$new_user_username),
                         user = current_username())
      upload_log(load_upload_log())
      showNotification("User created.", type = "message", duration = 4)
    })

    observeEvent(input$sys_delete_user, {
      req(is_admin())
      u <- input$sys_delete_user
      req(nzchar(u))
      showModal(modalDialog(
        title = tagList(icon("trash"), " Delete user permanently?"),
        tags$p(
          "Permanently delete ", tags$strong(u), " from ",
          tags$code("app.users"), "?"
        ),
        tags$p(
          class = "text-muted small",
          "Type ", tags$strong(u), " below to confirm permanent deletion."
        ),
        textInput(ns("sys_delete_confirm_text"), "Username", width = "100%"),
        tags$p(
          class = "text-muted small mb-0",
          "This cannot be undone. Past activity log entries may still show this username."
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("sys_delete_confirm"), "Delete permanently",
                       class = "btn btn-danger", icon = icon("trash"))
        ),
        easyClose = TRUE
      ))
      session$userData$sys_pending_delete <- u
    })

    observeEvent(input$sys_delete_confirm, {
      req(is_admin())
      u <- session$userData$sys_pending_delete
      typed <- trimws(input$sys_delete_confirm_text %||% "")
      session$userData$sys_pending_delete <- NULL
      removeModal()
      req(nzchar(u))
      if (!identical(typed, u)) {
        showNotification("Type the username exactly to confirm deletion.", type = "error", duration = 6)
        return()
      }
      res <- auth_delete_user(u, current_username())
      if (!isTRUE(res$ok)) {
        showNotification(res$error %||% "Could not delete user.", type = "error", duration = 8)
        return()
      }
      users_refresh(users_refresh() + 1L)
      append_upload_log("", "account", note = paste("deleted user", u),
                         user = current_username())
      upload_log(load_upload_log())
      showNotification(paste(u, "permanently deleted."), type = "message", duration = 4)
    })

    observeEvent(input$sys_deact_user, {
      req(is_admin())
      u <- input$sys_deact_user
      req(nzchar(u))
      showModal(modalDialog(
        title = tagList(icon("user-slash"), " Deactivate user?"),
        tags$p("Deactivate ", tags$strong(u), "? They will not be able to sign in."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("sys_deact_confirm"), "Deactivate",
                       class = "btn btn-warning", icon = icon("user-slash"))
        ),
        easyClose = TRUE
      ))
      session$userData$sys_pending_deact <- u
    })

    observeEvent(input$sys_set_role, {
      req(is_admin())
      parts <- strsplit(input$sys_set_role, "|", fixed = TRUE)[[1]]
      req(length(parts) >= 2L)
      u <- parts[[1]]
      want_role <- parts[[2]]
      req(nzchar(u), nzchar(want_role))
      res <- auth_set_user_role(u, want_role, current_username())
      if (!isTRUE(res$ok)) {
        showNotification(res$error %||% "Could not update role.", type = "error", duration = 8)
        return()
      }
      users_refresh(users_refresh() + 1L)
      append_upload_log("", "account",
                         note = paste("set role", want_role, "for", u),
                         user = current_username())
      upload_log(load_upload_log())
      showNotification(paste(u, "is now", auth_role_label(want_role), "."),
                       type = "message", duration = 4)
    })

    observeEvent(input$sys_reactivate_user, {
      req(is_admin())
      u <- input$sys_reactivate_user
      req(nzchar(u))
      res <- auth_reactivate_user(u, current_username())
      if (!isTRUE(res$ok)) {
        showNotification(res$error %||% "Could not reactivate user.", type = "error", duration = 8)
        return()
      }
      users_refresh(users_refresh() + 1L)
      append_upload_log("", "account", note = paste("reactivated user", u),
                         user = current_username())
      upload_log(load_upload_log())
      showNotification(paste(u, "reactivated."), type = "message", duration = 4)
    })

    observeEvent(input$sys_reset_pwd, {
      req(is_admin())
      u <- input$sys_reset_pwd
      req(nzchar(u))
      showModal(modalDialog(
        title = tagList(icon("key"), " Reset password"),
        tags$p("Set a new password for ", tags$strong(u), "."),
        passwordInput(ns("sys_reset_pwd_new"), "New password", width = "100%"),
        tags$p(class = "small text-muted", "Minimum 8 characters."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("sys_reset_pwd_confirm"), "Set password",
                       class = "btn btn-primary", icon = icon("key"))
        ),
        easyClose = TRUE
      ))
      session$userData$sys_pending_reset_pwd <- u
    })

    observeEvent(input$sys_reset_pwd_confirm, {
      req(is_admin())
      u <- session$userData$sys_pending_reset_pwd
      session$userData$sys_pending_reset_pwd <- NULL
      removeModal()
      req(nzchar(u))
      res <- auth_reset_user_password(u, input$sys_reset_pwd_new, current_username())
      if (!isTRUE(res$ok)) {
        showNotification(res$error %||% "Could not reset password.", type = "error", duration = 8)
        return()
      }
      users_refresh(users_refresh() + 1L)
      append_upload_log("", "account", note = paste("reset password for", u),
                         user = current_username())
      upload_log(load_upload_log())
      showNotification(paste("Password updated for", u, "."), type = "message", duration = 4)
    })

    observeEvent(input$sys_deact_confirm, {
      req(is_admin())
      u <- session$userData$sys_pending_deact
      session$userData$sys_pending_deact <- NULL
      removeModal()
      req(nzchar(u))
      res <- auth_deactivate_user(u, current_username())
      if (!isTRUE(res$ok)) {
        showNotification(res$error %||% "Could not deactivate user.", type = "error", duration = 8)
        return()
      }
      users_refresh(users_refresh() + 1L)
      append_upload_log("", "account", note = paste("deactivated user", u),
                         user = current_username())
      upload_log(load_upload_log())
      showNotification(paste(u, "deactivated."), type = "message", duration = 4)
    })

    # ── Restore archived dataset ────────────────────────────────────────────
    observeEvent(input$sys_restore, {
      req(can_edit())
      ds <- input$sys_restore
      req(nzchar(ds))
      showModal(modalDialog(
        title = tagList(icon("rotate-left"), " Restore dataset?"),
        tagList(
          tags$p("Restore ", tags$strong(ds),
                 " so it re-appears in Overview and Report Mode?"),
          tags$p(class = "text-muted small mb-0",
                 "The archived snapshot in ", tags$code("data/archive/"),
                 " will not be deleted.")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("sys_restore_confirm"), "Restore",
                       class = "btn btn-success", icon = icon("rotate-left"))
        ),
        easyClose = TRUE
      ))
      session$userData$sys_pending_restore <- ds
    })

    observeEvent(input$sys_restore_confirm, {
      req(can_edit())
      ds <- session$userData$sys_pending_restore
      session$userData$sys_pending_restore <- NULL
      removeModal()
      req(!is.null(ds), nzchar(ds))
      if (!isTRUE(set_dataset_archived(ds, archived = FALSE))) {
        showNotification("Could not restore dataset.", type = "error", duration = 6)
        return()
      }
      append_upload_log(ds, "archive_undo", note = "restored from System tab")
      meta(load_meta())
      upload_log(load_upload_log())
      showNotification(paste(ds, "restored."), type = "message", duration = 4)
    })

    # ── Activity log ────────────────────────────────────────────────────────
    output$upload_log_table <- renderDT({
      log <- upload_log()
      if (!nrow(log)) {
        return(datatable(
          data.frame(Info = "No activity recorded yet."),
          rownames = FALSE, options = list(dom = "t", paging = FALSE),
          class = "table table-sm"
        ))
      }
      f <- log_filter()
      cat_col <- sys_action_category(log$action)
      keep <- switch(f,
                     all      = rep(TRUE, nrow(log)),
                     save     = cat_col == "Save",
                     upload   = cat_col == "Upload",
                     archive  = cat_col == "Archive",
                     metadata = cat_col == "Metadata",
                     pipeline = cat_col == "Pipeline",
                     accounts = cat_col == "Accounts",
                     rep(TRUE, nrow(log)))
      log <- log[keep, , drop = FALSE]
      if (!nrow(log)) {
        return(datatable(
          data.frame(Info = "No entries in this category."),
          rownames = FALSE, options = list(dom = "t", paging = FALSE),
          class = "table table-sm"
        ))
      }

      cat_col <- sys_action_category(log$action)
      cat_badge <- vapply(cat_col, function(c) {
        cls <- switch(c,
                      Accounts = "text-bg-warning",
                      Pipeline = "text-bg-success",
                      Archive  = "text-bg-secondary",
                      Metadata = "text-bg-info",
                      Upload   = "text-bg-primary",
                      Save     = "text-bg-primary",
                      Catalog  = "text-bg-dark",
                      "text-bg-light border")
        sprintf('<span class="badge %s">%s</span>', cls, c)
      }, character(1))

      df <- data.frame(
        When    = format_log_ts_local(log$timestamp),
        Action  = sys_action_label(log$action),
        Category = cat_badge,
        Dataset = as.character(log$dataset %||% ""),
        User    = as.character(log$user %||% ""),
        Note    = as.character(log$note %||% ""),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      datatable(
        df,
        rownames = FALSE,
        escape   = FALSE,
        selection = "none",
        options  = list(
          paging = TRUE,
          pageLength = 15,
          lengthChange = FALSE,
          dom = "ftip",
          order = list(list(0, "desc")),
          columnDefs = list(list(className = "dt-center", targets = 2)),
          language = list(search = "Search activity:")
        ),
        class = "table table-sm table-hover"
      )
    }, server = FALSE)

  })
}
