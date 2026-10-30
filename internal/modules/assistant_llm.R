# internal/modules/assistant_llm.R
# Phase 1 dashboard assistant: context-aware help (read-only, no actions).

GROQ_API_BASE_DEFAULT <- "https://api.groq.com/openai/v1"
OPENAI_API_BASE_DEFAULT <- "https://api.openai.com/v1"
GROQ_MODEL_DEFAULT <- "llama-3.1-8b-instant"
OPENAI_MODEL_DEFAULT <- "gpt-4o-mini"

assistant_api_config <- function() {
  groq_key <- trimws(Sys.getenv("GROQ_API_KEY"))
  openai_key <- trimws(Sys.getenv("OPENAI_API_KEY"))
  custom_base <- trimws(Sys.getenv("ASSISTANT_API_BASE_URL"))
  custom_model <- trimws(Sys.getenv("ASSISTANT_MODEL"))
  groq_model <- trimws(Sys.getenv("GROQ_MODEL"))
  openai_model <- trimws(Sys.getenv("OPENAI_API_KEY_MODEL"))

  if (nzchar(groq_key)) {
    return(list(
      key = groq_key,
      base_url = if (nzchar(custom_base)) sub("/+$", "", custom_base) else GROQ_API_BASE_DEFAULT,
      model = if (nzchar(custom_model)) {
        custom_model
      } else if (nzchar(groq_model)) {
        groq_model
      } else {
        GROQ_MODEL_DEFAULT
      },
      provider = "Groq"
    ))
  }
  if (nzchar(openai_key)) {
    return(list(
      key = openai_key,
      base_url = if (nzchar(custom_base)) sub("/+$", "", custom_base) else OPENAI_API_BASE_DEFAULT,
      model = if (nzchar(custom_model)) {
        custom_model
      } else if (nzchar(openai_model)) {
        openai_model
      } else {
        OPENAI_MODEL_DEFAULT
      },
      provider = "OpenAI"
    ))
  }
  NULL
}

assistant_use_llm <- function() {
  !is.null(assistant_api_config())
}

assistant_model <- function() {
  cfg <- assistant_api_config()
  if (is.null(cfg)) {
    if (nzchar(trimws(Sys.getenv("GROQ_API_KEY")))) return(GROQ_MODEL_DEFAULT)
    return(OPENAI_MODEL_DEFAULT)
  }
  cfg$model
}

assistant_provider_label <- function() {
  cfg <- assistant_api_config()
  if (is.null(cfg)) return(NULL)
  cfg$provider
}

assistant_tab_label <- function(tab_id) {
  tab_id <- as.character(tab_id %||% "")
  if (!nzchar(tab_id)) return("Unknown")
  if (grepl("overview", tab_id, ignore.case = TRUE)) return("Overview")
  if (grepl("explore", tab_id, ignore.case = TRUE)) return("Explore")
  if (grepl("system", tab_id, ignore.case = TRUE)) return("System")
  if (grepl("setting", tab_id, ignore.case = TRUE)) return("Settings")
  tab_id
}

assistant_warmup_state <- new.env(parent = emptyenv())
assistant_warmup_state$done <- FALSE
assistant_warmup_state$in_progress <- FALSE

assistant_warmup_ready <- function() {
  !assistant_use_llm() || isTRUE(assistant_warmup_state$done)
}

assistant_build_context <- function(meta, meta_loading, nav_dirty, tab_id,
                                    overview_ctx = list(),
                                    explorer_ctx = list()) {
  oc <- overview_ctx %||% list()
  ec <- explorer_ctx %||% list()
  role <- get_app_role()
  tab <- assistant_tab_label(tab_id)
  ctx <- list(
    app_version = APP_VERSION %||% "4.0",
    current_tab = tab,
    signed_in_username = get_app_user(),
    user_role = auth_role_label(role),
    can_edit = isTRUE(get_app_can_edit()),
    is_admin = isTRUE(get_app_is_admin()),
    unsaved_changes = isTRUE(nav_dirty),
    registry_loading = isTRUE(meta_loading)
  )
  if (!isTRUE(meta_loading) && !is.null(meta) && is.data.frame(meta) && nrow(meta)) {
    m <- meta
    active <- m
    if ("archived" %in% names(m)) {
      active <- m[!is_archived_flag(m$archived), , drop = FALSE]
    }
    ctx$total_datasets <- nrow(active)
    ctx$updated_datasets <- sum(
      !is.na(active$last_updated) & nzchar(trimws(as.character(active$last_updated)))
    )
    if (identical(tab, "Overview")) {
      g <- as.character(oc$selected_goal %||% "")
      if (nzchar(g) && "goal" %in% names(active)) {
        in_goal <- active[as.character(active$goal) == g, , drop = FALSE]
        ctx$selected_goal <- GOAL_LABELS[[g]] %||% paste("Goal", g)
        ctx$selected_goal_id <- g
        ctx$goal_dataset_count <- nrow(in_goal)
      }
    }
  }
  if (identical(tab, "Explore")) {
    g <- as.character(ec$goal %||% "all")
    ds <- as.character(ec$dataset %||% "")
    if (identical(g, "all")) {
      ctx$explore_goal <- "All goals"
    } else if (nzchar(g)) {
      ctx$explore_goal <- GOAL_LABELS[[g]] %||% paste("Goal", g)
      ctx$explore_goal_id <- g
    }
    if (nzchar(ds)) {
      ctx$explore_dataset <- ds
      if (!is.null(meta) && is.data.frame(meta) && nrow(meta)) {
        key <- ds
        m <- meta
        r <- m[paste0(as.character(m$folder), "/", as.character(m$dataset)) == key, , drop = FALSE]
        if (nrow(r) == 0L && "dataset" %in% names(m)) {
          r <- m[as.character(m$dataset) == basename(ds), , drop = FALSE]
        }
        if (nrow(r) >= 1L) {
          ctx$explore_dataset_name <- as.character(r$display_name[[1]] %||% r$dataset[[1]])
        }
      }
    }
  }
  if (isTRUE(oc$update_panel_open)) {
    ctx$update_panel_open <- TRUE
    ctx$editing_dataset <- as.character(oc$editor_dataset %||% "")
    ctx$editor_mode <- as.character(oc$editor_mode %||% "view")
    if (nzchar(ctx$editing_dataset) && !is.null(meta) && nrow(meta)) {
      r <- meta[meta$dataset == ctx$editing_dataset, , drop = FALSE]
      if (nrow(r) == 1L) {
        ctx$dataset_display_name <- as.character(r$display_name %||% ctx$editing_dataset)
        ctx$dataset_rows <- suppressWarnings(as.integer(r$row_count %||% NA))
        ctx$dataset_last_updated <- as.character(r$last_updated %||% "")
        if (use_neon_db()) {
          ctx$has_backup <- neon_has_dataset_backup(r[1, , drop = FALSE])
        }
      }
    }
  }
  ctx
}

assistant_context_summary <- function(ctx) {
  lines <- c(
    sprintf("- Tab: %s", ctx$current_tab %||% "?"),
    sprintf("- Signed in as: %s (%s)", ctx$signed_in_username %||% "?", ctx$user_role %||% "?"),
    sprintf("- Permissions: %s", if (isTRUE(ctx$is_admin)) {
      "administrator"
    } else if (isTRUE(ctx$can_edit)) {
      "editor (can update data)"
    } else {
      "viewer (read-only)"
    }),
    sprintf("- Unsaved editor changes: %s", if (isTRUE(ctx$unsaved_changes)) "yes" else "no")
  )
  if (!is.null(ctx$total_datasets)) {
    lines <- c(lines, sprintf(
      "- Registry: %d active datasets (%d with a last-updated date)",
      ctx$total_datasets %||% 0L, ctx$updated_datasets %||% 0L
    ))
  }
  if (!is.null(ctx$selected_goal)) {
    lines <- c(lines, sprintf(
      "- Overview goal selected: %s (%d datasets)",
      ctx$selected_goal, ctx$goal_dataset_count %||% 0L
    ))
  }
  if (!is.null(ctx$explore_goal)) {
    lines <- c(lines, sprintf("- Explore goal filter: %s", ctx$explore_goal))
  }
  if (!is.null(ctx$explore_dataset_name)) {
    lines <- c(lines, sprintf("- Explore dataset: %s", ctx$explore_dataset_name))
  } else if (!is.null(ctx$explore_dataset) && nzchar(ctx$explore_dataset)) {
    lines <- c(lines, sprintf("- Explore dataset key: %s", ctx$explore_dataset))
  }
  if (identical(ctx$current_tab, "Settings")) {
    lines <- c(lines, "- On Settings: personal Account + Password cards (not org user management)")
  }
  if (identical(ctx$current_tab, "System")) {
    lines <- c(lines, "- On System: Datasets / Activity log sub-tabs; Users sub-tab only if administrator")
  }
  if (isTRUE(ctx$update_panel_open)) {
    ds <- ctx$dataset_display_name %||% ctx$editing_dataset %||% "?"
    lines <- c(lines, sprintf(
      "- Update data panel open: %s (mode: %s)",
      ds, ctx$editor_mode %||% "view"
    ))
    if (!is.null(ctx$dataset_rows) && !is.na(ctx$dataset_rows)) {
      lines <- c(lines, sprintf("- Dataset rows: %d", ctx$dataset_rows))
    }
    if (isTRUE(ctx$has_backup)) {
      lines <- c(lines, "- Rolling backup available (app.dataset_backups)")
    }
  }
  paste(lines, collapse = "\n")
}

assistant_system_prompt <- function(ctx) {
  paste(
    "You are the in-app helper for the Success for Geneva (SFGC) Internal Dashboard.",
    "Phase 1: guidance ONLY — you cannot run actions, edit data, delete, or restore anything.",
    "Be concise (2–4 short paragraphs). Use exact UI labels from the reference below.",
    "CRITICAL: Answer ONLY using APP KNOWLEDGE, CANONICAL ANSWERS, and CURRENT SESSION CONTEXT. Never invent UI locations, buttons, or workflows. If something is not documented, say you are not sure rather than guessing.",
    "Use CURRENT SESSION CONTEXT for the active tab only — Overview goal vs Explore goal filter are different; do not assume Goal 1 unless context says so.",
    "Phase 1: you cannot update data, delete, save, restore, or run any dashboard actions — only explain how the user can do them in the UI.",
    "",
    assistant_canonical_answers(),
    "",
    assistant_app_knowledge(),
    "",
    "Current session context:",
    assistant_context_summary(ctx),
    sep = "\n"
  )
}

assistant_suggested_prompts <- function(ctx) {
  prompts <- character()
  if (isTRUE(ctx$update_panel_open)) {
    prompts <- c(prompts,
                 "How do I save my changes?",
                 "What does Discard changes do?",
                 "How do backups work here?")
    if (identical(ctx$editor_mode, "add")) {
      prompts <- c("How do I add a new row?", prompts)
    }
  } else if (identical(ctx$current_tab, "Overview")) {
    prompts <- c(prompts,
                 "How do I update a dataset?",
                 "How do I add a new dataset?",
                 "What goal am I looking at?")
  } else if (identical(ctx$current_tab, "Explore")) {
    prompts <- c(prompts,
                 "How do I export a chart?",
                 "What can I do on this tab?")
  } else if (identical(ctx$current_tab, "System")) {
    prompts <- c(prompts,
                 "Where is the activity log?",
                 "How do user roles work?")
    if (isTRUE(ctx$is_admin)) {
      prompts <- c("How do I reset a user's password?", prompts)
    }
  } else if (identical(ctx$current_tab, "Settings")) {
    prompts <- c(prompts,
                 "How do I change my password?",
                 "Where is my username shown?")
  } else {
    prompts <- c(prompts, "What can I do in this dashboard?")
  }
  if (!isTRUE(ctx$can_edit)) {
    prompts <- c("What can I do as a viewer?", prompts)
  }
  unique(prompts)[seq_len(min(4L, length(unique(prompts))))]
}

assistant_answer_local <- function(query, ctx) {
  q <- tolower(trimws(as.character(query %||% "")))
  if (!nzchar(q)) {
    return("Ask me how to use the dashboard — for example how to update a dataset, save changes, or use backups.")
  }

  if (grepl("role|permission|viewer|editor|admin", q)) {
    if (!isTRUE(ctx$can_edit)) {
      return(paste0(
        "You are signed in as **", ctx$user_role, "** (view only). You can browse Overview, Explore charts, and read metadata, but you cannot save data, add datasets, or restore backups. ",
        "Contact an administrator if you need edit access."
      ))
    }
    if (isTRUE(ctx$is_admin)) {
      return(paste0(
        "You are an **Administrator**. You can edit datasets, manage users and roles on the **System** tab, export the activity log, and use backup restore. ",
        "Editors can update data but cannot manage accounts."
      ))
    }
    return(paste0(
      "You are an **Editor**. You can open **Update data** on Overview cards, save changes, use backups, and add datasets when **Manage goal** is on. ",
      "User management is on **System** (admins only)."
    ))
  }

  if (grepl("backup|restore|previous version", q)) {
    return(paste(
      "Each dataset keeps **one rolling backup** in the database (`app.dataset_backups`) — the version from **before** your last save.",
      "After your first save on a dataset, the editor row shows **Previous version** with Preview (eye) and Restore.",
      "Restore replaces current saved data; your current data becomes the new backup so you can undo a restore.",
      "Use **Discard changes** for unsaved edits — that does not touch backups.",
      sep = " "
    ))
  }

  if (grepl("discard|revert|undo", q)) {
    return(paste(
      "**Discard changes** reloads the last **saved** version and drops any edits you have not saved yet.",
      "It does not restore an older backup — use **Restore** (previous version) for that.",
      if (isTRUE(ctx$unsaved_changes)) "You currently have unsaved changes — Discard or Save before leaving this dataset."
      else "",
      sep = " "
    ))
  }

  if (grepl("add.*row|new row|add data", q)) {
    if (isTRUE(ctx$update_panel_open) && identical(ctx$editor_mode, "add")) {
      return("You are in **Add data** mode. Type in the blank row at the bottom of the table, then click **Save changes**. Existing rows stay read-only in this mode.")
    }
    return(paste(
      "On **Overview**, click **Update data** on a dataset card, then choose **Add data** in the toolbar.",
      "Fill the blank row at the bottom and click **Save changes**.",
      sep = " "
    ))
  }

  if (grepl("save|saving", q)) {
    if (isTRUE(ctx$unsaved_changes)) {
      return("You have unsaved changes — click **Save changes** in the editor row. If the save fails because someone else updated the file, use **Discard changes** and try again.")
    }
    return("After editing the table, click **Save changes**. The app checks that the dataset was not changed by someone else since you opened it.")
  }

  if (grepl("update.*dataset|edit.*data|open.*dataset", q)) {
    return(paste(
      "On **Overview**, pick a goal in the left sidebar, then click **Update data** on a dataset card.",
      "Modes: **View** (read-only), **Add data**, **Edit** (change existing rows), **Replace CSV** (upload a file).",
      if (!isTRUE(ctx$can_edit)) "Your account is view-only — you will not see Update data."
      else "",
      sep = " "
    ))
  }

  if (grepl("goal|which goal", q)) {
    if (identical(ctx$current_tab, "Explore") && !is.null(ctx$explore_goal)) {
      return(sprintf("On **Explore** your goal filter is **%s**%s.",
                     ctx$explore_goal,
                     if (!is.null(ctx$explore_dataset_name)) {
                       paste0(", viewing **", ctx$explore_dataset_name, "**")
                     } else ""))
    }
    if (!is.null(ctx$selected_goal)) {
      return(sprintf("On **Overview** you are viewing **%s** (%d datasets in this goal). Switch goals using the left sidebar.", ctx$selected_goal, ctx$goal_dataset_count %||% 0L))
    }
    return("On **Overview**, use the **Goals** sidebar on the left to switch between community outcome areas.")
  }

  if (grepl("what.*(editing|dataset|working on)|current dataset", q)) {
    if (isTRUE(ctx$update_panel_open) && nzchar(ctx$editing_dataset %||% "")) {
      return(sprintf(
        "You have **Update data** open for **%s** in **%s** mode%s.",
        ctx$dataset_display_name %||% ctx$editing_dataset,
        ctx$editor_mode %||% "view",
        if (isTRUE(ctx$unsaved_changes)) " with unsaved changes" else ""
      ))
    }
    return("You do not have a dataset editor open right now. On **Overview**, click **Update data** on a card to start.")
  }

  if (grepl("explore|chart|export", q)) {
    return("**Explore** lets you filter and chart datasets. Use the filters at the top, then download CSV or PNG from the export buttons when a chart is shown.")
  }

  if (grepl("password|reset.*pwd|change.*pwd", q)) {
    if (grepl("other|someone|user|admin|reset", q) && isTRUE(ctx$is_admin)) {
      return(paste(
        "To reset **another user's** password: **System** → **Users** → **Reset password** on that user's row.",
        "To change **your own** password: click **Settings** (user icon in the navbar), enter **Current password**, **New password**, and **Confirm new password**, then click **Change password**.",
        sep = " "
      ))
    }
    return("Click **Settings** (user icon in the navbar). Enter your **Current password**, **New password**, and **Confirm new password**, then click **Change password**.")
  }

  if (grepl("username|who am i|signed in|my account|display name|settings", q)) {
    u <- ctx$signed_in_username %||% "your account"
    return(paste(
      sprintf("You are signed in as **%s** (%s).", u, ctx$user_role %||% "user"),
      "Your username is shown on the **Settings** tab (user icon in navbar) as a read-only field — it is **not** shown in the navbar or System tab header.",
      "The green status bar below the navbar shows your **role badge** (View only / Editor / Admin), not your username.",
      "Edit display name on Settings → Account → **Save display name**.",
      sep = " "
    ))
  }

  if (grepl("activity|log|user", q)) {
    if (grepl("manage|create|add user|role|deactivat|delete", q)) {
      if (!isTRUE(ctx$is_admin)) {
        return("User management is on **System → Users** (administrators only). Your role cannot create users or reset passwords for others.")
      }
      return(paste(
        "**System → Users** (admins only): **Add user**, set roles (Admin/Editor/Viewer), **Reset password**, Deactivate, Delete.",
        "Your own row shows \"(you)\" with no admin actions.",
        "**Activity log** is a separate sub-tab — filter and **Export CSV**; all roles can view it.",
        sep = " "
      ))
    }
    return("**System → Activity log**: filter by category and **Export CSV**. All roles can view it. User management is on **System → Users** (admins only).")
  }

  if (grepl("hello|hi|help|what can", q)) {
    base <- sprintf(
      "You're on the **%s** tab as **%s**.",
      ctx$current_tab %||% "dashboard", ctx$user_role %||% "user"
    )
    if (isTRUE(ctx$update_panel_open)) {
      base <- paste(base, sprintf(
        "You're editing **%s** — ask about saving, backups, or modes.",
        ctx$dataset_display_name %||% ctx$editing_dataset
      ))
    } else {
      base <- paste(base, "Ask how to update data, use backups, or what your role allows.")
    }
    return(base)
  }

  paste(
    "I can help you navigate this dashboard (read-only guidance).",
    assistant_context_summary(ctx),
    "\n\nTry asking: how to update a dataset, save changes, use backups, or what your role allows.",
    "For smarter answers, an administrator can set `GROQ_API_KEY` (free tier) in `.Renviron`.",
    sep = "\n"
  )
}

assistant_warmup <- function() {
  if (isTRUE(assistant_warmup_state$done)) return(invisible(TRUE))
  if (isTRUE(assistant_warmup_state$in_progress)) return(invisible(FALSE))
  cfg <- assistant_api_config()
  if (is.null(cfg)) return(invisible(FALSE))
  assistant_warmup_state$in_progress <- TRUE
  on.exit({ assistant_warmup_state$in_progress <- FALSE }, add = TRUE)
  msgs <- list(
    list(role = "system", content = "Reply with exactly: OK"),
    list(role = "user", content = "ping")
  )
  ok <- tryCatch({
    assistant_call_chat_api(msgs, cfg = cfg, max_tokens = 5L)
    TRUE
  }, error = function(e) {
    message("[assistant] warmup failed: ", conditionMessage(e))
    FALSE
  })
  if (isTRUE(ok)) assistant_warmup_state$done <- TRUE
  invisible(ok)
}

assistant_call_chat_api <- function(messages, cfg = assistant_api_config(), max_tokens = 700L) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required for AI answers.")
  }
  if (is.null(cfg) || !nzchar(cfg$key %||% "")) {
    stop("No assistant API key configured (set GROQ_API_KEY or OPENAI_API_KEY).")
  }
  url <- paste0(cfg$base_url, "/chat/completions")
  resp <- httr::POST(
    url = url,
    httr::add_headers(Authorization = paste("Bearer", cfg$key)),
    httr::content_type_json(),
    body = list(
      model = cfg$model,
      messages = messages,
      temperature = 0.3,
      max_tokens = as.integer(max_tokens)
    ),
    encode = "json",
    httr::timeout(45)
  )
  if (httr::status_code(resp) >= 400L) {
    err <- tryCatch(httr::content(resp, as = "parsed", type = "application/json"), error = function(e) list())
    msg <- err$error$message %||% paste("API error", httr::status_code(resp))
    stop(msg)
  }
  out <- httr::content(resp, as = "parsed", type = "application/json")
  choice <- out$choices[[1]]$message$content
  if (is.null(choice) || !nzchar(trimws(choice))) stop("Empty response from assistant API.")
  trimws(choice)
}

assistant_canonical_reply <- function(query, ctx) {
  q <- tolower(trimws(as.character(query %||% "")))
  if (!nzchar(q)) return(NULL)
  if (grepl("password|change.*pwd", q) && !grepl("other|someone|another|reset", q)) {
    return("Click **Settings** (user icon in the navbar). Enter your **Current password**, **New password**, and **Confirm new password**, then click **Change password**.")
  }
  if (grepl("where.*username|username.*shown|my username", q)) {
    return("Your username is on the **Settings** tab (user icon in the navbar) in the read-only Username field. It is not shown in the navbar or System tab.")
  }
  NULL
}

assistant_answer <- function(query, ctx, history = list()) {
  canonical <- assistant_canonical_reply(query, ctx)
  if (!is.null(canonical)) {
    return(list(text = canonical, source = "canonical"))
  }
  cfg <- assistant_api_config()
  if (!is.null(cfg)) {
    msgs <- c(
      list(list(role = "system", content = assistant_system_prompt(ctx))),
      history,
      list(list(role = "user", content = as.character(query)))
    )
    ai_res <- tryCatch(
      list(ok = TRUE, text = assistant_call_chat_api(msgs, cfg)),
      error = function(e) {
        message("[assistant] ", cfg$provider, " error: ", conditionMessage(e))
        list(ok = FALSE, error = conditionMessage(e))
      }
    )
    if (isTRUE(ai_res$ok) && nzchar(trimws(ai_res$text %||% ""))) {
      return(list(text = ai_res$text, source = tolower(cfg$provider)))
    }
    local <- assistant_answer_local(query, ctx)
    err <- ai_res$error %||% "empty response"
    return(list(
      text = paste0(
        local,
        "\n\n_(", cfg$provider, " could not answer: ", err,
        ". Built-in help shown — check `GROQ_API_KEY` in `.Renviron` or retry.)_"
      ),
      source = "local_fallback"
    ))
  }
  list(text = assistant_answer_local(query, ctx), source = "local")
}
