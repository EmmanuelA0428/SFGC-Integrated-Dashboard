# internal/modules/overview_tab.R
# Overview: goal nav, manage-mode (archive overlays + add card), Update data panel.

# Replace CSV modal: show full uploaded table in a scrollable preview; cap only avoids huge HTML tables.
REPLACE_PREVIEW_MAX_ROWS <- 10000L

overviewUI <- function(id) {
  ns <- NS(id)
  div(
    class = "overview-sidebar-root",
    layout_sidebar(
      fillable = FALSE,
      sidebar  = sidebar(
        width  = 240,
        title  = "Goals",
        class  = "overview-goals-sidebar",
        uiOutput(ns("goal_nav"))
      ),
      div(
        id = ns("overview_main_col"),
        class = "overview-main-col",
        uiOutput(ns("overview_boot")),
        uiOutput(ns("overview_header")),
        uiOutput(ns("overview_cards"))
      )
    )
  )
}

overviewServer <- function(id, meta, upload_log, nav_dirty = NULL, meta_loading = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    selected_goal   <- reactiveVal("1")
    editor_ctx      <- reactiveVal(NULL)
    ed_dirty        <- reactiveVal(FALSE)
    ed_reload       <- reactiveVal(0L)
    details_reload  <- reactiveVal(0L)
    manage_edit_mode <- reactiveVal(FALSE)
    add_wizard_open <- reactiveVal(FALSE)
    backup_reload    <- reactiveVal(0L)
    backup_preview_path <- reactiveVal(NULL)
    backup_restore_pending <- reactiveVal(NULL)

    can_edit <- reactive({
      get_app_can_edit()
    })

    session$userData$overview_context_rv <- reactiveVal(list(
      selected_goal = "1",
      update_panel_open = FALSE,
      editor_dataset = NA_character_,
      editor_mode = NA_character_,
      dirty = FALSE
    ))

    observe({
      ed <- editor_ctx()
      session$userData$overview_context_rv(list(
        selected_goal = selected_goal(),
        update_panel_open = !is.null(ed) && !is.null(ed$dataset),
        editor_dataset = if (!is.null(ed)) ed$dataset else NA_character_,
        editor_mode = if (!is.null(ed)) ed$mode %||% "view" else NA_character_,
        dirty = isTRUE(ed_dirty())
      ))
    })
    archive_pending_ds   <- reactiveVal(NULL)
    last_archived_undo_ds <- reactiveVal(NULL)
    
    refresh_meta <- function() {
      meta(load_meta())
      upload_log(load_upload_log())
    }
    
    ctx_for_editor <- reactive({
      ed <- editor_ctx()
      if (is.null(ed) || is.null(ed$dataset)) {
        return(list(dataset = NA_character_, mode = "view"))
      }
      if (identical(ed$mode, "replace")) {
        return(list(dataset = NA_character_, mode = "view"))
      }
      list(dataset = ed$dataset, mode = ed$mode %||% "view")
    })
    
    backup_for_editor <- reactive({
      backup_reload()
      ed_reload()
      ed <- editor_ctx()
      if (is.null(ed) || is.null(ed$dataset) || !use_neon_db()) return(NULL)
      m <- meta()
      r <- m[dataset_key(m) == ed$dataset, , drop = FALSE]
      if (nrow(r) != 1L) return(NULL)
      backups <- list_dataset_backups(r[1, , drop = FALSE])
      if (!length(backups)) return(NULL)
      summary <- backup_summary_from_ref(r[1, , drop = FALSE], backups[[1]])
      if (is.null(summary)) return(NULL)
      list(
        dataset = ed$dataset,
        ref = backups[[1]],
        summary = summary,
        preview_input_id = ns("preview_backup"),
        restore_input_id = ns("restore_backup")
      )
    })

    datasetEditorServer(
      "ded", ctx_for_editor, meta, ed_dirty, ed_reload, refresh_meta,
      backup_info = backup_for_editor
    )
    
    observe({
      if (!is.null(nav_dirty)) nav_dirty(ed_dirty())
    })

    # Register download handlers only for the active goal (not all ~50 datasets).
    observe({
      if (!is.null(meta_loading) && isTRUE(meta_loading())) return()
      m <- meta()
      if (is.null(m)) return()
      m <- active_meta(m)
      g <- selected_goal()
      rows <- m[as.character(m$goal) == g, , drop = FALSE]
      if (nrow(rows) == 0) return()
      lapply(seq_len(nrow(rows)), function(i) {
        local({
          r    <- rows[i, ]
          dlid <- paste0("dl_", gsub("[^A-Za-z0-9]", "_", r$dataset))
          output[[dlid]] <- downloadHandler(
            filename = function() r$dataset,
            content  = function(file) {
              df <- load_dataset(r)
              if (!is.null(df))
                write.csv(df, file, row.names = FALSE)
              else
                write.csv(data.frame(note = "No data available"), file, row.names = FALSE)
            }
          )
        })
      })
    })
    
    output$goal_nav <- renderUI({
      if (!is.null(meta_loading) && isTRUE(meta_loading())) {
        return(loading_placeholder_ui("Loading goals..."))
      }
      m <- meta()
      if (is.null(m)) return(loading_placeholder_ui("Loading goals..."))
      m <- active_meta(m)
      tagList(lapply(names(GOAL_LABELS), function(g) {
        g_chr   <- as.character(m$goal)
        in_goal <- g_chr == g & !is.na(g_chr)
        n_ds    <- sum(in_goal, na.rm = TRUE)
        upd     <- !is.na(m$last_updated) & nzchar(trimws(as.character(m$last_updated)))
        n_done  <- sum(in_goal & upd, na.rm = TRUE)
        is_sel  <- selected_goal() == g
        pct     <- if (n_ds > 0) round(n_done / n_ds * 100) else 0
        bar_col <- if (pct == 100) "#2C6E49" else if (pct > 0) "#f0ad4e" else "#dee2e6"
        div(
          class = paste("goal-nav-item", if (is_sel) "selected" else ""),
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s', {priority:'event'})",
            ns("goal_click"), g
          ),
          div(class = "goal-nav-label", GOAL_LABELS[[g]]),
          div(
            class = "goal-nav-progress-track",
            div(
              class = "goal-nav-progress-bar",
              style = paste0("width:", pct, "%; background:", bar_col, ";")
            )
          ),
          tags$small(class = "goal-nav-meta", sprintf("%d / %d datasets", n_done, n_ds))
        )
      }))
    })
    
    observeEvent(input$goal_click, { selected_goal(input$goal_click) })
    
    observeEvent(input$btn_manage_goal, {
      if (!isTRUE(can_edit())) {
        showNotification("View-only accounts cannot manage datasets.", type = "warning", duration = 4)
        return()
      }
      manage_edit_mode(!manage_edit_mode())
    })

    observeEvent(manage_edit_mode(), {
      mm <- manage_edit_mode()
      updateActionButton(
        session, "btn_manage_goal",
        label = if (mm) "Done managing" else "Manage goal",
        icon  = if (mm) icon("check") else icon("sliders")
      )
      shinyjs::toggleClass(
        selector = paste0("#", ns("overview_main_col")),
        class    = "overview-manage-mode",
        condition = mm
      )
    }, ignoreInit = TRUE)

    output$overview_boot <- renderUI({
      if (!is.null(meta_loading) && isTRUE(meta_loading())) {
        return(loading_placeholder_ui("Loading dataset registry..."))
      }
      NULL
    })

    output$overview_header <- renderUI({
      if (!is.null(meta_loading) && isTRUE(meta_loading())) return(NULL)
      req(meta())
      g <- selected_goal()
      div(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2 mb-3",
        h4(GOAL_LABELS[[g]], class = "mb-0 dashboard-page-title"),
        if (isTRUE(can_edit())) {
          actionButton(
            ns("btn_manage_goal"),
            "Manage goal",
            class = "btn btn-sm btn-outline-secondary fw-semibold",
            icon  = icon("sliders")
          )
        } else {
          tags$span(class = "badge text-bg-secondary", "View only")
        }
      )
    })

    # Cards depend on goal + meta only — manage mode toggles via CSS (no full rebuild).
    output$overview_cards <- renderUI({
      if (!is.null(meta_loading) && isTRUE(meta_loading())) return(NULL)
      tryCatch({
        req(meta())
        g    <- selected_goal()
        m    <- active_meta(meta())
        rows <- m[as.character(m$goal) == g, , drop = FALSE]

        if (nrow(rows) == 0) {
          return(tagList(
            div(
              class = "overview-add-card overview-manage-only mb-3",
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority:'event'})",
                ns("open_add_wizard"), g
              ),
              div(class = "text-center text-muted",
                  icon("plus", class = "fa-2x mb-2"),
                  tags$div("Add dataset"))
            ),
            p(class = "overview-manage-hint text-muted",
              "No active datasets for this goal. Turn on Manage goal to add one.")
          ))
        }

        card_list <- lapply(seq_len(nrow(rows)), function(i) {
          r        <- rows[i, ]
          yr_range <- meta_year_range(r)
          n_rows   <- suppressWarnings(as.integer(r$row_count %||% NA))
          has_data <- meta_has_data(r)
          dlid     <- paste0("dl_", gsub("[^A-Za-z0-9]", "_", r$dataset))
          safe_ds  <- gsub("[^A-Za-z0-9]", "_", r$dataset)
          
          can_fetch <- has_fetcher(r) && isTRUE(can_edit())

          update_btn <- if (isTRUE(can_edit())) {
            actionButton(
              ns(paste0("upd_", safe_ds)),
              label = tagList(icon("pen-to-square", class = "me-1"), "Update data"),
              class = "btn btn-sm btn-primary overview-action-btn overview-update-btn",
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority:'event'})",
                ns("open_update_panel"), dataset_key(r)
              )
            )
          } else {
            NULL
          }

          core_card <- card(
            card_header(
              class = "d-flex justify-content-between align-items-start gap-2 py-2",
              tags$span(class = "overview-card-title", r$display_name),
              if (can_fetch)
                actionButton(
                  ns(paste0("rf_", safe_ds)),
                  label = NULL,
                  icon  = icon("rotate"),
                  class = "btn btn-link btn-sm p-0 border-0 shadow-none flex-shrink-0 overview-refresh-btn",
                  title = "Fetch latest data from source",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', '%s', {priority:'event'})",
                    ns("refresh_dataset"), dataset_key(r)
                  )
                )
            ),
            card_body(
              class = "overview-card-body py-2",
              div(
                class = "d-flex flex-wrap align-items-center gap-2 mb-1",
                freshness_badge(r$last_updated)
              ),
              if (!is.null(yr_range))
                tags$p(class = "small mb-1",
                       icon("calendar"), " ", tags$strong(yr_range))
              else if (!is.na(n_rows))
                tags$p(class = "small mb-1",
                       icon("table"), " ", tags$strong(n_rows), " rows")
              else if (!has_data)
                tags$p(class = "small text-danger mb-1",
                       icon("triangle-exclamation"), " No data file found"),
              div(
                class = "small overview-card-sources mb-1",
                render_source_line(r, variant = "compact")
              ),
              div(
                class = "d-flex flex-wrap gap-2 align-items-center pt-1 mt-1 border-top",
                update_btn,
                actionButton(
                  ns(paste0("how_", safe_ds)),
                  label = NULL,
                  icon  = icon("circle-info"),
                  class = "btn btn-sm btn-outline-secondary overview-icon-btn",
                  title = "How to collect / data info",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', '%s', {priority:'event'})",
                    ns("show_how_to_collect"), dataset_key(r)
                  )
                ),
                if (has_data)
                  downloadButton(
                    ns(dlid),
                    label = "",
                    class = "btn btn-sm btn-outline-secondary overview-icon-btn",
                    title = "Download CSV"
                  )
              )
            ),
            class = "overview-dataset-card",
            style = "margin-bottom:12px;"
          )

          div(
            class = "overview-card-wrap",
            actionButton(
              ns(paste0("arch_", safe_ds)),
              label = NULL,
              icon  = icon("box-archive"),
              class = "btn btn-sm btn-warning overview-archive-btn",
              title = "Archive dataset",
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority:'event'})",
                ns("archive_dataset"), dataset_key(r)
              )
            ),
            core_card
          )
        })

        add_card <- div(
          class = "overview-add-card overview-manage-only mb-3",
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s', {priority:'event'})",
            ns("open_add_wizard"), g
          ),
          div(class = "text-center text-muted",
              icon("plus", class = "fa-2x mb-2"),
              tags$div(style = "font-weight:600;", "Add dataset"),
              tags$small("New CSV for this goal"))
        )
        card_list <- c(card_list, list(add_card))

        layout_columns(
          col_widths = 4,
          fillable   = FALSE,
          !!!card_list
        )
      }, error = function(e) {
        div(
          class = "alert alert-danger",
          icon("triangle-exclamation"), " Overview could not render: ",
          tags$pre(class = "small mb-0 mt-2", conditionMessage(e))
        )
      })
    })
    
    observeEvent(input$archive_dataset, {
      req(can_edit())
      ds <- input$archive_dataset
      req(nzchar(ds))
      archive_pending_ds(ds)
      m <- active_meta(meta())
      r <- m[dataset_key(m) == ds, , drop = FALSE]
      disp <- if (nrow(r) == 1) (r$display_name[1] %||% ds) else ds
      showModal(modalDialog(
        title = tagList(icon("box-archive"), " Archive this dataset?"),
        tagList(
          tags$p(
            tags$strong(disp),
            tags$br(),
            tags$span(class = "text-muted small",
                      "It will be hidden from Overview and Report Mode. You can restore it from the System tab later.")
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("archive_confirm"), "Archive",
                       class = "btn btn-warning", icon = icon("box-archive"))
        ),
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$archive_confirm, {
      req(can_edit())
      ds <- archive_pending_ds()
      req(nzchar(ds))
      removeModal()
      archive_pending_ds(NULL)
      if (!isTRUE(set_dataset_archived(ds, archived = TRUE))) {
        showNotification("Could not update metadata.", type = "error", duration = 6)
        return()
      }
      refresh_meta()
      append_upload_log(ds, "archive", note = "overview archive")
      upload_log(load_upload_log())
      last_archived_undo_ds(ds)
      showModal(modalDialog(
        title = tagList(icon("circle-check"), " Archived"),
        tags$p("Archived: ", tags$code(ds)),
        footer = tagList(
          actionButton(ns("archive_undo"), "Undo",
                       class = "btn btn-primary", icon = icon("rotate-left")),
          modalButton("Close")
        ),
        easyClose = FALSE
      ))
    })
    
    observeEvent(input$archive_undo, {
      req(can_edit())
      ds <- last_archived_undo_ds()
      req(nzchar(ds))
      removeModal()
      last_archived_undo_ds(NULL)
      if (!isTRUE(set_dataset_archived(ds, archived = FALSE))) {
        showNotification("Could not restore dataset.", type = "error", duration = 6)
        return()
      }
      refresh_meta()
      append_upload_log(ds, "archive_undo", note = "restored from overview")
      upload_log(load_upload_log())
      showNotification(paste(ds, "restored (unarchived)."), type = "message", duration = 5)
    })
    
    observeEvent(input$open_add_wizard, {
      req(can_edit())
      add_wizard_open(TRUE)
      showModal(modalDialog(
        title = tagList(icon("plus"), " Add dataset \u2014 ", GOAL_LABELS[[selected_goal()]]),
        size  = "l",
        uiOutput(ns("add_wizard_body")),
        footer = tagList(modalButton("Cancel")),
        easyClose = TRUE
      ))
    })
    
    output$add_wizard_body <- renderUI({
      req(add_wizard_open())
      tagList(
        p(class = "small text-muted",
          "Create an empty dataset (with optional custom headers) or upload a CSV now."),
        tags$h6(class = "text-uppercase text-muted small mt-2 mb-2",
                "Basics"),
        layout_columns(
          col_widths = c(6, 6), fillable = FALSE,
          textInput(ns("new_display"), "Display name", width = "100%"),
          textInput(ns("new_file"), "CSV filename", width = "100%",
                    placeholder = "MyData.csv")
        ),
        tags$h6(class = "text-uppercase text-muted small mt-3 mb-2",
                "Source"),
        selectInput(
          ns("new_source_type"), "Source type",
          choices = c(
            "Public portal (link or downloadable dataset)"  = "public",
            "Contact (provided by a person or partner org)" = "contact",
            "Both public and contact"                       = "hybrid"
          ),
          selected = "public",
          width = "100%"
        ),
        textInput(ns("new_source_name"), "Source / partner name",
                  placeholder = "e.g. NYS Department of Health or GCSD",
                  width = "100%"),
        conditionalPanel(
          condition = sprintf(
            "input['%s'] == 'public' || input['%s'] == 'hybrid'",
            ns("new_source_type"), ns("new_source_type")
          ),
          textInput(ns("new_source_url"), "Primary source URL",
                    placeholder = "https://...", width = "100%")
        ),
        conditionalPanel(
          condition = sprintf(
            "input['%s'] == 'contact' || input['%s'] == 'hybrid'",
            ns("new_source_type"), ns("new_source_type")
          ),
          layout_columns(
            col_widths = c(6, 6), fillable = FALSE,
            textInput(ns("new_contact_name"), "Contact name",
                      placeholder = "Person or org", width = "100%"),
            textInput(ns("new_contact_email"), "Contact email",
                      placeholder = "name@example.org", width = "100%")
          )
        ),
        tags$p(
          class = "small text-muted mb-2",
          icon("circle-info"), " Optional: add a ", tags$code("Source"),
          " column in the CSV for row-level URLs or labels; the dashboard lists distinct values under each dataset."
        ),
        textAreaInput(ns("new_how"), "How to collect",
                      placeholder = "Short instructions for future updates (where to download, who to email, etc.)",
                      rows = 2, width = "100%"),
        tags$h6(class = "text-uppercase text-muted small mt-3 mb-2",
                "File"),
        checkboxInput(ns("new_upload_csv"), "Create from uploaded CSV", FALSE),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("new_upload_csv")),
          fileInput(ns("new_csv_file"), "Upload CSV", accept = ".csv", width = "100%")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] != true", ns("new_upload_csv")),
          checkboxInput(ns("new_blank"), "Define custom headers for empty file", FALSE),
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("new_blank")),
            textInput(ns("new_headers"), "Column names (comma-separated)",
                      value = "Year,Community,Value", width = "100%")
          )
        ),
        actionButton(ns("new_save"), "Save new dataset",
                     class = "btn btn-success mt-3", icon = icon("floppy-disk"))
      )
    })
    
    observeEvent(input$new_save, {
      g <- selected_goal()
      r <- NULL
      # Only surface URL / contact fields that apply to the chosen source type
      stype <- input$new_source_type %||% "public"
      src_url   <- if (stype %in% c("public", "hybrid")) input$new_source_url   %||% "" else ""
      ctc_name  <- if (stype %in% c("contact", "hybrid")) input$new_contact_name  %||% "" else ""
      ctc_email <- if (stype %in% c("contact", "hybrid")) input$new_contact_email %||% "" else ""

      r <- tryCatch({
        if (isTRUE(input$new_upload_csv)) {
          if (is.null(input$new_csv_file) || !nzchar(input$new_csv_file$datapath %||% "")) {
            return(list(ok = FALSE, error = "Upload a CSV file first."))
          }
          up <- tryCatch(
            read.csv(input$new_csv_file$datapath, stringsAsFactors = FALSE),
            error = function(e) NULL
          )
          if (is.null(up) || ncol(up) == 0) {
            return(list(ok = FALSE, error = "Could not read uploaded CSV."))
          }
          create_dataset_from_uploaded_csv(
            goal          = g,
            display_name  = input$new_display,
            file_name     = input$new_file,
            source_type   = stype,
            source_name   = input$new_source_name,
            source_url    = src_url,
            contact_name  = ctc_name,
            contact_email = ctc_email,
            how           = input$new_how,
            df            = up
          )
        } else {
          create_dataset_from_wizard(
            goal               = g,
            display_name       = input$new_display,
            file_name          = input$new_file,
            source_type        = stype,
            source_name        = input$new_source_name,
            source_url         = src_url,
            contact_name       = ctc_name,
            contact_email      = ctc_email,
            how                = input$new_how,
            use_custom_headers = isTRUE(input$new_blank),
            headers_csv        = input$new_headers %||% ""
          )
        }
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))

      if (is.null(r) || !isTRUE(r$ok)) {
        showNotification(r$error %||% "Could not create dataset",
                         type = "error", duration = 8)
        return()
      }
      refresh_meta()
      showNotification(paste("Registered", basename(r$path)),
                       type = "message", duration = 5)
      removeModal()
      add_wizard_open(FALSE)
    })
    
    observeEvent(input$show_how_to_collect, {
      ds <- input$show_how_to_collect
      req(nzchar(ds))
      m <- meta()
      r <- m[dataset_key(m) == ds, , drop = FALSE]
      req(nrow(r) == 1L)
      r <- r[1, , drop = FALSE]
      how_txt <- if (has_val(r$how_to_collect))
        tags$p(class = "mb-0", r$how_to_collect)
      else
        tags$p(class = "text-muted mb-0 fst-italic",
               "No collection instructions recorded yet. Add them from ",
               tags$strong("Update data \u2192 Dataset details"), ".")

      source_block <- render_source_line(r, variant = "detailed")
      contact_block <- NULL

      fetchable_badge <- if (has_fetcher(r))
        tags$span(class = "badge text-bg-success ms-2",
                  title = "A registered fetcher can pull latest data for this dataset",
                  icon("bolt"), " Fetchable")
      else NULL

      showModal(modalDialog(
        title = tagList(
          icon("clipboard-list"), " How to collect",
          tags$span(class = "text-muted ms-2 small", r$display_name %||% ds),
          fetchable_badge
        ),
        size = "m",
        tagList(
          tags$div(
            class = "mb-3",
            tags$p(class = "small text-muted mb-1",
                   icon("list-check"), " Collection steps"),
            how_txt
          ),
          tags$div(
            class = "mb-0",
            tags$p(class = "small text-muted mb-1",
                   icon("address-card"), " Source & contact"),
            source_block
          )
        ),
        footer = tagList(
          actionButton(
            ns("how_to_open_edit"),
            tagList(icon("pen-to-square", class = "me-1"), "Edit in Update data"),
            class = "btn btn-sm btn-primary fw-semibold overview-action-btn",
            onclick = sprintf(
              "Shiny.setInputValue('%s', '%s', {priority:'event'})",
              ns("open_update_from_how"), ds
            )
          ),
          modalButton("Close")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$open_update_from_how, {
      req(can_edit())
      ds <- input$open_update_from_how
      req(nzchar(ds))
      removeModal()
      if (ed_dirty()) {
        showModal(modalDialog(
          title = "Unsaved changes",
          "Save or discard changes before opening another dataset.",
          footer = modalButton("OK")
        ))
        return()
      }
      editor_ctx(list(dataset = ds, mode = "view"))
      showModal(modalDialog(
        title = tagList(icon("table"), " ", ds),
        size  = "xl",
        class = "update-data-modal",
        div(
          class = "update-data-panel",
          uiOutput(ns("update_toolbar")),
          uiOutput(ns("update_body"))
        ),
        footer = tagList(
          actionButton(ns("close_update_modal"), "Close", class = "btn-secondary")
        ),
        easyClose = FALSE
      ))
    })

    # ── Refresh (fetch-and-stage) workflow ────────────────────────────────
    # fetch_state() carries everything needed by the staging modal:
    #   dataset     character(1) — dataset filename
    #   row         data.frame    — metadata row for the dataset
    #   handle      character(1) — fetcher registry handle
    #   fetcher     list          — registry entry
    #   latest      integer       — latest year the fetcher can produce
    #   existing    data.frame    — current CSV contents
    #   fetched     data.frame    — rows returned by the last dry-run (may be NULL)
    #   diff        list          — fetcher_dry_run() output (may be NULL)
    fetch_state <- reactiveVal(NULL)

    fetch_years_selected <- function() {
      st <- fetch_state()
      if (is.null(st)) return(integer(0))
      start <- suppressWarnings(as.integer(input$refresh_year_start))
      end   <- suppressWarnings(as.integer(input$refresh_year_end))
      if (is.na(start)) start <- st$latest
      if (is.na(end))   end   <- st$latest
      if (end < start) { tmp <- start; start <- end; end <- tmp }
      ys <- seq.int(start, end)
      if (isTRUE(input$refresh_gap_only)) {
        have <- if (!is.null(st$existing) &&
                    !is.null(st$existing[[st$fetcher$year_column %||% "Year"]]))
          suppressWarnings(as.integer(
            st$existing[[st$fetcher$year_column %||% "Year"]]))
        else integer(0)
        ys <- setdiff(ys, have[!is.na(have)])
      }
      ys
    }

    render_refresh_modal <- function(message = NULL, message_type = "info") {
      st <- fetch_state()
      if (is.null(st)) return(invisible())

      existing_years <- suppressWarnings(as.integer(
        st$existing[[st$fetcher$year_column %||% "Year"]]))
      existing_years <- existing_years[!is.na(existing_years)]
      have_latest <- if (length(existing_years)) max(existing_years) else NA_integer_
      default_year <- if (!is.na(st$latest)) st$latest
                      else if (!is.na(have_latest)) have_latest + 1L
                      else as.integer(format(Sys.Date(), "%Y")) - 2L

      info_rows <- tagList(
        tags$p(class = "small mb-2 text-muted",
               "Fetch runs against the live source and shows a preview. Nothing is written until you click Append or Overwrite."),
        tags$div(
          class = "d-flex flex-wrap gap-3 small mb-3",
          tags$div(icon("database"), " ", tags$strong("Source: "),
                   st$fetcher$source_name %||% "—"),
          tags$div(icon("calendar-check"), " ", tags$strong("Already loaded: "),
                   if (length(existing_years))
                     sprintf("%d–%d (%d yrs)",
                             min(existing_years), max(existing_years),
                             length(unique(existing_years)))
                   else "none"),
          tags$div(icon("cloud-arrow-down"), " ", tags$strong("Latest available: "),
                   if (!is.na(st$latest)) as.character(st$latest) else "—")
        )
      )

      year_inputs <- fluidRow(
        column(4,
               numericInput(ns("refresh_year_start"), "From year",
                            value = default_year, step = 1,
                            min = 2000, max = default_year + 5)),
        column(4,
               numericInput(ns("refresh_year_end"), "To year",
                            value = default_year, step = 1,
                            min = 2000, max = default_year + 5)),
        column(4,
               tags$label(class = "small fw-semibold", "Options"),
               checkboxInput(ns("refresh_gap_only"),
                             "Only fetch years not already in file",
                             value = FALSE))
      )

      diff_block <- if (!is.null(st$diff) && isTRUE(st$diff$ok)) {
        new_n  <- nrow(st$diff$new_rows)
        conf_n <- nrow(st$diff$conflict_rows)
        same_n <- nrow(st$diff$identical_rows)

        summary_strip <- tags$div(
          class = "d-flex flex-wrap gap-2 mb-2 small",
          tags$span(class = "badge bg-success",
                    sprintf("%d new row%s", new_n, if (new_n == 1) "" else "s")),
          tags$span(class = if (conf_n > 0) "badge bg-warning text-dark" else "badge bg-light text-muted",
                    sprintf("%d conflict%s", conf_n, if (conf_n == 1) "" else "s")),
          tags$span(class = "badge bg-light text-muted",
                    sprintf("%d unchanged", same_n))
        )

        preview_df <- rbind(
          if (new_n)  cbind(Status = "New",        st$diff$new_rows)       else NULL,
          if (conf_n) cbind(Status = "Conflict",   st$diff$conflict_rows)  else NULL,
          if (same_n) cbind(Status = "Identical",  st$diff$identical_rows) else NULL
        )
        if (is.null(preview_df) || !nrow(preview_df))
          preview_df <- data.frame(Status = "No rows fetched")

        tagList(
          tags$hr(),
          tags$h6("Preview"),
          summary_strip,
          div(style = "max-height:260px; overflow:auto;",
              renderTable_html(preview_df))
        )
      } else if (!is.null(st$diff) && isFALSE(st$diff$ok)) {
        tagList(
          tags$hr(),
          tags$div(class = "alert alert-danger small mb-0",
                   icon("triangle-exclamation"), " ",
                   st$diff$error %||% "Fetch failed.")
        )
      } else NULL

      msg_block <- if (!is.null(message))
        tags$div(
          class = paste0("alert alert-", message_type, " small py-2 mb-2"),
          message
        )

      showModal(modalDialog(
        title = tagList(icon("rotate"),
                        sprintf(" Fetch latest · %s",
                                st$row$display_name %||% st$dataset)),
        size  = "l",
        easyClose = FALSE,
        msg_block,
        info_rows,
        year_inputs,
        tags$div(
          class = "d-flex gap-2 mt-2",
          actionButton(ns("refresh_fetch"), tagList(icon("cloud-arrow-down"), " Fetch preview"),
                       class = "btn btn-primary btn-sm"),
          if (!is.null(st$diff) && isTRUE(st$diff$ok))
            tags$span(class = "small text-muted align-self-center",
                      sprintf("Preview for years: %s",
                              paste(sort(unique(
                                suppressWarnings(as.integer(
                                  st$diff$fetched[[st$fetcher$year_column %||% "Year"]]))
                              )), collapse = ", ")))
        ),
        diff_block,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("refresh_apply_append"),
                       tagList(icon("plus"), " Append new rows"),
                       class = "btn btn-outline-success",
                       disabled = if (is.null(st$diff) ||
                                      !isTRUE(st$diff$ok) ||
                                      !nrow(st$diff$new_rows)) "disabled" else NULL),
          actionButton(ns("refresh_apply_overwrite"),
                       tagList(icon("arrows-rotate"), " Overwrite fetched years"),
                       class = "btn btn-warning",
                       disabled = if (is.null(st$diff) ||
                                      !isTRUE(st$diff$ok) ||
                                      !nrow(st$diff$fetched)) "disabled" else NULL)
        )
      ))
    }

    # Tiny HTML table helper so we don't need a new output binding inside the modal
    renderTable_html <- function(df) {
      if (is.null(df) || !nrow(df)) return(tags$p("No rows."))
      HTML(
        knitr::kable(df, format = "html",
                     table.attr = "class='table table-sm table-bordered small mb-0'",
                     escape = TRUE)
      )
    }

    observeEvent(input$refresh_dataset, {
      req(can_edit())
      ds <- input$refresh_dataset
      req(nzchar(ds))
      m <- meta()
      rr <- m[dataset_key(m) == ds, , drop = FALSE]
      if (!nrow(rr)) {
        showNotification("Dataset metadata not found.", type = "error")
        return()
      }
      r <- rr[1, , drop = FALSE]
      if (!has_fetcher(r)) {
        showNotification(
          "This dataset doesn't have a registered fetcher. Use 'Update data' to refresh manually.",
          type = "warning", duration = 6)
        return()
      }
      handle <- as.character(r$fetcher)
      f      <- fetcher_for(handle)

      avail <- fetcher_available(handle)
      existing <- tryCatch(
        if (file.exists(dataset_path(r))) read.csv(dataset_path(r), stringsAsFactors = FALSE)
        else data.frame(),
        error = function(e) data.frame())

      fetch_state(list(
        dataset  = ds,
        row      = r,
        handle   = handle,
        fetcher  = f,
        latest   = fetcher_latest_year(handle),
        existing = existing,
        diff     = NULL
      ))

      msg <- if (!isTRUE(avail$ok)) {
        reason <- avail$reason %||% "\u2014"
        is_key_missing <- grepl("CENSUS_API_KEY|api[ _]?key", reason,
                                ignore.case = TRUE)
        if (is_key_missing) {
          list(
            text = tagList(
              tags$strong("Census API key not set."),
              " Add it to your ", tags$code(".Renviron"),
              " file (local) or the content's ", tags$strong("Vars"),
              " panel (Posit Connect), then reload. ",
              tags$a(href   = "https://api.census.gov/data/key_signup.html",
                     target = "_blank", rel = "noopener",
                     "Get a free key \u2192")
            ),
            type = "warning"
          )
        } else {
          list(text = paste0("Fetcher unavailable: ", reason),
               type = "warning")
        }
      } else NULL

      if (is.null(msg)) render_refresh_modal()
      else render_refresh_modal(msg$text, msg$type)
    })

    observeEvent(input$refresh_fetch, {
      st <- fetch_state()
      req(!is.null(st))
      years <- fetch_years_selected()
      if (!length(years)) {
        showNotification("Select at least one year to fetch.", type = "warning")
        return()
      }

      removeModal()
      withProgress(
        message = sprintf("Fetching %s…", st$fetcher$label %||% st$handle),
        value   = 0.3,
        {
          diff <- fetcher_dry_run(st$handle, years, st$existing)
          st$diff <- diff
          fetch_state(st)
        }
      )
      render_refresh_modal()
    })

    apply_and_save <- function(mode) {
      req(can_edit())
      st <- fetch_state()
      req(!is.null(st), !is.null(st$diff), isTRUE(st$diff$ok))
      keys <- st$diff$key_columns %||% st$fetcher$key_columns %||% "Year"

      new_df <- fetcher_apply_rows(st$existing, st$diff$fetched, keys, mode = mode)
      n_added <- nrow(new_df) - nrow(st$existing)

      res <- tryCatch(
        save_dataset_csv(new_df, st$dataset, st$row, force_replace = TRUE),
        error = function(e) list(status = "error",
                                 error  = conditionMessage(e)))

      if (!identical(res$status, "saved")) {
        showNotification(
          paste0("Could not save: ", res$error %||% "unknown error"),
          type = "error", duration = 8)
        return()
      }

      append_upload_log(
        st$dataset,
        if (identical(mode, "append")) "fetch_append" else "fetch_overwrite",
        note = sprintf("%s · years %s · %+d rows",
                       st$fetcher$label %||% st$handle,
                       paste(sort(unique(
                         suppressWarnings(as.integer(
                           st$diff$fetched[[st$fetcher$year_column %||% "Year"]]))
                       )), collapse = ", "),
                       n_added))

      removeModal()
      fetch_state(NULL)
      meta(load_meta())
      upload_log(load_upload_log())
      showNotification(
        sprintf("Updated %s (%+d rows).", st$dataset, n_added),
        type = "message", duration = 5)
    }

    observeEvent(input$refresh_apply_append,    apply_and_save("append"))
    observeEvent(input$refresh_apply_overwrite, apply_and_save("overwrite"))
    
    # ── Update data panel (toolbar + table / replace) ─────────────────────
    set_editor_mode <- function(mode) {
      ed <- editor_ctx()
      if (is.null(ed) || is.null(ed$dataset)) return()
      if (isTRUE(ed_dirty()) && !identical(mode, ed$mode %||% "")) {
        showNotification("Save or discard changes before switching modes.", type = "warning", duration = 4)
        return()
      }
      editor_ctx(list(dataset = ed$dataset, mode = mode))
    }
    
    observeEvent(input$open_update_panel, {
      ds <- input$open_update_panel
      req(nzchar(ds))
      if (!isTRUE(can_edit())) {
        showNotification("View-only accounts cannot update data.", type = "warning", duration = 4)
        return()
      }
      if (ed_dirty()) {
        showModal(modalDialog(
          title = "Unsaved changes",
          "Save or discard changes before opening another dataset.",
          footer = modalButton("OK")
        ))
        return()
      }
      removeModal()
      editor_ctx(list(dataset = ds, mode = "view"))
      showModal(modalDialog(
        title = tagList(icon("table"), " ", ds),
        size  = "xl",
        class = "update-data-modal",
        div(
          class = "update-data-panel",
          uiOutput(ns("update_toolbar")),
          uiOutput(ns("update_body"))
        ),
        footer = tagList(
          actionButton(ns("close_update_modal"), "Close", class = "btn-secondary")
        ),
        easyClose = FALSE
      ))
    })
    
    output$update_toolbar <- renderUI({
      ed <- editor_ctx()
      req(!is.null(ed), !is.null(ed$dataset))
      mode <- ed$mode %||% "view"
      btn <- function(m, label, icn) {
        active <- identical(mode, m)
        actionButton(
          ns(paste0("tb_", m)),
          label = tagList(icon(icn, class = "me-1"), label),
          class = if (active) {
            "btn btn-sm btn-primary me-1 fw-semibold overview-action-btn update-data-mode-btn"
          } else {
            "btn btn-sm btn-outline-secondary me-1 fw-semibold update-data-mode-btn"
          }
        )
      }
      div(
        class = "border-bottom pb-2 mb-2 update-data-toolbar",
        tags$p(class = "small text-muted mb-1 fw-semibold", "Mode"),
        div(
          class = "d-flex flex-wrap gap-1 align-items-center",
          btn("view",  "View",       "eye"),
          btn("add",   "Add data",   "plus"),
          btn("edit",  "Edit",       "pencil"),
          btn("replace", "Replace CSV", "upload")
        )
      )
    })
    
    render_sources_in_file <- function(ds) {
      m <- meta()
      r <- m[dataset_key(m) == ds, , drop = FALSE]
      if (nrow(r) != 1L) return(NULL)
      df <- load_dataset(r[1, , drop = FALSE])
      fs <- distinct_sources_in_dataframe(df)
      if (length(fs) == 0L) {
        return(tags$details(
          class = "editor-sources-in-file mb-3",
          tags$summary(
            class = "small text-muted",
            style = "cursor:pointer;",
            icon("layer-group"), " Sources in data file (none detected)"
          ),
          tags$div(class = "small text-muted mt-2 ps-3",
                   "No ", tags$code("Source"),
                   " column found (or all values empty). Add a ",
                   tags$code("Source"),
                   " column for row-level provenance.")
        ))
      }
      tags$details(
        class = "editor-sources-in-file mb-3",
        tags$summary(
          class = "small",
          style = "cursor:pointer;",
          icon("layer-group"), " Sources in data file (", length(fs), " distinct)"
        ),
        tags$ul(
          class = "mb-0 mt-2 ps-3 small",
          lapply(fs, function(s) {
            is_url <- grepl("^https?://", s, ignore.case = TRUE)
            disp <- shorten_source_display(s)
            tags$li(
              if (is_url) {
                tags$a(href = s, target = "_blank", rel = "noopener",
                       class = "text-break", disp, title = s)
              } else {
                tags$span(title = s, disp)
              }
            )
          })
        )
      )
    }

    render_dataset_details_form <- function(ds) {
      m <- meta()
      r <- m[dataset_key(m) == ds, , drop = FALSE]
      if (nrow(r) != 1L) return(NULL)
      r <- r[1, , drop = FALSE]
      tags$details(
        class = "editor-dataset-details mb-3",
        tags$summary(
          class = "small",
          style = "cursor:pointer;",
          icon("pen-to-square"), " Dataset details (metadata)"
        ),
        div(
          class = "border rounded p-3 mt-2 bg-body-tertiary editor-details-form",
          tags$p(
            class = "small text-muted mb-2",
            icon("circle-info"),
            " Edits here update ", tags$code("datasets.csv"),
            " and are saved separately from the table data."
          ),
          layout_columns(
            col_widths = c(6, 6),
            fillable = FALSE,
            textInput(ns("md_display_name"), "Display name",
                      value = r$display_name %||% "", width = "100%"),
            selectInput(
              ns("md_source_type"), "Source type",
              choices = c(
                "Public portal"        = "public",
                "Contact / partner"    = "contact",
                "Both public & contact"= "hybrid"
              ),
              selected = {
                t <- as.character(r$source_type %||% "")
                if (t %in% c("public", "contact", "hybrid")) t
                else classify_source(r)
              },
              width = "100%"
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            fillable = FALSE,
            textInput(ns("md_source_name"), "Primary / base source name",
                      value = r$source_name %||% "", width = "100%"),
            textInput(ns("md_source_url"), "Primary source URL",
                      value = r$source_url %||% "", width = "100%",
                      placeholder = "https://...")
          ),
          layout_columns(
            col_widths = c(6, 6),
            fillable = FALSE,
            textInput(ns("md_contact_name"), "Contact name",
                      value = r$contact_name %||% "", width = "100%"),
            textInput(ns("md_contact_email"), "Contact email",
                      value = r$contact_email %||% "", width = "100%")
          ),
          textAreaInput(ns("md_how"), "How to collect",
                        value = r$how_to_collect %||% "",
                        rows = 2, width = "100%"),
          div(
            class = "d-flex gap-2",
            actionButton(ns("md_save"), "Save dataset details",
                         class = "btn btn-sm btn-primary",
                         icon = icon("floppy-disk")),
            actionButton(ns("md_revert"), "Discard changes",
                         class = "btn btn-sm btn-outline-secondary fw-semibold",
                         icon = icon("xmark"))
          )
        )
      )
    }

    output$details_form <- renderUI({
      details_reload()
      ed <- editor_ctx()
      req(!is.null(ed), !is.null(ed$dataset))
      render_dataset_details_form(ed$dataset)
    })

    output$sources_in_file <- renderUI({
      ed_reload()
      ed <- editor_ctx()
      req(!is.null(ed), !is.null(ed$dataset))
      render_sources_in_file(ed$dataset)
    })

    render_backup_preview_table <- function(summary) {
      if (is.null(summary)) return(NULL)
      prev <- summary$preview
      if (!nrow(prev)) {
        return(tags$p(class = "small text-muted mb-0", "Backup is empty."))
      }
      header_cells <- lapply(colnames(prev), function(nm) tags$th(nm))
      body_rows <- lapply(seq_len(nrow(prev)), function(i) {
        tags$tr(lapply(prev[i, , drop = FALSE], function(v) {
          tags$td(class = "small", if (is.na(v)) "" else as.character(v))
        }))
      })
      tagList(
        tags$p(
          class = "small mb-2",
          tags$strong(summary$file),
          " — ", summary$rows, " rows, ", summary$cols, " columns",
          tags$span(class = "text-muted",
                    title = paste("Saved", summary$saved_at),
                    " · saved ", summary$saved_at_ago %||% summary$saved_at)
        ),
        tags$p(
          class = "small text-muted mb-2",
          icon("table-columns"), " ",
          paste(summary$colnames, collapse = ", ")
        ),
        tags$div(
          class = "table-responsive",
          style = "max-height:220px; overflow:auto;",
          tags$table(
            class = "table table-sm table-bordered mb-0",
            tags$thead(tags$tr(header_cells)),
            tags$tbody(body_rows)
          )
        ),
        tags$p(class = "small text-muted mb-0",
               "Showing first ", nrow(prev), " row(s).")
      )
    }

    render_backup_preview_popup <- function() {
      path <- backup_preview_path()
      if (is.null(path) || !nzchar(path)) return(NULL)
      ed <- editor_ctx()
      if (is.null(ed) || is.null(ed$dataset)) return(NULL)
      m <- meta()
      r <- m[dataset_key(m) == ed$dataset, , drop = FALSE]
      if (nrow(r) != 1L) return(NULL)
      summary <- backup_summary_from_ref(r[1, , drop = FALSE], path)
      if (is.null(summary)) return(NULL)
      close_id <- ns("close_backup_preview")
      tags$div(
        class = "backup-preview-overlay",
        onclick = sprintf(
          "Shiny.setInputValue('%s', true, {priority: 'event'});",
          close_id
        ),
        tags$div(
          class = "backup-preview-dialog card shadow",
          onclick = "event.stopPropagation();",
          tags$div(
            class = "card-header d-flex justify-content-between align-items-center py-2 px-3",
            tags$span(class = "fw-semibold small",
                      icon("eye"), " Backup preview"),
            tags$button(
              type = "button",
              class = "btn-close btn-close-sm",
              `aria-label` = "Close",
              onclick = sprintf(
                "Shiny.setInputValue('%s', true, {priority: 'event'});",
                close_id
              )
            )
          ),
          tags$div(
            class = "card-body py-2 px-3",
            render_backup_preview_table(summary)
          )
        )
      )
    }

    output$restore_confirm_bar <- renderUI({
      backup_restore_pending()
      pending <- backup_restore_pending()
      if (is.null(pending)) return(NULL)
      confirm_id <- ns("restore_backup_confirm")
      cancel_id <- ns("restore_backup_cancel")
      div(
        class = "alert alert-warning py-2 px-3 mt-2 mb-0 small editor-restore-confirm",
        tags$p(class = "mb-2",
               icon("triangle-exclamation"),
               " Replace current saved data with the previous version?",
               " Your current data will replace the backup slot first (you can restore again to undo)."),
        div(
          class = "d-flex gap-2",
          actionButton(confirm_id, "Confirm restore",
                       class = "btn btn-sm btn-warning",
                       icon = icon("rotate-left")),
          actionButton(cancel_id, "Cancel",
                       class = "btn btn-sm btn-outline-secondary")
        )
      )
    })

    output$backup_preview_popup <- renderUI({
      backup_preview_path()
      render_backup_preview_popup()
    })

    observeEvent(input$preview_backup, {
      req(can_edit())
      path <- input$preview_backup
      req(nzchar(path))
      backup_preview_path(path)
    })

    observeEvent(input$close_backup_preview, {
      backup_preview_path(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$restore_backup, {
      req(can_edit())
      val <- input$restore_backup
      parts <- strsplit(val, "|", fixed = TRUE)[[1]]
      req(length(parts) >= 2L)
      ds <- parts[[1]]
      bkp <- paste(parts[-1], collapse = "|")
      backup_restore_pending(list(dataset = ds, path = bkp))
    })

    observeEvent(input$restore_backup_cancel, {
      backup_restore_pending(NULL)
    })

    observeEvent(input$restore_backup_confirm, {
      req(can_edit())
      pending <- backup_restore_pending()
      req(!is.null(pending))
      m <- meta()
      r <- m[dataset_key(m) == pending$dataset, , drop = FALSE]
      req(nrow(r) == 1L)
      if (isTRUE(ed_dirty())) {
        showNotification("Save or discard changes before restoring a backup.", type = "warning", duration = 5)
        return()
      }
      res <- restore_dataset_from_backup(r[1, , drop = FALSE], pending$path)
      if (!isTRUE(res$ok)) {
        showNotification(res$error %||% "Could not restore backup.", type = "error", duration = 8)
        return()
      }
      backup_restore_pending(NULL)
      append_upload_log(pending$dataset, "restore_backup",
                        note = if (grepl("^neon:", pending$path)) {
                          "database backup"
                        } else {
                          basename(pending$path)
                        })
      refresh_meta()
      ed_reload(ed_reload() + 1L)
      backup_reload(backup_reload() + 1L)
      details_reload(details_reload() + 1L)
      showNotification(
        sprintf("Restored %s to the previous version (%d rows).",
                pending$dataset, res$rows %||% 0L),
        type = "message", duration = 6
      )
    })

    output$update_body <- renderUI({
      ed <- editor_ctx()
      req(!is.null(ed), !is.null(ed$dataset))
      mode <- ed$mode %||% "view"
      frame_top <- tagList(
        uiOutput(ns("details_form")),
        uiOutput(ns("sources_in_file")),
        uiOutput(ns("backup_preview_popup"))
      )
      if (identical(mode, "replace")) {
        m <- meta()
        r <- m[dataset_key(m) == ed$dataset, , drop = FALSE]
        req(nrow(r) == 1)
        tagList(
          frame_top,
          div(
            class = "alert alert-secondary border small p-2 mb-3",
            tags$p(class = "mb-1", icon("circle-info"), " ",
                   tags$strong("How to collect: "),
                   if (has_val(r$how_to_collect[1])) r$how_to_collect[1] else "No instructions."),
            div(
              class = "form-check",
              tags$input(class = "form-check-input", type = "checkbox", id = ns("force_replace2")),
              tags$label(class = "form-check-label small", `for` = ns("force_replace2"),
                         "Replace entire file (overwrite all rows)")
            )
          ),
          fileInput(ns("replace_upload"), "Choose CSV:", accept = ".csv", width = "100%"),
          uiOutput(ns("replace_preview")),
          uiOutput(ns("replace_confirm_btn"))
        )
      } else {
        tagList(frame_top, datasetEditorUI(ns("ded")), uiOutput(ns("restore_confirm_bar")))
      }
    })
    
    observeEvent(input$tb_view,  { set_editor_mode("view") })
    observeEvent(input$tb_add,   { set_editor_mode("add") })
    observeEvent(input$tb_edit,  { set_editor_mode("edit") })
    observeEvent(input$tb_replace, { set_editor_mode("replace") })
    
    observeEvent(input$md_save, {
      req(can_edit())
      ed <- editor_ctx()
      req(!is.null(ed), !is.null(ed$dataset))
      res <- tryCatch(
        update_dataset_metadata(
          ed$dataset,
          list(
            display_name   = input$md_display_name %||% "",
            source_type    = input$md_source_type %||% "public",
            source_name    = input$md_source_name %||% "",
            source_url     = input$md_source_url %||% "",
            contact_name   = input$md_contact_name %||% "",
            contact_email  = input$md_contact_email %||% "",
            how_to_collect = input$md_how %||% ""
          )
        ),
        error = function(e) list(ok = FALSE, error = conditionMessage(e))
      )
      if (!isTRUE(res$ok)) {
        showNotification(res$error %||% "Could not save metadata.",
                         type = "error", duration = 8)
        return()
      }
      refresh_meta()
      details_reload(details_reload() + 1L)
      showNotification("Dataset details saved.", type = "message", duration = 4)
    })
    
    observeEvent(input$md_revert, {
      ed <- editor_ctx()
      req(!is.null(ed), !is.null(ed$dataset))
      refresh_meta()
      details_reload(details_reload() + 1L)
      showNotification("Dataset details discarded.", type = "message", duration = 3)
    })

    replace_df <- reactive({
      req(input$replace_upload)
      tryCatch(
        read.csv(input$replace_upload$datapath, stringsAsFactors = FALSE),
        error = function(e) NULL
      )
    })
    
    output$replace_preview <- renderUI({
      df <- replace_df()
      if (is.null(df)) return(NULL)
      ed <- editor_ctx()
      req(!is.null(ed))
      m <- meta()
      r <- m[dataset_key(m) == ed$dataset, , drop = FALSE]
      existing <- if (nrow(r) > 0) load_dataset(r) else NULL
      warn <- if (!is.null(existing)) {
        mc <- setdiff(names(existing), names(df))
        ec <- setdiff(names(df), names(existing))
        if (length(mc) + length(ec) > 0)
          div(class = "alert alert-warning small", "Column mismatch vs. current file.")
      } else NULL
      n <- nrow(df)
      pre_df <- if (n > REPLACE_PREVIEW_MAX_ROWS) {
        utils::head(df, REPLACE_PREVIEW_MAX_ROWS)
      } else {
        df
      }
      big_note <- if (n > REPLACE_PREVIEW_MAX_ROWS) {
        div(class = "alert alert-info small py-1 mb-2",
            sprintf(
              "Preview shows the first %s of %s rows (scroll to review). Full file will be applied on confirm.",
              format(REPLACE_PREVIEW_MAX_ROWS, big.mark = ","), format(n, big.mark = ",")
            ))
      } else NULL
      preview_caption <- if (n > REPLACE_PREVIEW_MAX_ROWS) {
        sprintf(
          "Preview: first %s of %s rows \u00d7 %d cols (scrollable)",
          format(REPLACE_PREVIEW_MAX_ROWS, big.mark = ","), format(n, big.mark = ","), ncol(df)
        )
      } else {
        sprintf("Preview: %d \u00d7 %d (full table, scrollable)", n, ncol(df))
      }
      tagList(
        warn,
        big_note,
        tags$p(class = "small text-muted", preview_caption),
        div(
          style = "overflow:auto; max-height:min(60vh, 520px); border:1px solid #dee2e6; border-radius:4px;",
          renderTable(pre_df, striped = TRUE, bordered = TRUE)
        )
      )
    })
    
    output$replace_confirm_btn <- renderUI({
      req(replace_df())
      actionButton(ns("replace_confirm"), "Apply replace",
                   class = "btn btn-success", icon = icon("floppy-disk"))
    })
    
    observeEvent(input$replace_confirm, {
      req(can_edit())
      df <- replace_df()
      req(df)
      ed <- editor_ctx()
      req(!is.null(ed))
      ds <- ed$dataset
      m  <- meta()
      r  <- m[dataset_key(m) == ds, ]
      req(nrow(r) == 1)
      force <- isTRUE(input$force_replace2)
      result <- tryCatch(
        save_dataset_csv(df, ds, r[1, , drop = FALSE], force_replace = force),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 8)
          NULL
        }
      )
      if (is.null(result)) return()
      if (!is.null(result$status) && result$status == "error") {
        showNotification(result$error %||% "Save error", type = "error", duration = 8)
        return()
      }
      if (result$status == "skipped") {
        showNotification("No new rows to add.", type = "warning", duration = 5)
      } else {
        refresh_meta()
        ed_reload(ed_reload() + 1L)
        showNotification("File replaced.", type = "message", duration = 4)
        set_editor_mode("view")
      }
    })
    
    observeEvent(input$close_update_modal, {
      if (ed_dirty()) {
        showNotification("Save or discard changes before closing.", type = "warning", duration = 4)
        return()
      }
      editor_ctx(NULL)
      removeModal()
    })
    
  })
}
