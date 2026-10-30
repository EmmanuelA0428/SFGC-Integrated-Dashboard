# internal/modules/dataset_editor_module.R
# In-app CSV editor: view / add / edit modes, dirty tracking, atomic save via helpers.

datasetEditorUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("status_banner")),
    uiOutput(ns("mode_hint")),
    uiOutput(ns("editor_loading")),
    div(
      class = "editor-table-frame border rounded bg-body",
      uiOutput(ns("hot_ui"))
    ),
    div(
      class = "d-flex gap-2 flex-wrap mt-2 align-items-center editor-action-row",
      actionButton(ns("btn_save"), "Save changes",
                   class = "btn btn-primary btn-sm fw-semibold",
                   icon  = icon("floppy-disk")),
      actionButton(ns("btn_discard"), "Discard changes",
                   class = "btn btn-outline-secondary btn-sm fw-semibold editor-revert-btn",
                   icon  = icon("xmark")),
      uiOutput(ns("backup_affordance")),
      tags$span(class = "small text-muted ms-auto editor-scroll-hint",
                icon("circle-info"),
                " Scroll inside the framed area above to navigate the table.")
    )
  )
}

datasetEditorServer <- function(id, ctx, meta, dirty_rv, reload_counter, refresh_meta,
                                backup_info = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    snapshot <- reactiveVal(NULL)
    editor_loading <- reactiveVal(FALSE)
    hot_reload_key <- reactiveVal(0L)
    skip_hot_events <- reactiveVal(0L)

    editor_trigger <- reactive({
      c <- ctx()
      req(!is.null(c), !is.na(c$dataset), nzchar(c$dataset %||% ""))
      list(c$dataset, c$mode %||% "view", reload_counter())
    })

    load_snapshot <- function() {
      c <- ctx()
      req(!is.null(c), !is.null(c$dataset))
      m <- meta()
      r <- m[dataset_key(m) == c$dataset, , drop = FALSE]
      req(nrow(r) == 1)
      df <- load_dataset(r[1, ])
      req(!is.null(df))
      df <- ensure_df_colnames(df)
      snapshot(list(
        meta_row = r[1, , drop = FALSE],
        df       = df,
        fp       = dataset_fingerprint(r[1, ]),
        mode     = c$mode %||% "view"
      ))
    }

    push_hot_revert <- function(snap) {
      if (is.null(snap) || is.null(snap$df)) return()
      df <- snap$df
      session$sendCustomMessage(
        "editorHotRevert",
        list(
          widgetId = session$ns("hot"),
          colnames = as.character(colnames(df)),
          data     = df
        )
      )
    }

    observeEvent(editor_trigger(), {
      editor_loading(TRUE)
      snapshot(NULL)
      load_snapshot()
      editor_loading(FALSE)
      dirty_rv(FALSE)
      skip_hot_events(8L)
    }, ignoreNULL = TRUE)

    output$editor_loading <- renderUI({
      if (isTRUE(editor_loading())) {
        loading_spinner_ui("Loading dataset...")
      }
    })

    output$status_banner <- renderUI({
      req(snapshot())
      if (isTRUE(dirty_rv())) {
        div(
          class = "alert alert-warning py-1 px-2 small mb-2",
          icon("triangle-exclamation"), " Unsaved changes"
        )
      } else {
        div(class = "small text-muted mb-2",
            icon("circle-check"), " In sync with last save")
      }
    })

    output$mode_hint <- renderUI({
      snap <- snapshot()
      req(!is.null(snap))
      if (identical(snap$mode, "add")) {
        div(
          class = "small text-muted mb-2",
          icon("plus"),
          " Existing rows are read-only. Type in the blank row(s) at the bottom, or right-click for more row actions."
        )
      } else {
        NULL
      }
    })

    output$hot_ui <- renderUI({
      req(!editor_loading())
      hot_reload_key()
      editor_trigger()
      rHandsontableOutput(ns("hot"))
    })

    output$hot <- renderRHandsontable({
      req(!editor_loading())
      editor_trigger()
      hot_reload_key()
      snap <- snapshot()
      req(!is.null(snap), nzchar(snap$meta_row$dataset %||% ""))
      mode <- snap$mode
      df   <- ensure_df_colnames(snap$df)
      hdrs <- as.character(colnames(df))
      rh <- rhandsontable(df, readOnly = (mode == "view"),
                          height = 440, colHeaders = hdrs, rowHeaders = TRUE)
      rh <- rhandsontable::hot_table(
        rh,
        stretchH = "all",
        contextMenu = (mode != "view"),
        minSpareRows = if (identical(mode, "add")) 1L else 0L,
        fixedRowsTop = 0L,
        highlightCol = FALSE,
        highlightRow = FALSE
      )
      if (identical(mode, "add") && nrow(df) > 0L) {
        rh <- rhandsontable::hot_row(rh, row = seq_len(nrow(df)), readOnly = TRUE)
      }
      rh
    })

    # Rhandsontable fires input$hot on init, redraw, and focus — not only on edits.
    observeEvent(input$hot, {
      n <- skip_hot_events()
      if (n > 0L) {
        skip_hot_events(n - 1L)
        dirty_rv(FALSE)
        return()
      }
      snap <- snapshot()
      req(!is.null(snap))
      df <- tryCatch(rhandsontable::hot_to_r(input$hot), error = function(e) NULL)
      if (is.null(df)) return()
      cmp_df <- df
      if (identical(snap$mode, "add")) {
        cmp_df <- normalize_add_mode_hot_for_compare(df, nrow(snap$df))
      }
      if (df_equal_content(cmp_df, snap$df)) {
        dirty_rv(FALSE)
      } else {
        dirty_rv(TRUE)
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$btn_discard, {
      editor_loading(TRUE)
      snapshot(NULL)
      load_snapshot()
      snap <- snapshot()
      req(!is.null(snap))
      skip_hot_events(12L)
      dirty_rv(FALSE)
      push_hot_revert(snap)
      hot_reload_key(hot_reload_key() + 1L)
      reload_counter(reload_counter() + 1L)
      editor_loading(FALSE)
      showNotification("Discarded unsaved changes.", type = "message", duration = 3)
    })

    output$backup_affordance <- renderUI({
      if (is.null(backup_info)) return(NULL)
      bi <- backup_info()
      if (is.null(bi) || is.null(bi$summary)) return(NULL)
      sm <- bi$summary
      tags$div(
        class = "d-inline-flex align-items-center flex-wrap gap-1 ms-1 editor-backup-affordance",
        tags$span(
          class = "small text-muted",
          icon("clock-rotate-left"),
          " Previous version",
          tags$span(
            class = "text-muted",
            title = paste("Saved", sm$saved_at),
            paste0(" · ", sm$saved_at_ago %||% sm$saved_at)
          )
        ),
        tags$button(
          type = "button",
          class = "btn btn-sm btn-outline-secondary py-0 px-2",
          title = "Preview previous version",
          `data-path` = bi$ref,
          onclick = sprintf(
            "Shiny.setInputValue('%s', this.getAttribute('data-path'), {priority: 'event'});",
            bi$preview_input_id
          ),
          icon("eye")
        ),
        tags$button(
          type = "button",
          class = "btn btn-sm btn-outline-warning py-0 px-2",
          title = "Restore previous version",
          `data-dataset` = bi$dataset,
          `data-path` = bi$ref,
          onclick = sprintf(
            "Shiny.setInputValue('%s', this.getAttribute('data-dataset') + '|' + this.getAttribute('data-path'), {priority: 'event'});",
            bi$restore_input_id
          ),
          icon("rotate-left")
        )
      )
    })

    observeEvent(input$btn_save, {
      req(get_app_can_edit())
      snap <- snapshot()
      req(!is.null(snap))
      df <- tryCatch(rhandsontable::hot_to_r(input$hot), error = function(e) NULL)
      req(df)
      res <- save_editor_dataset(
        df, snap$meta_row, snap$fp, snap$mode, snap$df
      )
      if (!isTRUE(res$ok)) {
        showNotification(res$error %||% "Save failed", type = "error", duration = 10)
        return()
      }
      dirty_rv(FALSE)
      refresh_meta()
      showNotification(paste("Saved", res$rows %||% nrow(df), "rows."), type = "message", duration = 4)
      editor_loading(TRUE)
      snapshot(NULL)
      load_snapshot()
      editor_loading(FALSE)
      skip_hot_events(8L)
      reload_counter(reload_counter() + 1L)
    })
  })
}
