# internal/modules/explorer_tab.R
# Report Mode — sidebar: goal + dataset; main: plot builder → chart → export/caption → accordion table.

explorerUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Row 1: Sidebar (data + plot builder) + main (plot + export)
    layout_columns(
      col_widths = c(3, 9),
      fillable   = FALSE,
      tagList(
        card(
          class = "mb-3",
          card_header(tagList(icon("database"), " Data & plot builder")),
          card_body(
            uiOutput(ns("explorer_loading")),
            selectInput(
              ns("explorer_goal"), "Goal:",
              choices = c("All goals" = "all",
                          setNames(names(GOAL_LABELS), GOAL_LABELS)),
              width   = "100%"
            ),
            selectInput(ns("explorer_dataset"), "Dataset:",
                        choices = NULL, width = "100%"),
            hr(class = "my-2"),
            selectInput(
              ns("plot_kind"), "Plot type:",
              choices = c(
                "Auto (bar, or points if Color by is set)" = "auto",
                "Bar / column" = "bar",
                "Line" = "line",
                "Scatter / points" = "scatter"
              ),
              selected = "auto",
              width    = "100%"
            ),
            uiOutput(ns("explorer_x_ui")),
            uiOutput(ns("explorer_y_ui")),
            uiOutput(ns("explorer_year_ui")),
            uiOutput(ns("explorer_color_ui")),
            uiOutput(ns("explorer_filter_ui"))
          )
        )
      ),
      tagList(
        card(
          class = "mb-3",
          card_header(uiOutput(ns("explorer_chart_title"))),
          card_body(plotOutput(ns("explorer_chart"), height = "380px"))
        ),
        card(
          class = "mb-3",
          card_header(tagList(icon("file-export"), " Export & caption")),
          card_body(
            layout_columns(
              col_widths = c(6, 6),
              fillable   = FALSE,
              downloadButton(
                ns("dl_csv"), "Filtered CSV",
                class = "btn btn-sm btn-outline-secondary w-100 mb-2"
              ),
              downloadButton(
                ns("dl_png"), "Plot PNG",
                class = "btn btn-sm btn-outline-secondary w-100 mb-2"
              )
            ),
            textAreaInput(
              ns("caption_text"), "Caption (copy for slides)",
              rows = 3, width = "100%"
            ),
            actionButton(
              ns("copy_cap"), "Copy caption",
              class = "btn btn-sm btn-outline-primary mb-2"
            ),
            uiOutput(ns("explorer_source_info"))
          )
        )
      )
    ),
    
    # Row 2: Filtered data
    card(
      class = "mb-3",
      card_header(tagList(
        "Filtered data ",
        tags$span(
          class = "small text-muted ms-1",
          icon("circle-info"),
          " This is the exact table behind the chart after filters are applied."
        )
      )),
      card_body(DTOutput(ns("explorer_table")))
    )
  )
}

explorer_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

# Coerce to numeric for filters/plots (CSV often has Year/Value as character).
col_as_numeric <- function(x) {
  if (is.numeric(x) || is.integer(x)) return(as.numeric(x))
  if (is.logical(x)) return(as.numeric(x))
  suppressWarnings(as.numeric(if (is.factor(x)) as.character(x) else as.character(x)))
}

# Columns with at least one value that parses as numeric (usable as Y).
numeric_plot_columns <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0L) return(character())
  names(df)[vapply(df, function(v) {
    if (!length(v)) return(FALSE)
    n <- col_as_numeric(v)
    any(!is.na(n))
  }, logical(1))]
}

# Numeric Year vector or NULL if column missing / not coercible.
year_numeric <- function(df) {
  if (!is.data.frame(df) || !"Year" %in% names(df)) return(NULL)
  yn <- col_as_numeric(df$Year)
  if (all(is.na(yn))) return(NULL)
  yn
}

explorerServer <- function(id, meta, meta_loading = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    session$userData$explorer_context_rv <- reactiveVal(list(
      goal = "all",
      dataset = ""
    ))

    observe({
      session$userData$explorer_context_rv(list(
        goal = as.character(input$explorer_goal %||% "all"),
        dataset = as.character(input$explorer_dataset %||% "")
      ))
    })

    meta_key <- function(m) paste0(m$folder, "/", m$dataset)

    ds_state <- reactiveValues(loading = FALSE, df = NULL, key = NULL)

    observe({
      if (!is.null(meta_loading) && isTRUE(meta_loading())) return()
      m <- meta()
      if (is.null(m)) return()
      m <- active_meta(m)
      g <- input$explorer_goal
      if (is.null(g)) return()
      if (g != "all") m <- m[as.character(m$goal) == g, ]
      if (nrow(m) == 0) {
        updateSelectInput(session, "explorer_dataset",
                          choices = list(), selected = NULL)
        return()
      }
      labs    <- make.unique(as.character(m$display_name), sep = " ")
      choices <- setNames(meta_key(m), labs)
      cur <- input$explorer_dataset
      sel <- if (!is.null(cur) && nzchar(as.character(cur)) &&
                  as.character(cur) %in% choices) {
        as.character(cur)
      } else {
        choices[[1]]
      }
      updateSelectInput(session, "explorer_dataset",
                        choices = choices, selected = sel)
    })

    meta_row <- reactive({
      req(input$explorer_dataset)
      m <- meta()
      req(!is.null(m))
      sel <- as.character(input$explorer_dataset)[[1]]
      r <- m[meta_key(m) == sel, , drop = FALSE]
      req(nrow(r) >= 1)
      r[1L, , drop = FALSE]
    })

    observeEvent(input$explorer_dataset, {
      key <- as.character(input$explorer_dataset %||% "")[[1]]
      if (!nzchar(key)) {
        ds_state$loading <- FALSE
        ds_state$df <- NULL
        ds_state$key <- NULL
        return()
      }
      ds_state$loading <- TRUE
      ds_state$df <- NULL
      ds_state$key <- key
      r <- meta_row()
      fol <- as.character(r$folder)[[1]]
      ds  <- as.character(r$dataset)[[1]]
      df <- tryCatch(
        load_dataset(r[1L, , drop = FALSE]),
        error = function(e) {
          message("[explorer] load_dataset error for ", fol, "/", ds, ": ",
                  conditionMessage(e))
          NULL
        }
      )
      ds_state$df <- df
      ds_state$key <- key
      ds_state$loading <- FALSE
    }, ignoreNULL = FALSE)

    explorer_df <- reactive({
      req(
        !ds_state$loading,
        identical(ds_state$key, as.character(input$explorer_dataset %||% "")[[1]])
      )
      ds_state$df
    })

    output$explorer_loading <- renderUI({
      if (isTRUE(ds_state$loading)) {
        loading_spinner_ui("Loading dataset...")
      }
    })
    
    # Year filter (numeric coercion — CSV Year is often character)
    output$explorer_year_ui <- renderUI({
      df <- explorer_df()
      req(df)
      yn <- year_numeric(df)
      if (is.null(yn)) return(NULL)
      yrs <- sort(unique(yn[!is.na(yn)]))
      if (length(yrs) == 0) return(NULL)
      y0 <- min(yrs)
      y1 <- max(yrs)
      step <- if (length(yrs) > 1L && all(abs(yrs - round(yrs)) < 1e-9)) 1L else NULL
      sliderInput(
        ns("year_rng"), "Year range:",
        min = y0, max = y1,
        value = c(y0, y1),
        step = step, width = "100%"
      )
    })
    
    explorer_filtered <- reactive({
      df  <- explorer_df()
      req(df)
      # Only when Year parses as numbers (avoids stale slider vs non-numeric Year)
      if (!is.null(year_numeric(df)) && !is.null(input$year_rng)) {
        yn <- col_as_numeric(df$Year)
        lo <- input$year_rng[[1]]
        hi <- input$year_rng[[2]]
        df <- df[!is.na(yn) & yn >= lo & yn <= hi, , drop = FALSE]
      }
      loc <- intersect(c("Community", "Location", "Geography", "ZIP"), names(df))
      if (length(loc) > 0 &&
          !is.null(input$explorer_filter_val) &&
          input$explorer_filter_val != "__all__") {
        col <- loc[[1]]
        fv <- input$explorer_filter_val
        df <- df[as.character(df[[col]]) == as.character(fv), , drop = FALSE]
      }
      df
    })
    
    output$explorer_x_ui <- renderUI({
      df <- explorer_df()
      req(df)
      nm <- names(df)
      def <- if ("Year" %in% nm) "Year" else nm[[1]]
      selectInput(ns("explorer_x"), "X axis",
                  choices = nm, selected = def, width = "100%")
    })
    
    output$explorer_y_ui <- renderUI({
      df <- explorer_df()
      req(df)
      num <- numeric_plot_columns(df)
      if (length(num) == 0) {
        return(p(class = "small text-muted",
                 "No numeric (or number-like) columns for Y. Check column types in the CSV."))
      }
      def <- if ("Value" %in% num) "Value" else num[[1]]
      selectInput(ns("explorer_y"), "Y axis",
                  choices = num, selected = def, width = "100%")
    })
    
    output$explorer_color_ui <- renderUI({
      df <- explorer_df()
      req(df)
      nm <- names(df)
      catc <- nm[vapply(df, function(v) is.character(v) || is.factor(v), logical(1))]
      catc <- setdiff(catc, c(input$explorer_x %||% "", input$explorer_y %||% ""))
      if (length(catc) == 0) return(NULL)
      selectInput(ns("explorer_color"), "Color by (optional):",
                  choices = c("(none)" = "__none__", catc),
                  selected = "__none__", width = "100%")
    })
    
    output$explorer_filter_ui <- renderUI({
      df  <- explorer_df()
      req(df)
      loc <- intersect(c("Community", "Location", "Geography", "ZIP"), names(df))
      if (length(loc) == 0) return(NULL)
      col  <- loc[1]
      vals <- sort(unique(df[[col]]))
      selectInput(
        ns("explorer_filter_val"),
        paste("Filter by", col, ":"),
        choices = c("All" = "__all__", vals),
        width   = "100%"
      )
    })
    
    output$explorer_source_info <- renderUI({
      req(input$explorer_dataset)
      r  <- meta_row()
      tagList(
        render_source_line(r, variant = "compact"),
        tags$p(class = "small text-muted mb-0 mt-1",
               icon("circle-info"),
               " Per-row sources (if any) are listed on the dataset's ",
               tags$em("Update data"), " screen.")
      )
    })
    
    output$explorer_chart_title <- renderUI({
      r <- meta_row()
      tagList(
        tags$strong(r$display_name),
        tags$span(class = "text-muted ms-2 small",
                  input$explorer_y %||% "")
      )
    })
    
    plot_r <- reactive({
      df <- explorer_filtered()
      req(df, nrow(df) > 0)
      xc <- input$explorer_x
      yc <- input$explorer_y
      req(xc %in% names(df), yc %in% names(df))
      cc <- input$explorer_color
      if (is.null(cc) || cc == "__none__") cc <- NULL
      use_colour <- !is.null(cc) && cc %in% names(df)
      
      # ggplot needs numeric Y for these geoms; CSV columns are often character.
      df_plot <- df
      df_plot[[yc]] <- col_as_numeric(df_plot[[yc]])
      ok <- !is.na(df_plot[[yc]])
      df_plot <- df_plot[ok, , drop = FALSE]
      if (nrow(df_plot) == 0) {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = "No numeric Y values in this column for the current filters.",
                     size = 4) +
            theme_void()
        )
      }
      
      pk <- input$plot_kind %||% "auto"
      if (pk == "auto") {
        pk <- if (use_colour) "scatter" else "bar"
      }
      
      # Line charts: order by X when X is numeric (avoids wrong line connections).
      if (pk == "line") {
        xv <- col_as_numeric(df_plot[[xc]])
        if (!all(is.na(xv))) {
          ord <- order(xv, seq_len(nrow(df_plot)))
          df_plot <- df_plot[ord, , drop = FALSE]
        }
      }
      
      # If X is numeric and all values are whole numbers (typical for Year),
      # coerce to numeric so breaks can be forced to integers. Otherwise
      # ggplot picks "pretty" fractional breaks (e.g. 2022.75) when only a
      # couple of years are present.
      x_vals_num    <- suppressWarnings(col_as_numeric(df_plot[[xc]]))
      x_is_numeric  <- any(!is.na(x_vals_num))
      x_is_integer  <- x_is_numeric &&
                       all(abs(x_vals_num[!is.na(x_vals_num)] -
                               round(x_vals_num[!is.na(x_vals_num)])) < 1e-9)
      if (x_is_numeric) {
        df_plot[[xc]] <- x_vals_num
      }
      integer_breaks <- if (x_is_integer) {
        u <- sort(unique(round(x_vals_num[!is.na(x_vals_num)])))
        if (length(u) == 0) NULL
        else if (length(u) <= 12) u
        else {
          # too many years — let ggplot choose, but snap to integers
          function(limits) {
            rng <- pretty(limits, n = 8)
            unique(round(rng))
          }
        }
      } else NULL

      if (pk == "bar") {
        p <- ggplot(df_plot, aes(x = .data[[xc]], y = .data[[yc]])) +
          geom_col(fill = "#2C6E49", alpha = 0.85) +
          labs(x = xc, y = yc)
        leg <- theme()
      } else if (pk == "line") {
        if (use_colour) {
          p <- ggplot(df_plot, aes(x = .data[[xc]], y = .data[[yc]], colour = .data[[cc]])) +
            geom_line(linewidth = 0.8) +
            geom_point(size = 2.5, alpha = 0.9) +
            labs(x = xc, y = yc, colour = NULL) +
            scale_colour_brewer(palette = "Set2")
          leg <- theme(legend.position = "bottom")
        } else {
          p <- ggplot(df_plot, aes(x = .data[[xc]], y = .data[[yc]])) +
            geom_line(color = "#2C6E49", linewidth = 0.8) +
            geom_point(color = "#2C6E49", size = 2.5, alpha = 0.85) +
            labs(x = xc, y = yc)
          leg <- theme()
        }
      } else {
        # scatter
        if (use_colour) {
          p <- ggplot(df_plot, aes(x = .data[[xc]], y = .data[[yc]], colour = .data[[cc]])) +
            geom_point(size = 2.5, alpha = 0.85) +
            labs(x = xc, y = yc, colour = NULL) +
            scale_colour_brewer(palette = "Set2")
          leg <- theme(legend.position = "bottom")
        } else {
          p <- ggplot(df_plot, aes(x = .data[[xc]], y = .data[[yc]])) +
            geom_point(size = 2.5, alpha = 0.85, color = "#2C6E49") +
            labs(x = xc, y = yc)
          leg <- theme()
        }
      }

      if (!is.null(integer_breaks)) {
        p <- p + scale_x_continuous(breaks = integer_breaks)
      }

      p + explorer_theme() + leg
    })
    
    output$explorer_chart <- renderPlot({
      shiny::validate(
        shiny::need(nrow(explorer_filtered()) > 0, "No rows after filters."),
        shiny::need(!is.null(input$explorer_x) && !is.null(input$explorer_y),
                    "Choose X and Y.")
      )
      plot_r()
    })
    
    output$explorer_table <- renderDT({
      df <- explorer_filtered()
      req(df)
      datatable(
        df,
        rownames = FALSE,
        filter   = "top",
        options  = list(
          scrollX = TRUE,
          scrollY = "480px",
          scrollCollapse = TRUE,
          paging = FALSE
        ),
        class    = "table table-sm table-hover"
      )
    })
    
    dataset_stem <- function() {
      ds <- input$explorer_dataset %||% "dataset"
      if (!nzchar(ds)) ds <- "dataset"
      # Composite key is "folder/file.csv"; for file names, strip the folder
      # prefix and .csv extension.
      ds <- basename(ds)
      sub("\\.csv$", "", ds, ignore.case = TRUE)
    }
    
    output$dl_csv <- downloadHandler(
      filename = function() {
        paste0(dataset_stem(), "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        df <- tryCatch(explorer_filtered(), error = function(e) NULL)
        if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
          showNotification(
            "No filtered data to export. Select a goal + dataset and ensure filters return rows.",
            type = "warning", duration = 6
          )
          write.csv(data.frame(note = "No filtered data available"),
                    file, row.names = FALSE)
          return()
        }
        tryCatch(
          write.csv(df, file, row.names = FALSE),
          error = function(e) {
            showNotification(paste("CSV export failed:", conditionMessage(e)),
                             type = "error", duration = 8)
            write.csv(data.frame(error = conditionMessage(e)), file, row.names = FALSE)
          }
        )
      }
    )
    
    output$dl_png <- downloadHandler(
      filename = function() {
        paste0(dataset_stem(), "_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        p <- tryCatch(plot_r(), error = function(e) NULL)
        if (is.null(p) || !inherits(p, "ggplot")) {
          showNotification(
            "No chart to export. Pick X and Y (numeric) with rows after filters, then retry.",
            type = "warning", duration = 6
          )
          grDevices::png(file, width = 800, height = 500)
          graphics::plot.new(); graphics::title(main = "No chart available")
          grDevices::dev.off()
          return()
        }
        tryCatch(
          ggplot2::ggsave(file, plot = p, width = 8, height = 5, dpi = 150,
                          device = "png"),
          error = function(e) {
            showNotification(paste("PNG export failed:", conditionMessage(e)),
                             type = "error", duration = 8)
            grDevices::png(file, width = 800, height = 500)
            graphics::plot.new(); graphics::title(main = "Plot export failed")
            grDevices::dev.off()
          }
        )
      }
    )

    # (Goal-report builder moved to the Reports tab — full report builder,
    #  WYSIWYG click-to-edit, Word output. The deprecated reactives, pandoc
    #  sniffer, fallback writer, and download handler that lived here were
    #  removed to cut reactive cycles from Explore.)
    if (FALSE) {
    report_format_plan <- reactive({
      ensure_pandoc_env()
      want <- input$rep_format %||% "pdf"
      has_pandoc <- isTRUE(rmarkdown::pandoc_available())
      has_latex  <- nzchar(Sys.which("xelatex")) ||
                    nzchar(Sys.which("pdflatex")) ||
                    (requireNamespace("tinytex", quietly = TRUE) &&
                       isTRUE(tinytex::is_tinytex()))
      goal <- effective_report_goal()
      goal_ok <- nzchar(goal) && !identical(goal, "all")
      goal_source <- if (isTRUE(nzchar(input$explorer_goal %||% "") &&
                              !identical(input$explorer_goal, "all")))
        "goal-select" else if (goal_ok) "dataset" else "none"
      fell_back <- FALSE
      final <- want
      if (identical(final, "pdf") && !has_latex) {
        final <- "docx"
        fell_back <- TRUE
      }
      list(
        requested  = want,
        final      = final,
        ext        = if (identical(final, "docx")) "docx" else "pdf",
        has_pandoc = has_pandoc,
        has_latex  = has_latex,
        fell_back  = fell_back,
        goal        = goal,
        goal_ok     = goal_ok,
        goal_source = goal_source,
        ready       = has_pandoc && goal_ok
      )
    })
    
    # Small helper: write a readable error "report" so the user always gets
    # a file matching the expected extension, never Shiny's generic HTML error.
    write_fallback_report <- function(file, ext, title, body_lines) {
      if (identical(ext, "docx") && isTRUE(rmarkdown::pandoc_available())) {
        tmp_rmd <- tempfile(fileext = ".Rmd")
        writeLines(c(
          "---",
          sprintf("title: \"%s\"", title),
          "output: word_document",
          "---",
          "",
          body_lines
        ), tmp_rmd)
        ok <- tryCatch({
          rmarkdown::render(
            input             = tmp_rmd,
            output_format     = rmarkdown::word_document(),
            output_file       = basename(file),
            output_dir        = dirname(file),
            intermediates_dir = dirname(tmp_rmd),
            knit_root_dir     = dirname(tmp_rmd),
            envir = new.env(parent = globalenv()),
            quiet = TRUE
          )
          TRUE
        }, error = function(e) FALSE)
        if (ok && file.exists(file)) return(invisible(TRUE))
      }
      writeLines(c(paste0("# ", title), "", body_lines), file)
      invisible(FALSE)
    }
    
    output$dl_goal_report <- downloadHandler(
      filename = function() {
        plan <- tryCatch(report_format_plan(),
                         error = function(e) list(ext = "docx", goal = "goal"))
        g <- if (!is.null(plan$goal) && nzchar(plan$goal)) plan$goal else "goal"
        g <- gsub("[^A-Za-z0-9]", "_", g)
        paste0("Internal_Goal_Report_", g, "_", Sys.Date(), ".", plan$ext)
      },
      content = function(file) {
        plan <- tryCatch(report_format_plan(),
                         error = function(e) NULL)
        if (is.null(plan)) plan <- list(ext = "docx", has_pandoc = FALSE,
                                        has_latex = FALSE, fell_back = FALSE,
                                        goal_ok = FALSE, final = "docx")
        message("[goal-report] content() plan=", toString(plan),
                " dest=", file)
        
        # Guard: user hasn't picked a specific goal. Always return a real file.
        if (!isTRUE(plan$goal_ok)) {
          showNotification(
            "Pick a specific Goal in Data & plot builder before downloading a goal report (it can't be generated for \"All goals\").",
            type = "warning", duration = 10
          )
          write_fallback_report(
            file, plan$ext,
            title = "Goal Report \u2014 Goal not selected",
            body_lines = c(
              "A goal report could not be generated because no specific goal was selected.",
              "",
              "In the dashboard, open **Report Mode**, pick a specific Goal in the",
              "**Data & plot builder** panel, then click **Download goal report** again."
            )
          )
          return()
        }
        
        # Guard: pandoc missing. Still deliver a real file.
        if (!isTRUE(plan$has_pandoc)) {
          showNotification(
            "Pandoc is not available on this R session, so the report cannot be rendered. Install pandoc or launch the app from RStudio.",
            type = "error", duration = 12
          )
          write_fallback_report(
            file, plan$ext,
            title = "Goal Report \u2014 Pandoc unavailable",
            body_lines = c(
              "The report could not be generated because Pandoc is not available on the R process running this app.",
              "",
              "Fixes:",
              "- Launch the dashboard from RStudio (RStudio bundles Pandoc).",
              "- Or install Pandoc and ensure it is on the PATH of the R session."
            )
          )
          return()
        }
        if (isTRUE(plan$fell_back)) {
          showNotification(
            "LaTeX is not installed, so PDF output is unavailable. Generating a Word (.docx) file instead. Run tinytex::install_tinytex() in R to enable PDF.",
            type = "warning", duration = 12
          )
        }
        
        yr_start <- if (!is.null(input$rep_years)) input$rep_years[[1]] else NULL
        yr_end   <- if (!is.null(input$rep_years)) input$rep_years[[2]] else NULL
        template <- here::here("internal", "report_template.Rmd")
        
        fmt <- if (identical(plan$final, "docx")) {
          rmarkdown::word_document(toc = TRUE)
        } else {
          rmarkdown::pdf_document(latex_engine = "xelatex", toc = TRUE, toc_depth = 2)
        }
        tmp_dir <- tempfile(pattern = "internal-goal-report-")
        dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
        out_name <- paste0("report_",
                           format(Sys.time(), "%Y%m%d%H%M%S"),
                           ".", plan$ext)
        tmp_out <- file.path(tmp_dir, out_name)
        
        result <- tryCatch(
          rmarkdown::render(
            input             = template,
            output_format     = fmt,
            output_file       = out_name,
            output_dir        = tmp_dir,
            intermediates_dir = tmp_dir,
            knit_root_dir     = tmp_dir,
            params = list(
              goal     = as.character(plan$goal),
              yr_start = yr_start,
              yr_end   = yr_end
            ),
            envir = new.env(parent = globalenv()),
            quiet = TRUE
          ),
          error = function(e) {
            showNotification(
              paste("Report generation failed:", conditionMessage(e)),
              type = "error", duration = 12
            )
            message("[goal-report] render ERROR: ", conditionMessage(e))
            NULL
          }
        )
        message("[goal-report] render result=", result %||% "NULL",
                " tmp-dir-listing=",
                paste(list.files(tmp_dir), collapse = ", "))
        
        built <- if (!is.null(result) && file.exists(result)) result else tmp_out
        if (is.null(built) || !file.exists(built)) {
          write_fallback_report(
            file, plan$ext,
            title = "Goal Report \u2014 Render failed",
            body_lines = c(
              "The report could not be rendered. If PDF was selected, try Word (.docx).",
              "Check the server log for the underlying pandoc/rmarkdown error."
            )
          )
          return()
        }
        
        # Verify magic bytes before shipping the file so we never send HTML
        # labelled as .pdf / .docx to the browser.
        sig <- tryCatch(readBin(built, "raw", n = 4L), error = function(e) raw())
        sig_hex <- if (length(sig) > 0L) {
          paste(sprintf("%02x", as.integer(sig)), collapse = " ")
        } else {
          "<none>"
        }
        ok_pdf  <- length(sig) >= 4L &&
                   identical(rawToChar(sig[1:4]), "%PDF")
        ok_docx <- length(sig) >= 2L &&
                   sig[1] == as.raw(0x50) && sig[2] == as.raw(0x4B)
        want_pdf  <- identical(plan$final, "pdf")
        want_docx <- identical(plan$final, "docx")
        good <- (want_pdf && ok_pdf) || (want_docx && ok_docx)
        message("[goal-report] built=", built, " size=", file.size(built),
                " sig=", sig_hex, " want=", plan$final, " good=", good)
        if (!good) {
          showNotification(
            sprintf(
              "Rendered file was not a valid %s (got magic bytes %s). Try the other format.",
              toupper(plan$ext), sig_hex
            ),
            type = "error", duration = 12
          )
          write_fallback_report(
            file, plan$ext,
            title = "Goal Report \u2014 Unexpected output",
            body_lines = c(
              "The renderer produced a file that does not match the requested format.",
              sprintf("First bytes: %s", sig_hex),
              "Try downloading in the other format (Word if you picked PDF, or vice versa)."
            )
          )
          return()
        }
        
        ok_copy <- tryCatch(
          isTRUE(file.copy(built, file, overwrite = TRUE)),
          error = function(e) {
            message("[goal-report] copy ERROR: ", conditionMessage(e))
            FALSE
          }
        )
        message("[goal-report] copied=", ok_copy, " -> ", file,
                " final-size=",
                if (file.exists(file)) tryCatch(file.size(file),
                                                error = function(e) -1)
                else -1)
        if (!ok_copy) {
          showNotification(
            "Report was generated but could not be copied to the download.",
            type = "error", duration = 8
          )
          write_fallback_report(
            file, plan$ext,
            title = "Goal Report \u2014 Copy failed",
            body_lines = c(
              "The report was generated at the temporary path but could not be copied to the download location.",
              paste("Temporary file:", built)
            )
          )
        }
      }
    )
    }  # end if (FALSE) — dead legacy goal-report-builder block

    observe({
      df <- explorer_filtered()
      r  <- meta_row()
      req(df, nrow(df) > 0)
      pk <- input$plot_kind %||% "auto"
      extra <- c(
        paste0("Plot: ", pk),
        paste0("X: ", input$explorer_x %||% ""),
        paste0("Y: ", input$explorer_y %||% ""),
        if (!is.null(year_numeric(df)) && !is.null(input$year_rng))
          paste0("Year filter: ", input$year_rng[1], "\u2013", input$year_rng[2])
      )
      cap <- build_report_caption(r, df, extra = extra)
      updateTextAreaInput(session, "caption_text", value = cap)
    })
    
    observeEvent(input$copy_cap, {
      session$sendCustomMessage("copyCaption", list(text = input$caption_text %||% ""))
    })
  })
}
