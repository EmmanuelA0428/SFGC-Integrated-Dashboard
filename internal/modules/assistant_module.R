# internal/modules/assistant_module.R
# Floating context-aware help assistant (Phase 1 — guidance only).

assistantUI <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "sfg-assistant-root",
    id = ns("root"),
    actionButton(
      ns("toggle"),
      label = NULL,
      icon = icon("circle-question"),
      class = "sfg-assistant-fab",
      title = "Dashboard helper"
    ),
    tags$div(
      class = "sfg-assistant-panel sfg-assistant-panel--hidden",
      id = ns("panel"),
      tags$div(
        class = "sfg-assistant-header",
        tags$div(
          class = "sfg-assistant-title",
          icon("seedling"),
          tags$span("SFG Helper")
        ),
        actionButton(
          ns("close"),
          label = NULL,
          icon = icon("xmark"),
          class = "sfg-assistant-close"
        )
      ),
      tags$div(
        class = "sfg-assistant-messages",
        id = ns("messages"),
        uiOutput(ns("message_list"))
      ),
      tags$div(class = "sfg-assistant-suggestions", uiOutput(ns("suggestions"))),
      uiOutput(ns("input_row_ui")),
      tags$script(HTML(sprintf(
        "(function() {
          var chipId = '%s';
          window.sfgAssistantAskChip = function(label) {
            Shiny.setInputValue(chipId, label, {priority: 'event'});
          };
          $(document).on('keydown', '.sfg-assistant-input-row input.form-control', function(e) {
            if (e.key === 'Enter') {
              e.preventDefault();
              Shiny.setInputValue(this.id, this.value, {priority: 'event'});
              $(this).closest('.sfg-assistant-input-row').find('.sfg-assistant-send').click();
            }
          });
        })();",
        ns("chip_query")
      )))
    )
  )
}

assistant_format_message_html <- function(text) {
  txt <- as.character(text %||% "")
  txt <- htmltools::htmlEscape(txt)
  txt <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", txt)
  txt <- gsub("\\n", "<br/>", txt)
  txt <- gsub("_([^_]+)_", "<em>\\1</em>", txt)
  HTML(txt)
}

assistant_finish_reply <- function(session, ns, thinking, chat_history, res, started_at,
                                  min_busy_secs = 0.45) {
  elapsed <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
  delay <- max(0, min_busy_secs - elapsed)

  finish <- function() {
    shiny::isolate({
      thinking(FALSE)
      h <- chat_history()
      h <- c(h, list(list(
        role = "assistant",
        content = res$text,
        source = res$source %||% "local"
      )))
      chat_history(h)
    })
    session$sendCustomMessage("sfgAssistantScroll", list(messagesId = ns("messages")))
  }

  if (delay > 0) {
    later::later(finish, delay = delay)
  } else {
    finish()
  }
}

assistantServer <- function(id, meta, meta_loading, nav_dirty, active_tab = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    panel_open <- reactiveVal(FALSE)
    chat_history <- reactiveVal(list())
    thinking <- reactiveVal(FALSE)
    warming_up <- reactiveVal(FALSE)

    assistant_busy <- reactive({
      isTRUE(thinking()) || isTRUE(warming_up())
    })

    run_warmup_async <- function() {
      if (!assistant_use_llm() || assistant_warmup_ready()) return()
      warming_up(TRUE)
      later::later(function() {
        assistant_warmup()
        shiny::isolate({
          warming_up(FALSE)
        })
      }, delay = 0.05)
    }

    get_ctx <- reactive({
      meta()
      meta_loading()
      nav_dirty()
      if (is.function(active_tab)) active_tab()
      overview_ctx <- if (is.function(session$userData$overview_context_rv)) {
        session$userData$overview_context_rv()
      } else {
        session$userData$overview_context %||% list()
      }
      explorer_ctx <- if (is.function(session$userData$explorer_context_rv)) {
        session$userData$explorer_context_rv()
      } else {
        session$userData$explorer_context %||% list()
      }
      tab_id <- if (is.function(active_tab)) active_tab() else NULL
      assistant_build_context(
        meta = meta(),
        meta_loading = meta_loading(),
        nav_dirty = nav_dirty(),
        tab_id = tab_id,
        overview_ctx = overview_ctx,
        explorer_ctx = explorer_ctx
      )
    })

    observe({
      if (assistant_use_llm() && !assistant_warmup_ready()) {
        run_warmup_async()
      }
    })

    welcome_message <- function(ctx) {
      paste0(
        "Hi! You're on **", ctx$current_tab, "** as **", ctx$user_role, "**.",
        if (isTRUE(ctx$update_panel_open)) {
          paste0(
            " You're working on **", ctx$dataset_display_name %||% ctx$editing_dataset,
            "** — ask about saving, modes, or backups."
          )
        } else {
          " Ask how to update data, explore charts, change your password, or what your role allows."
        }
      )
    }

    observe({
      if (length(chat_history()) == 0L && !isTRUE(meta_loading())) {
        ctx <- get_ctx()
        chat_history(list(list(
          role = "assistant",
          content = welcome_message(ctx),
          source = "welcome"
        )))
      }
    })

    toggle_panel <- function(open) {
      panel_open(open)
      session$sendCustomMessage("sfgAssistantPanel", list(
        panelId = ns("panel"),
        open = open
      ))
    }

    observeEvent(input$toggle, {
      open <- !panel_open()
      toggle_panel(open)
      if (open) run_warmup_async()
    }, ignoreInit = TRUE)

    observeEvent(input$close, {
      toggle_panel(FALSE)
    }, ignoreInit = TRUE)

    output$message_list <- renderUI({
      assistant_busy()
      chat_history()
      msgs <- chat_history()
      if (!length(msgs)) return(NULL)
      tagList(
        lapply(seq_along(msgs), function(i) {
          m <- msgs[[i]]
          role <- m$role %||% "assistant"
          div(
            class = paste("sfg-assistant-msg", paste0("sfg-assistant-msg--", role)),
            if (identical(role, "assistant")) {
              tagList(
                div(class = "sfg-assistant-msg-avatar", icon("seedling")),
                div(class = "sfg-assistant-msg-body",
                    assistant_format_message_html(m$content))
              )
            } else {
              tagList(
                div(class = "sfg-assistant-msg-body",
                    assistant_format_message_html(m$content)),
                div(class = "sfg-assistant-msg-avatar sfg-assistant-msg-avatar--user",
                    icon("user"))
              )
            }
          )
        }),
        if (isTRUE(thinking())) {
          div(
            class = "sfg-assistant-msg sfg-assistant-msg--assistant",
            div(class = "sfg-assistant-msg-avatar", icon("seedling")),
            div(
              class = "sfg-assistant-msg-body sfg-assistant-thinking",
              tags$span(class = "spinner-grow spinner-grow-sm me-2", role = "status"),
              "Waiting for answer…"
            )
          )
        } else if (isTRUE(warming_up())) {
          div(
            class = "sfg-assistant-msg sfg-assistant-msg--assistant",
            div(class = "sfg-assistant-msg-avatar", icon("seedling")),
            div(
              class = "sfg-assistant-msg-body sfg-assistant-thinking",
              tags$span(class = "spinner-grow spinner-grow-sm me-2", role = "status"),
              "Connecting…"
            )
          )
        }
      )
    })

    output$suggestions <- renderUI({
      get_ctx()
      assistant_busy()
      if (isTRUE(assistant_busy())) {
        return(tags$div(class = "sfg-assistant-suggestions-placeholder"))
      }
      chips <- assistant_suggested_prompts(get_ctx())
      if (!length(chips)) {
        return(tags$div(class = "sfg-assistant-suggestions-placeholder"))
      }
      tagList(
        tags$span(class = "sfg-assistant-suggestions-label small text-muted", "Try:"),
        div(
          class = "d-flex flex-wrap gap-1 mt-1",
          lapply(chips, function(label) {
            tags$button(
              type = "button",
              class = "btn btn-sm btn-outline-secondary sfg-assistant-chip",
              onclick = sprintf("sfgAssistantAskChip(%s);", shQuote(label, type = "cmd")),
              label
            )
          })
        )
      )
    })

    output$input_row_ui <- renderUI({
      assistant_busy()
      if (isTRUE(assistant_busy())) {
        label <- if (isTRUE(warming_up())) "Connecting…" else "Waiting for answer…"
        return(div(
          class = "sfg-assistant-input-row sfg-assistant-input-row--waiting",
          div(
            class = "sfg-assistant-waiting-prompt",
            tags$span(class = "spinner-grow spinner-grow-sm me-2", role = "status"),
            label
          )
        ))
      }
      div(
        class = "sfg-assistant-input-row",
        textInput(
          ns("prompt"),
          label = NULL,
          placeholder = "Ask anything about this dashboard…",
          width = "100%"
        ),
        actionButton(
          ns("send"),
          label = NULL,
          icon = icon("paper-plane"),
          class = "btn btn-primary btn-sm sfg-assistant-send"
        )
      )
    })

    run_query_async <- function(query) {
      if (isTRUE(assistant_busy())) {
        showNotification("Please wait for the current response…", type = "message", duration = 3)
        return()
      }
      query <- trimws(as.character(query %||% ""))
      if (!nzchar(query)) {
        showNotification("Type a question first.", type = "warning", duration = 3)
        return()
      }
      ctx <- isolate(get_ctx())
      hist <- chat_history()
      hist <- c(hist, list(list(role = "user", content = query)))
      chat_history(hist)
      thinking(TRUE)
      session$sendCustomMessage("sfgAssistantScroll", list(messagesId = ns("messages")))

      llm_hist <- Filter(function(m) m$role %in% c("user", "assistant"), hist)
      llm_hist <- lapply(llm_hist, function(m) {
        list(role = m$role, content = m$content)
      })
      if (length(llm_hist) > 10L) {
        llm_hist <- llm_hist[(length(llm_hist) - 9L):length(llm_hist)]
      }
      history_for_api <- head(llm_hist, -1L)
      started_at <- Sys.time()

      later::later(function() {
        res <- assistant_answer(query, ctx, history = history_for_api)
        assistant_finish_reply(
          session = session,
          ns = ns,
          thinking = thinking,
          chat_history = chat_history,
          res = res,
          started_at = started_at
        )
      }, delay = 0.12)
    }

    observeEvent(input$send, {
      run_query_async(input$prompt)
    }, ignoreInit = TRUE)

    observeEvent(input$chip_query, {
      q <- trimws(as.character(input$chip_query %||% ""))
      req(nzchar(q))
      run_query_async(q)
    }, ignoreInit = TRUE)

    outputOptions(output, "message_list", suspendWhenHidden = FALSE)
    outputOptions(output, "input_row_ui", suspendWhenHidden = FALSE)
  })
}
