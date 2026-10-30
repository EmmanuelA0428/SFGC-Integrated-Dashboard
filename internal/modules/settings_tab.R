# internal/modules/settings_tab.R
# Personal account: display name and password (all logged-in users).

settingsUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "mb-3",
      h4(class = "dashboard-page-title mb-0",
         icon("user"), " Settings"),
      tags$p(
        class = "text-muted mb-0",
        style = "font-size:0.88rem;",
        "Manage your account. Organization user management is in ",
        tags$strong("System"), " (admins only)."
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      fillable   = FALSE,
      card(
        card_header(tagList(icon("id-card"), " Account")),
        card_body(
          textInput(ns("acct_username"), "Username", value = "", width = "100%"),
          textInput(ns("acct_display_name"), "Display name", width = "100%"),
          actionButton(
            ns("acct_save_profile"),
            tagList(icon("floppy-disk"), " Save display name"),
            class = "btn btn-primary btn-sm"
          )
        )
      ),
      card(
        card_header(tagList(icon("key"), " Password")),
        card_body(
          passwordInput(ns("acct_current_pwd"), "Current password", width = "100%"),
          passwordInput(ns("acct_new_pwd"), "New password", width = "100%"),
          passwordInput(ns("acct_confirm_pwd"), "Confirm new password", width = "100%"),
          tags$p(class = "small text-muted", "Minimum 8 characters."),
          actionButton(
            ns("acct_change_pwd"),
            tagList(icon("key"), " Change password"),
            class = "btn btn-outline-primary btn-sm"
          )
        )
      )
    )
  )
}

settingsServer <- function(id, res_auth, upload_log = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current_username <- reactive({
      trimws(as.character(res_auth$user %||% ""))
    })

    current_display_name <- reactive({
      trimws(as.character(res_auth$name %||% res_auth$user %||% ""))
    })

    observe({
      u <- current_username()
      if (!nzchar(u)) return()
      shinyjs::disable(ns("acct_username"))
      updateTextInput(session, "acct_username", value = u)
      updateTextInput(session, "acct_display_name", value = current_display_name())
    })

    observeEvent(input$acct_save_profile, {
      u <- current_username()
      req(nzchar(u))
      dn <- trimws(input$acct_display_name %||% "")
      if (!nzchar(dn)) {
        showNotification("Display name is required.", type = "warning", duration = 5)
        return()
      }
      res <- auth_update_display_name(u, dn)
      if (!isTRUE(res$ok)) {
        showNotification(res$error %||% "Could not update profile.", type = "error", duration = 8)
        return()
      }
      res_auth$name <- dn
      showNotification("Display name updated.", type = "message", duration = 4)
    })

    observeEvent(input$acct_change_pwd, {
      u <- current_username()
      req(nzchar(u))
      new_pwd <- input$acct_new_pwd %||% ""
      confirm <- input$acct_confirm_pwd %||% ""
      if (!identical(new_pwd, confirm)) {
        showNotification("New passwords do not match.", type = "warning", duration = 5)
        return()
      }
      res <- auth_change_password(
        u,
        new_password     = new_pwd,
        current_password = input$acct_current_pwd %||% "",
        require_current  = TRUE
      )
      if (!isTRUE(res$ok)) {
        showNotification(res$error %||% "Could not change password.", type = "error", duration = 8)
        return()
      }
      updateTextInput(session, "acct_current_pwd", value = "")
      updateTextInput(session, "acct_new_pwd", value = "")
      updateTextInput(session, "acct_confirm_pwd", value = "")
      append_upload_log("", "account", note = "password changed", user = u)
      if (!is.null(upload_log)) upload_log(load_upload_log())
      showNotification("Password changed successfully.", type = "message", duration = 5)
    })
  })
}
