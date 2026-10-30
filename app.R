library(shiny)
library(shinymanager)

if (file.exists(".Renviron")) readRenviron(".Renviron")

source("internal/modules/helpers.R")

# Internal dashboard (Neon-backed data + auth)
source("internal/app.R")
internal_server <- server
dashboard_ui <- ui

login_brand_ui <- tags$div(
  class = "sfg-login-brand text-center",
  tags$span(class = "badge sfg-login-badge mb-2", "Internal dashboard"),
  tags$span(class = "sfg-login-icon", icon("seedling")),
  tags$h1("Success for Geneva"),
  tags$p(
    class = "sfg-login-lead",
    "This is the ",
    tags$strong("internal staff dashboard"),
    " for managing community data, datasets, and reports."
  ),
  tags$p(
    class = "sfg-login-sub",
    "Not the public website. Sign in with your organization account to continue."
  )
)

login_footer_ui <- tags$div(
  class = "sfg-login-footer",
  tags$p(class = "mb-1", "Authorized SFG staff and partners only."),
  tags$p(class = "mb-0", "Need access? Contact a dashboard administrator.")
)

ui <- secure_app(
  dashboard_ui,
  enable_admin = FALSE,
  head_auth = list(
    tags$link(rel = "stylesheet", href = "login-custom.css"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap"
    ),
    tags$script(HTML("
      (function () {
        function authInputsReady() {
          var user = document.getElementById('auth-user_id');
          var pwd = document.getElementById('auth-user_pwd');
          var btn = document.getElementById('auth-go_auth');
          if (!user || !pwd || !btn) return;
          var ok = user.value.trim().length > 0 && pwd.value.length > 0;
          btn.disabled = !ok;
        }
        document.addEventListener('input', function (e) {
          if (e.target && (e.target.id === 'auth-user_id' || e.target.id === 'auth-user_pwd')) {
            authInputsReady();
          }
        });
        document.addEventListener('shiny:connected', function () {
          authInputsReady();
          setTimeout(authInputsReady, 300);
          setTimeout(authInputsReady, 1000);
        });
        document.addEventListener('click', function (e) {
          var btn = e.target && e.target.closest ? e.target.closest('#auth-go_auth') : null;
          if (!btn) return;
          btn.disabled = true;
          setTimeout(function () { authInputsReady(); }, 2500);
        }, true);
      })();
    "))
  ),
  tags_top = login_brand_ui,
  tags_bottom = login_footer_ui
)

server <- function(input, output, session) {
  if (use_neon_db()) {
    tryCatch({
      con <- get_db_con()
      DBI::dbGetQuery(con, "SELECT 1 AS ok")
    }, error = function(e) {
      message("[auth] DB prewarm failed: ", conditionMessage(e))
      db_disconnect_all()
    })
  }

  res_auth <- secure_server(
    check_credentials = auth_check_credentials
  )

  observe({
    u <- trimws(as.character(res_auth$user %||% ""))
    if (nzchar(u)) {
      set_app_user(
        u,
        is_admin = res_auth$admin,
        role = res_auth$role %||% if (isTRUE(res_auth$admin)) "admin" else "viewer"
      )
    } else {
      set_app_user("", role = "viewer")
    }
  })

  internal_server(input, output, session, res_auth = res_auth)
}

shinyApp(ui, server)
