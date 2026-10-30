# internal/app.R
# SFG Internal Dashboard — entry point.
# Run with: shiny::runApp("internal")

library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(here)
library(readr)
library(shinyjs)
library(rhandsontable)

source(here("internal", "modules", "helpers.R"))
source(here("pipeline", "fetcher_registry.R"))

# Load secrets: CENSUS_API_KEY, DATABASE_URL (Neon), etc.
if (!nzchar(Sys.getenv("CENSUS_API_KEY")) ||
    !nzchar(Sys.getenv("DATABASE_URL")) ||
    !nzchar(Sys.getenv("GROQ_API_KEY"))) {
  renviron_path <- here(".Renviron")
  if (file.exists(renviron_path)) {
    readRenviron(renviron_path)
  }
}
source(here("internal", "modules", "dataset_editor_module.R"))
source(here("internal", "modules", "overview_tab.R"))
source(here("internal", "modules", "system_tab.R"))
source(here("internal", "modules", "explorer_tab.R"))
source(here("internal", "modules", "settings_tab.R"))
source(here("internal", "modules", "assistant_knowledge.R"))
source(here("internal", "modules", "assistant_llm.R"))
source(here("internal", "modules", "assistant_module.R"))

if (use_neon_db()) {
  con <- get_db_con()
  neon_ensure_registry_columns(con)
  neon_ensure_users_table(con)
  neon_ensure_users_role_column(con)
  neon_ensure_dataset_backups_table(con)
  neon_seed_admin_user(con)
}

APP_VERSION <- "4.0"

dashboard_theme <- bs_theme(
  version      = 5,
  bootswatch   = "flatly",
  primary      = "#2C6E49",
  secondary    = "#4A4E69",
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter"),
  font_scale   = 0.95
)

# ── UI ────────────────────────────────────────────────────────────────────────
# Global head: keep all CSS here (avoid nested tags$head inside tab UIs, which can break layout).

ui <- tagList(
  tags$head(
    tags$style(HTML("
      /* Stuck modal backdrop when no modal is open */
      body:not(.modal-open) .modal-backdrop { display: none !important; }
      /* Navbar: bslib page_navbar puts id on <ul>; tab <a> often has NO .nav-link class */
      #main_nav {
        --bs-nav-link-color: rgba(255, 255, 255, 0.95);
        --bs-nav-link-hover-color: #ffffff;
        --bs-nav-underline-link-active-color: #ffffff;
        --bs-navbar-active-color: #ffffff;
      }
      #main_nav .navbar-brand,
      .navbar .navbar-brand {
        color: #ffffff !important;
        opacity: 1 !important;
      }
      /* Tab links: target <a> directly (matches .nav-link too if present) */
      #main_nav > li > a,
      #main_nav > li > a.nav-link,
      #main_nav .nav-link {
        color: #ffffff !important;
        opacity: 1 !important;
        -webkit-text-fill-color: #ffffff !important;
        background: transparent !important;
      }
      #main_nav > li > a:link,
      #main_nav > li > a:visited,
      #main_nav > li > a:hover,
      #main_nav > li > a:focus,
      #main_nav > li > a:active,
      #main_nav > li.active > a,
      #main_nav > li > a.active,
      #main_nav > li > a[aria-selected=\"true\"] {
        color: #ffffff !important;
        opacity: 1 !important;
        -webkit-text-fill-color: #ffffff !important;
        background: transparent !important;
      }
      #main_nav > li > a *,
      #main_nav > li > a:hover *,
      #main_nav > li > a:focus *,
      #main_nav > li > a:active *,
      #main_nav > li.active > a * {
        color: #ffffff !important;
        fill: #ffffff !important;
        -webkit-text-fill-color: #ffffff !important;
      }
      /* BETA badge must stay readable (not forced to white on yellow) */
      #main_nav > li > a .badge {
        color: #212529 !important;
        -webkit-text-fill-color: #212529 !important;
      }
      #main_nav > li > a:focus-visible {
        outline: 2px solid rgba(255,255,255,0.75);
        outline-offset: 1px;
      }
      /* Overview: bslib layout_sidebar (collapsible Goals) */
      .overview-sidebar-root .bslib-sidebar-layout { min-height: 72vh; }
      .overview-sidebar-root .overview-goals-sidebar,
      .overview-sidebar-root .bslib-sidebar-layout > .sidebar,
      .overview-sidebar-root [data-bslib-sidebar-role=\"sidebar\"] {
        background-color: #f0f4f1 !important;
        border-color: #dee2e6 !important;
      }
      .overview-sidebar-root .sidebar-title,
      .overview-sidebar-root .bslib-sidebar-title {
        font-weight: 700;
        color: #1f3a28;
      }
      .goal-nav-item {
        cursor: pointer;
        padding: 10px 12px;
        margin-bottom: 4px;
        border-radius: 6px;
        border-left: 4px solid transparent;
        background: transparent;
        transition: background 0.15s ease;
      }
      .goal-nav-item:hover { background: rgba(44, 110, 73, 0.08); }
      .goal-nav-item.selected {
        border-left-color: #2C6E49;
        background: #e8f0ea;
      }
      .goal-nav-label {
        font-size: 0.88rem;
        font-weight: 600;
        color: #1f3a28;
        line-height: 1.35;
      }
      .goal-nav-meta {
        color: #5a6570;
        font-size: 0.8rem;
      }
      .goal-nav-progress-track {
        height: 4px;
        background: #dee2e6;
        border-radius: 2px;
        margin-top: 6px;
      }
      .goal-nav-progress-bar {
        height: 4px;
        border-radius: 2px;
        transition: width 0.4s;
      }
      .dashboard-page-title {
        color: #1f3a28;
        font-weight: 600;
      }
      .overview-card-title {
        font-weight: 600;
        font-size: 0.95rem;
        color: var(--bs-body-color);
        line-height: 1.35;
      }
      .overview-action-btn {
        min-height: 2rem;
        font-weight: 600;
        letter-spacing: 0.01em;
      }
      .overview-action-btn .fa,
      .overview-action-btn svg { opacity: 1; }
      .overview-icon-btn {
        min-width: 2.35rem;
        min-height: 2rem;
        color: var(--bs-body-color) !important;
        border-color: var(--bs-border-color) !important;
      }
      .overview-icon-btn .fa,
      .overview-icon-btn svg {
        font-size: 0.95rem;
        opacity: 0.92;
      }
      .overview-icon-btn:hover,
      .overview-icon-btn:focus-visible {
        background: rgba(44, 110, 73, 0.12) !important;
        border-color: #2C6E49 !important;
        color: #2C6E49 !important;
      }
      .overview-dataset-card .overview-card-body {
        padding-top: 0.5rem !important;
        padding-bottom: 0.5rem !important;
      }
      .overview-refresh-btn {
        min-width: 1.25rem;
        background: transparent !important;
        color: #5a6570 !important;
        --bs-btn-bg: transparent;
        --bs-btn-border-width: 0;
      }
      .overview-refresh-btn:hover,
      .overview-refresh-btn:focus-visible {
        background: rgba(44, 110, 73, 0.1) !important;
        color: #2C6E49 !important;
      }
      .overview-refresh-btn:focus { box-shadow: none !important; }
      .overview-dataset-card .card-header .overview-refresh-btn,
      .overview-dataset-card .card-header .overview-refresh-btn:active {
        background: transparent !important;
      }
      .overview-refresh-btn .fa,
      .overview-refresh-btn svg { color: inherit !important; }
      .overview-dl-icon-only {
        min-width: 2.25rem;
        padding-left: 0.5rem !important;
        padding-right: 0.5rem !important;
      }
      .overview-card-wrap > .card > .card-header { padding-left: 2.75rem; }
      .overview-card-wrap { position: relative; }
      .overview-archive-btn {
        position: absolute; top: 6px; left: 8px; right: auto; z-index: 3;
        padding: 2px 6px !important; border-radius: 50% !important;
      }
      .overview-add-card {
        min-height: 160px; border: 2px dashed #c5d4c9; border-radius: 8px;
        cursor: pointer; display: flex; align-items: center; justify-content: center;
        background: #fafcfb; transition: background 0.15s;
      }
      .overview-add-card:hover { background: #eef4f0; border-color: #2C6E49; }
      .overview-manage-only { display: none !important; }
      .overview-manage-mode .overview-manage-only { display: flex !important; }
      .overview-manage-mode .overview-archive-btn { display: inline-block !important; }
      .overview-archive-btn { display: none !important; }
      .overview-manage-hint { display: block; }
      .overview-manage-mode .overview-manage-hint { display: none; }
      .overview-main-col .overview-card-wrap > .card > .card-header { padding-left: 1rem; }
      .overview-manage-mode .overview-card-wrap > .card > .card-header { padding-left: 2.75rem; }
      .dashboard-loading-panel { min-height: 4rem; }
      .overview-sources-in-file summary::-webkit-details-marker { display: none; }
      .overview-sources-in-file summary::marker { content: ''; }
      /* Update-data editor frame & sticky header */
      .editor-table-frame {
        margin-bottom: 0.5rem;
        padding: 8px;
        background: var(--bs-body-bg);
        border-color: var(--bs-border-color) !important;
      }
      .editor-table-frame .handsontable .ht_clone_top,
      .editor-table-frame .handsontable .ht_clone_top_left_corner,
      .editor-table-frame .handsontable .ht_clone_top_inline_start_corner {
        z-index: 200 !important;
      }
      .editor-table-frame .handsontable thead th {
        background: #f0f4f1 !important;
        color: #1f3a28 !important;
        font-weight: 600;
      }
      .editor-dataset-details summary::-webkit-details-marker,
      .editor-sources-in-file summary::-webkit-details-marker { display: none; }
      .editor-dataset-details summary::marker,
      .editor-sources-in-file summary::marker { content: ''; }
      /* Status header strip */
      .dashboard-status-header {
        background: #f8f9fa;
        border-bottom: 1px solid #dee2e6;
        padding: 6px 20px;
        font-size: 0.82rem;
        color: #6c757d;
      }
      /* Navbar: theme toggle on green bar */
      .navbar-theme-toggle .form-check-input {
        cursor: pointer;
        border-color: rgba(255,255,255,0.55);
        background-color: rgba(255,255,255,0.2);
      }
      .navbar-theme-toggle .form-check-input:checked {
        background-color: #ffffff;
        border-color: #ffffff;
      }
      .navbar-theme-toggle .form-check-label { color: #ffffff; }
      /* Dark mode overrides */
      [data-bs-theme=\"dark\"] .dashboard-status-header {
        background: #1a1f1c !important;
        border-color: #3d4540 !important;
        color: #a8b0ab !important;
      }
      [data-bs-theme=\"dark\"] .overview-sidebar-root .overview-goals-sidebar,
      [data-bs-theme=\"dark\"] .overview-sidebar-root .bslib-sidebar-layout > .sidebar,
      [data-bs-theme=\"dark\"] .overview-sidebar-root [data-bslib-sidebar-role=\"sidebar\"] {
        background-color: #1a1f1c !important;
        border-color: #3d4540 !important;
        color: #c8d0cb;
      }
      [data-bs-theme=\"dark\"] .overview-sidebar-root .sidebar-title,
      [data-bs-theme=\"dark\"] .overview-sidebar-root .bslib-sidebar-title {
        color: #d4e8dc !important;
      }
      [data-bs-theme=\"dark\"] .goal-nav-item:hover {
        background: rgba(74, 158, 106, 0.12);
      }
      [data-bs-theme=\"dark\"] .goal-nav-item.selected {
        border-left-color: #4a9e6a;
        background: #252b28;
      }
      [data-bs-theme=\"dark\"] .goal-nav-label { color: #d4e8dc; }
      [data-bs-theme=\"dark\"] .goal-nav-meta { color: #9aa5a0; }
      [data-bs-theme=\"dark\"] .goal-nav-progress-track { background: #3d4540; }
      [data-bs-theme=\"dark\"] .dashboard-page-title { color: #d4e8dc; }
      [data-bs-theme=\"dark\"] .overview-icon-btn:hover,
      [data-bs-theme=\"dark\"] .overview-icon-btn:focus-visible {
        color: #8fd4a8 !important;
        border-color: #4a9e6a !important;
        background: rgba(74, 158, 106, 0.18) !important;
      }
      [data-bs-theme=\"dark\"] .overview-add-card {
        background: #1e2421;
        border-color: #3d5248;
      }
      [data-bs-theme=\"dark\"] .overview-add-card:hover {
        background: #252b28;
        border-color: #4a9e6a;
      }
      [data-bs-theme=\"dark\"] .overview-refresh-btn {
        color: #a8b0ab !important;
      }
      [data-bs-theme=\"dark\"] .overview-refresh-btn:hover,
      [data-bs-theme=\"dark\"] .overview-refresh-btn:focus-visible {
        background: rgba(74, 158, 106, 0.15) !important;
        color: #8fd4a8 !important;
      }
      [data-bs-theme=\"dark\"] .editor-table-frame .handsontable thead th {
        background: #2a3230 !important;
        color: #d4e8dc !important;
      }
      [data-bs-theme=\"dark\"] .card {
        border-color: #3d4540;
      }
      [data-bs-theme=\"dark\"] h4,
      [data-bs-theme=\"dark\"] h5 {
        color: #d4e8dc;
      }
      /* Update data modal — dark mode readability */
      [data-bs-theme=\"dark\"] .modal-content {
        background-color: #1e2421;
        color: #d4e8dc;
        border-color: #3d4540;
      }
      [data-bs-theme=\"dark\"] .modal-header,
      [data-bs-theme=\"dark\"] .modal-footer {
        border-color: #3d4540;
      }
      [data-bs-theme=\"dark\"] .update-data-panel .text-muted,
      [data-bs-theme=\"dark\"] .update-data-toolbar .text-muted {
        color: #a8b0ab !important;
      }
      [data-bs-theme=\"dark\"] .update-data-toolbar {
        border-color: #3d4540 !important;
      }
      [data-bs-theme=\"dark\"] .editor-details-form {
        background-color: #252b28 !important;
        border-color: #3d4540 !important;
      }
      [data-bs-theme=\"dark\"] .editor-sources-in-file summary,
      [data-bs-theme=\"dark\"] .editor-dataset-details summary {
        color: #c8d0cb;
      }
      [data-bs-theme=\"dark\"] .editor-sources-in-file a {
        color: #8fd4a8;
      }
      [data-bs-theme=\"dark\"] .handsontable,
      [data-bs-theme=\"dark\"] .handsontable .htCore,
      [data-bs-theme=\"dark\"] .handsontable .wtHolder {
        background-color: #1a1f1c !important;
        color: #d4e8dc !important;
      }
      [data-bs-theme=\"dark\"] .handsontable th,
      [data-bs-theme=\"dark\"] .handsontable thead th {
        background: #2a3230 !important;
        color: #d4e8dc !important;
        border-color: #3d4540 !important;
      }
      [data-bs-theme=\"dark\"] .handsontable td {
        background-color: #1a1f1c !important;
        color: #d4e8dc !important;
        border-color: #3d4540 !important;
      }
      [data-bs-theme=\"dark\"] .handsontable tbody tr:nth-child(even) td {
        background-color: #222822 !important;
      }
      [data-bs-theme=\"dark\"] .handsontable .htDimmed {
        color: #8a949c !important;
      }
      /* Handsontable selection: highlight active cell only, not whole row */
      .handsontable td.current {
        box-shadow: inset 0 0 0 2px #2C6E49 !important;
        background-color: rgba(44, 110, 73, 0.08) !important;
      }
      .handsontable td.area {
        background-color: rgba(44, 110, 73, 0.05) !important;
      }
      .handsontable tr.currentRow td {
        background-color: inherit !important;
      }
      .handsontable tr.currentRow td.current {
        background-color: rgba(44, 110, 73, 0.08) !important;
      }
      [data-bs-theme=\"dark\"] .handsontable td.current {
        box-shadow: inset 0 0 0 2px #4a9e6a !important;
        background-color: rgba(74, 158, 106, 0.14) !important;
        color: #d4e8dc !important;
      }
      [data-bs-theme=\"dark\"] .handsontable td.area {
        background-color: rgba(74, 158, 106, 0.06) !important;
        color: #d4e8dc !important;
      }
      [data-bs-theme=\"dark\"] .handsontable tr.currentRow td {
        background-color: inherit !important;
        color: #d4e8dc !important;
      }
      [data-bs-theme=\"dark\"] .handsontable tr.currentRow td.current {
        background-color: rgba(74, 158, 106, 0.14) !important;
      }
      [data-bs-theme=\"dark\"] .handsontable input,
      [data-bs-theme=\"dark\"] .handsontable textarea {
        background-color: #252b28 !important;
        color: #d4e8dc !important;
      }
      /* Update data modal — mode & revert buttons */
      .update-data-toolbar .update-data-mode-btn.btn-outline-secondary {
        color: var(--bs-body-color);
        border-color: var(--bs-border-color);
      }
      .update-data-toolbar .update-data-mode-btn.btn-outline-secondary:hover {
        border-color: #2C6E49;
        color: #2C6E49;
        background: rgba(44, 110, 73, 0.08);
      }
      .editor-revert-btn.btn-outline-secondary {
        color: var(--bs-body-color);
        border-color: var(--bs-border-color);
      }
      .editor-revert-btn.btn-outline-secondary:hover {
        border-color: #2C6E49;
        color: #2C6E49;
        background: rgba(44, 110, 73, 0.08);
      }
      .editor-backup-affordance .btn {
        line-height: 1.2;
      }
      .editor-restore-confirm {
        border-radius: 0.375rem;
      }
      [data-bs-theme=\"dark\"] .update-data-toolbar .update-data-mode-btn.btn-outline-secondary {
        color: #d4e8dc !important;
        border-color: #6a7570 !important;
        background: #252b28;
      }
      [data-bs-theme=\"dark\"] .update-data-toolbar .update-data-mode-btn.btn-outline-secondary:hover {
        color: #8fd4a8 !important;
        border-color: #4a9e6a !important;
        background: #2a3230 !important;
      }
      [data-bs-theme=\"dark\"] .update-data-toolbar .update-data-mode-btn.btn-primary {
        background: #3d9e6a !important;
        border-color: #3d9e6a !important;
        color: #ffffff !important;
      }
      [data-bs-theme=\"dark\"] .editor-revert-btn.btn-outline-secondary {
        color: #d4e8dc !important;
        border-color: #6a7570 !important;
        background: #252b28;
      }
      [data-bs-theme=\"dark\"] .editor-revert-btn.btn-outline-secondary:hover {
        color: #8fd4a8 !important;
        border-color: #4a9e6a !important;
        background: #2a3230 !important;
      }
      [data-bs-theme=\"dark\"] .editor-action-row .editor-scroll-hint {
        color: #9aa5a0 !important;
      }
      .backup-preview-overlay {
        position: fixed;
        inset: 0;
        z-index: 2000;
        background: rgba(0, 0, 0, 0.45);
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 1rem;
      }
      .backup-preview-dialog {
        max-width: 520px;
        width: 100%;
        max-height: 85vh;
        overflow: auto;
      }
      .backup-preview-dialog .table-responsive {
        max-height: 240px;
      }
      /* Floating dashboard helper */
      .sfg-assistant-root {
        position: fixed;
        right: 1.25rem;
        bottom: 1.25rem;
        z-index: 1080;
        pointer-events: auto;
      }
      .sfg-assistant-root > .action-button {
        margin: 0;
      }
      .sfg-assistant-fab,
      .sfg-assistant-fab.btn {
        width: 3.25rem;
        height: 3.25rem;
        border-radius: 50%;
        border: none;
        background: #2C6E49;
        color: #fff;
        box-shadow: 0 4px 14px rgba(0, 0, 0, 0.22);
        font-size: 1.35rem;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        padding: 0;
        line-height: 1;
      }
      .sfg-assistant-fab:hover,
      .sfg-assistant-fab.btn:hover {
        transform: scale(1.05);
        box-shadow: 0 6px 18px rgba(0, 0, 0, 0.28);
        background: #245a3c;
        color: #fff;
      }
      .sfg-assistant-close,
      .sfg-assistant-close.btn {
        background: transparent;
        border: none;
        color: #fff;
        opacity: 0.85;
        padding: 0.15rem 0.35rem;
        line-height: 1;
      }
      .sfg-assistant-close:hover,
      .sfg-assistant-close.btn:hover {
        opacity: 1;
        color: #fff;
        background: rgba(255, 255, 255, 0.12);
      }
      .sfg-assistant-panel {
        position: absolute;
        right: 0;
        bottom: 4rem;
        width: min(22rem, calc(100vw - 2rem));
        max-height: min(32rem, calc(100vh - 6rem));
        background: var(--bs-body-bg);
        border: 1px solid var(--bs-border-color);
        border-radius: 0.75rem;
        box-shadow: 0 8px 28px rgba(0, 0, 0, 0.18);
        display: flex;
        flex-direction: column;
        overflow: hidden;
      }
      .sfg-assistant-panel--hidden {
        display: none !important;
      }
      .sfg-assistant-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 0.65rem 0.85rem;
        background: #2C6E49;
        color: #fff;
      }
      .sfg-assistant-title {
        font-weight: 600;
        font-size: 0.95rem;
        display: flex;
        align-items: center;
        gap: 0.4rem;
      }
      .sfg-assistant-messages {
        flex: 1 1 auto;
        overflow-y: auto;
        padding: 0.75rem;
        display: flex;
        flex-direction: column;
        gap: 0.65rem;
        min-height: 10rem;
        max-height: 16rem;
      }
      .sfg-assistant-msg {
        display: flex;
        gap: 0.45rem;
        align-items: flex-start;
      }
      .sfg-assistant-msg--user {
        flex-direction: row-reverse;
      }
      .sfg-assistant-msg-avatar {
        width: 1.65rem;
        height: 1.65rem;
        border-radius: 50%;
        background: #e8f0ea;
        color: #2C6E49;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 0.75rem;
        flex-shrink: 0;
      }
      .sfg-assistant-msg-avatar--user {
        background: #4A4E69;
        color: #fff;
      }
      .sfg-assistant-msg-body {
        background: var(--bs-tertiary-bg);
        border-radius: 0.65rem;
        padding: 0.5rem 0.65rem;
        font-size: 0.82rem;
        line-height: 1.45;
        max-width: 85%;
      }
      .sfg-assistant-msg--user .sfg-assistant-msg-body {
        background: #2C6E49;
        color: #fff;
      }
      .sfg-assistant-suggestions {
        padding: 0 0.75rem 0.5rem;
        min-height: 3.25rem;
      }
      .sfg-assistant-suggestions-placeholder {
        min-height: 3.25rem;
      }
      .sfg-assistant-chip {
        font-size: 0.72rem;
        padding: 0.15rem 0.45rem;
        border-radius: 1rem;
      }
      .sfg-assistant-input-row {
        display: flex;
        gap: 0.4rem;
        align-items: center;
        padding: 0.55rem 0.75rem 0.65rem;
        border-top: 1px solid var(--bs-border-color);
        flex-shrink: 0;
        position: relative;
      }
      .sfg-assistant-input-row .form-group {
        flex: 1 1 0;
        min-width: 0;
        margin-bottom: 0;
        width: 100%;
      }
      .sfg-assistant-input-row input.form-control {
        font-size: 0.9rem;
        width: 100%;
        min-height: 2.375rem;
        box-sizing: border-box;
      }
      .sfg-assistant-input-row--waiting {
        pointer-events: none;
      }
      .sfg-assistant-waiting-prompt {
        flex: 1;
        display: flex;
        align-items: center;
        justify-content: center;
        min-height: 2.375rem;
        width: 100%;
        border: 1px solid var(--bs-border-color);
        border-radius: var(--bs-border-radius);
        background: var(--bs-secondary-bg);
        color: var(--bs-secondary-color);
        font-size: 0.88rem;
        font-weight: 500;
      }
      .sfg-assistant-thinking {
        color: var(--bs-secondary-color);
        font-style: italic;
      }
      .sfg-assistant-input-row .action-button {
        margin-bottom: 0;
        flex-shrink: 0;
      }
      .sfg-assistant-send {
        flex-shrink: 0;
        min-width: 2.375rem;
        min-height: 2.375rem;
      }
      [data-bs-theme=\"dark\"] .sfg-assistant-msg-body {
        background: #2a3230;
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('copyCaption', function(m) {
        if (m && m.text) {
          navigator.clipboard.writeText(m.text).catch(function() {});
        }
      });
      Shiny.addCustomMessageHandler('editorHotRevert', function(msg) {
        function matrixFromMsg() {
          var cols = msg.colnames || [];
          if (!cols.length) return [];
          var rows = msg.data;
          if (Array.isArray(rows)) {
            return rows.map(function(row) {
              return cols.map(function(c) {
                var v = row[c];
                return (v === null || v === undefined) ? '' : v;
              });
            });
          }
          var coldata = rows || {};
          var n = (coldata[cols[0]] && coldata[cols[0]].length) ? coldata[cols[0]].length : 0;
          var matrix = [];
          for (var i = 0; i < n; i++) {
            matrix.push(cols.map(function(c) {
              var v = coldata[c][i];
              return (v === null || v === undefined) ? '' : v;
            }));
          }
          return matrix;
        }
        function applyRevert(attempt) {
          var w = HTMLWidgets.find('#' + msg.widgetId);
          if (w && w.hotInstance) {
            var cols = msg.colnames || [];
            var matrix = matrixFromMsg();
            w.hotInstance.updateSettings({ colHeaders: cols });
            w.hotInstance.loadData(matrix);
            w.hotInstance.render();
            return;
          }
          if (attempt < 10) {
            setTimeout(function() { applyRevert(attempt + 1); }, 100);
          }
        }
        applyRevert(0);
      });
      Shiny.addCustomMessageHandler('sfgAssistantPanel', function(msg) {
        var el = document.getElementById(msg.panelId);
        if (!el) return;
        if (msg.open) {
          el.classList.remove('sfg-assistant-panel--hidden');
        } else {
          el.classList.add('sfg-assistant-panel--hidden');
        }
      });
      Shiny.addCustomMessageHandler('sfgAssistantScroll', function(msg) {
        var el = document.getElementById(msg.messagesId);
        if (el) el.scrollTop = el.scrollHeight;
      });
      $(document).on('keydown', '.sfg-assistant-input-row textarea', function(e) {
        if (e.key === 'Enter' && !e.shiftKey) {
          e.preventDefault();
          $(this).closest('.sfg-assistant-input-row').find('.sfg-assistant-send').click();
        }
      });
    "))
  ),
  page_navbar(
    id = "main_nav",
    title = tags$span(
      icon("seedling", class = "me-2", style = "opacity:0.95;"),
      "Success for Geneva — Internal Dashboard"
    ),
    theme = dashboard_theme,
    bg       = "#2C6E49",
    inverse  = TRUE,
    fillable = FALSE,
    
    useShinyjs(),
    
    header = div(
      class = "dashboard-status-header",
      uiOutput("header_status")
    ),
    
    nav_panel(
      title = tagList(icon("chart-line"), " Overview"),
      overviewUI("overview")
    ),
    
    nav_panel(
      title = tagList(icon("magnifying-glass-chart"), " Explore"),
      explorerUI("explorer")
    ),
    
    nav_panel(
      title = tagList(icon("gear"), " System"),
      systemUI("system")
    ),

    nav_spacer(),

    nav_item(
      div(
        class = "navbar-theme-toggle d-flex align-items-center px-2",
        input_dark_mode(id = "dark_mode", mode = "light")
      )
    ),

    nav_panel(
      title = tags$span(
        icon("user", class = "fa-fw"),
        title = "Settings",
        `aria-label` = "Settings"
      ),
      settingsUI("settings")
    ),
    
    nav_item(
      actionLink(
        "show_changelog",
        paste0("v", APP_VERSION),
        class = "text-white-50 small me-2",
        title = "What's new"
      )
    )
  ),
  assistantUI("assistant")
)

# ── SERVER ────────────────────────────────────────────────────────────────────

server <- function(input, output, session, res_auth = NULL) {
  
  observeEvent(input$show_changelog, {
    changelog_path <- here("CHANGELOG.md")
    body <- if (file.exists(changelog_path)) {
      raw <- readLines(changelog_path, warn = FALSE)
      # Show through first major section after header for a concise modal.
      tags$div(
        class = "small",
        HTML(paste(raw, collapse = "<br/>"))
      )
    } else {
      tags$p(class = "small", "No changelog file found.")
    }
    showModal(modalDialog(
      title = tagList(icon("sparkles"), " What's new in v", APP_VERSION),
      body,
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  }, ignoreInit = TRUE)

  meta         <- reactiveVal(NULL)
  upload_log   <- reactiveVal(NULL)
  meta_loading <- reactiveVal(TRUE)
  nav_dirty    <- reactiveVal(FALSE)

  observe({
    if (!is.null(res_auth)) {
      u <- trimws(as.character(res_auth$user %||% ""))
      req(nzchar(u))
    }
    meta_loading(TRUE)
    tryCatch({
      meta(load_meta())
      upload_log(load_upload_log())
    }, error = function(e) {
      message("[meta] initial load failed: ", conditionMessage(e))
      db_disconnect_all()
      meta(data.frame())
      upload_log(data.frame())
    })
    meta_loading(FALSE)
  })

  # Parse a single timestamp robustly: handles "T" separators (ISO-8601
  # pipeline logs), fractional seconds, and date-only strings.
  # NOTE: `tryFormats` on a *vector* picks the first format that parses ALL
  # elements. If one old ISO row fails %Y-%m-%d %H:%M:%S, the fallback
  # %Y-%m-%d silently parses every newer timestamp as midnight, which makes
  # same-day rows all look equal and `which.max` pick the first one.
  parse_ts_one <- function(s) {
    s <- trimws(as.character(s))
    if (!nzchar(s)) return(as.POSIXct(NA))
    s <- sub("T", " ", s, fixed = TRUE)   # ISO → space separator
    s <- sub("\\.[0-9]+$", "", s)         # drop fractional seconds
    for (fmt in c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
                  "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M",
                  "%Y-%m-%d", "%m/%d/%Y")) {
      ts <- as.POSIXct(s, format = fmt, tz = Sys.timezone())
      if (!is.na(ts)) return(ts)
    }
    as.POSIXct(NA)
  }
  parse_ts <- function(x) {
    do.call(c, lapply(x, parse_ts_one))
  }

  human_time_ago <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(as.character(x)))) {
      return("Never")
    }
    ts <- parse_ts_one(x)
    if (is.na(ts)) return(as.character(x))
    secs <- as.numeric(difftime(Sys.time(), ts, units = "secs"))
    if (is.na(secs) || secs < 0) return(format(ts, "%b %d, %Y %I:%M %p"))
    if (secs < 60) return("just now")
    mins <- floor(secs / 60)
    if (mins < 60) return(paste0(mins, " minute", if (mins == 1) "" else "s", " ago"))
    hrs <- floor(mins / 60)
    if (hrs < 24) return(paste0(hrs, " hour", if (hrs == 1) "" else "s", " ago"))
    days <- floor(hrs / 24)
    if (days == 1) return("yesterday")
    if (days < 7) return(paste(days, "days ago"))
    format(ts, "%b %d, %Y %I:%M %p")
  }
  
  # Tick every 30s so relative times ("3 minutes ago") stay fresh even when
  # the user is idle. Without this the header only updates when some other
  # reactive invalidates, so "Last activity: just now" never decays.
  header_tick <- reactiveTimer(30 * 1000, session = session)

    output$header_status <- renderUI({
    header_tick()
    role_badge <- if (!get_app_can_edit()) {
      tags$span(class = "badge text-bg-secondary ms-2", "View only")
    } else if (get_app_is_admin()) {
      tags$span(class = "badge text-bg-light text-dark ms-2", "Admin")
    } else {
      tags$span(class = "badge text-bg-light text-dark ms-2", "Editor")
    }
    if (isTRUE(meta_loading()) || is.null(meta())) {
      return(tagList(
        tags$span(class = "spinner-border spinner-border-sm me-2", role = "status"),
        "Loading dataset registry..."
      ))
    }
    upload_log()  # invalidate when any action writes the log
    m         <- meta()
    log       <- upload_log()
    if (is.null(log)) log <- data.frame()
    n_total   <- nrow(m)
    n_updated <- sum(!is.na(m$last_updated) & m$last_updated != "")
    if (nrow(log) > 0 && "timestamp" %in% names(log)) {
      ts_raw <- as.character(log$timestamp)
      ts_parsed <- parse_ts(ts_raw)
      if (any(!is.na(ts_parsed))) {
        last_idx <- which.max(ts_parsed)
        last_run <- ts_raw[[last_idx]]
      } else {
        last_run <- ts_raw[[length(ts_raw)]]
      }
    } else {
      last_run <- "Never"
    }
    last_human <- human_time_ago(last_run)
    last_display <- if (!identical(last_human, "Never") && nzchar(last_run)) {
      format_log_ts_local(last_run)[[1]]
    } else {
      last_run
    }
    tagList(
      tags$span(icon("circle-check", style = "color:#2C6E49"), " ",
                tags$strong(n_updated), " of ", tags$strong(n_total),
                " datasets have been updated"),
      tags$span(class = "mx-3", "|"),
      tags$span(
        icon("clock"), " Last activity: ", tags$strong(last_human),
        if (!identical(last_human, "Never")) tags$span(class = "text-muted", paste0(" (", last_display, ")"))
      ),
      role_badge
    )
  })
  overviewServer("overview", meta, upload_log, nav_dirty, meta_loading)
  systemServer("system", meta, upload_log, meta_loading, res_auth = res_auth)
  explorerServer("explorer", meta, meta_loading)
  settingsServer("settings", res_auth, upload_log)
  assistantServer("assistant", meta, meta_loading, nav_dirty,
                  active_tab = reactive(input$main_nav))
}