# R/utils_ui.R
library(shiny)
library(bslib)

# Bootstrap 5 theme (customize fonts/bootswatch as you like)
app_theme <- function() {
  bs_theme(
    version = 5,
    #base_font = font_google("Inter", local = TRUE),
    #heading_font = font_google("Inter", local = TRUE)
  )
}

# Top banner (logo left, centered title, logo right)
app_banner <- function(title, left_logo, right_logo) {
  tags$header(
    class = "app-banner d-flex align-items-center justify-content-between px-3 py-2",
    # Left logo
    tags$img(src = left_logo, class = "app-logo app-logo-left", alt = "Left logo"),
    # Title centered (flex-grow keeps it centered between images)
    tags$div(class = "app-title text-center flex-grow-1 fw-semibold", title),
    # Right logo
    tags$img(src = right_logo, class = "app-logo app-logo-right", alt = "Right logo")
  )
}

# CSS to style banner, sidebar
app_css <- function() {
  "
  /* --- General font stuff --- */
  /* Base paragraph font size */
  .tab-content p { font-size: 15px; margin-bottom: 0.1px;}

  /* Headers scaled relative to paragraph font size */
  .tab-content h1 { font-size: 1.6em; }
  .tab-content h2 { font-size: 1.5em; }
  .tab-content h3 { font-size: 1.4em; }
  .tab-content h4 { font-size: 1.3em; }
  .tab-content h5 { font-size: 1em; }
  .tab-content h6 { font-size: 0.9em; }

  /* --- Banner --- */
  .app-banner {
    background: #0B5258;
    border-bottom: 1px solid var(--bs-border-color);
  }
  .app-logo {
    height: 100px;            /* fixed height */
    object-fit: contain;
  }
  .app-title {
    color: white;
    font-size: 1.5rem;
  }

  /* --- Sidebar container padding (compact) --- */
  .bslib-sidebar {
    padding: .25rem .5rem;
  }

  /* --- Turn the radiogroup into a menu (works for ANY radio group inside the sidebar) --- */
  .bslib-sidebar .shiny-input-radiogroup .form-check {
    margin-bottom: .125rem;
  }

  /* Hide the radio dot but keep it focusable/accessible */
  .bslib-sidebar .shiny-input-radiogroup .form-check-input[type=radio] {
    position: absolute !important;
    opacity: 0 !important;
    width: 0 !important;
    height: 0 !important;
    margin: 0 !important;
    pointer-events: none !important;
  }

  /* Make the whole label look/act like a menu row */
  .bslib-sidebar .shiny-input-radiogroup .form-check-label {
    display: flex;
    align-items: center;
    gap: .5rem;
    padding: .375rem .5rem;
    border-radius: .375rem;
    cursor: pointer;
    user-select: none;
  }

  /* Icon sizing (we add class = 'sidebar-icon' on bsicons) */
  .bslib-sidebar .shiny-input-radiogroup .sidebar-icon {
    width: 1.05rem; height: 1.05rem;
    flex: 0 0 auto;
    opacity: .8;
  }

  /* Hover/focus/active states */
  .bslib-sidebar .shiny-input-radiogroup .form-check-label:hover {
    background: var(--bs-light);
  }
  .bslib-sidebar .shiny-input-radiogroup .form-check-input:focus + label {
    outline: 2px solid rgba(var(--bs-primary-rgb), .35);
    outline-offset: 2px;
  }
  .bslib-sidebar .shiny-input-radiogroup .form-check-input:checked + label {
    background: rgba(var(--bs-primary-rgb), .10);
    color: var(--bs-primary);
    font-weight: 600;
  }
  .bslib-sidebar .shiny-input-radiogroup .form-check-input:checked + label .sidebar-icon {
    opacity: 1;
    color: var(--bs-primary); /* bsicons uses currentColor */
  }

  /* --- Card spacing in main area --- */
  .p-3 .card { margin-bottom: 1rem; }

  /* Sidebar menu styling for link-based nav */
    .sidebar-menu .sidebar-link {
      display: flex; align-items: center; gap: .5rem;
      padding: .375rem .5rem; border-radius: .375rem;
      text-decoration: none; color: var(--bs-body-color);
    }
  .sidebar-menu .sidebar-link:hover { background: var(--bs-light); }
  .sidebar-menu .sidebar-link.active {
    background: rgba(var(--bs-primary-rgb), .10);
    color: var(--bs-primary); font-weight: 600;
  }
  .sidebar-menu .sidebar-icon {
    width: 1.05rem; height: 1.05rem; opacity: .85; flex: 0 0 auto;
  }
  .sidebar-menu .sidebar-link.active .sidebar-icon { color: var(--bs-primary); opacity: 1; }

  "

}

