# Add dashboard dependencies to a tag object
add_bs4Dash_deps <- function(tag, options) {
  
  # put all necessary ressources here
  adminLTE3_js <- "adminlte.min.js"
  adminLTE3_css <- "adminlte.min.css"
  bs4Dash_js <- "bs4Dash.js"
  bs4Dash_css <- "bs4Dash.css"
  jquery_ui_js <- "jquery-ui.min.js"
  bootstrap_js <- "bootstrap.bundle.min.js"
  fontawesome_css <- "https://use.fontawesome.com/releases/v5.0.13/css/"
  ionicons_css <- "https://unpkg.com/ionicons@4.4.2/dist/css/"
  google_fonts <- "https://fonts.googleapis.com/css?family=Source+Sans+Pro:300,400,400i,700"
  
  bs4Dash_deps <- list(
    # jquery UI deps for sortable elements
    htmltools::htmlDependency(
      name = "jquery-ui", 
      version = "1.12.1",
      src = c(file = system.file("jquery-ui-1.12.1", package = "bs4Dash")),
      script = jquery_ui_js
    ),
    # bootstrap deps
    htmltools::htmlDependency(
      name = "bootstrap", 
      version = "4.3.1",
      src = c(file = "bootstrap-4.3.1"),
      script = bootstrap_js,
      package = "bs4Dash"
    ),
    if (!is.null(options)) {
      # additional options (this needs to be loaded before shinydashboardPlus deps)
      htmltools::htmlDependency(
        "options",
        as.character(utils::packageVersion("bs4Dash")),
        src = c(file = "bs4Dash-2.0.0"),
        head = paste0(
          "<script>var AdminLTEOptions = ", 
          jsonlite::toJSON(
            options, 
            auto_unbox = TRUE,
            pretty = TRUE
          ),
          ";</script>"
        ),
        package = "bs4Dash"
      )
    },
    # adminLTE3 deps
    htmltools::htmlDependency(
      name = "AdminLTE3", 
      version = "3.1.0",
      src = c(file = "AdminLTE3-3.1.0"),
      script = adminLTE3_js,
      stylesheet = adminLTE3_css,
      package = "bs4Dash"
    ),
    # bs4Dash custom js/css (must come after adminlte, bootstrap 4)
    htmltools::htmlDependency(
      name = "bs4Dash",
      version = as.character(utils::packageVersion("bs4Dash")),
      src = c(file = "bs4Dash-2.0.0"),
      script = c(
        bs4Dash_js, 
        "leftSidebar.js", 
        "navbar.js", 
        "update-tabs.js", 
        "controlbar.js",
        "cards.js",
        "feedbacks.js"
      ),
      stylesheet = bs4Dash_css,
      package = "bs4Dash"
    ),
    # fontawesome
    htmltools::htmlDependency(
      name = "fontawesome",
      version = as.character(utils::packageVersion("bs4Dash")),
      src = c(href = fontawesome_css),
      stylesheet = "all.css"
    ),
    # ionicons
    htmltools::htmlDependency(
      name = "ionicons",
      version = "4.4.2",
      src = c(href = ionicons_css),
      stylesheet = "ionicons.min.css"
    ),
    # google fonts
    htmltools::htmlDependency(
      name = "googlefonts",
      version = as.character(utils::packageVersion("bs4Dash")),
      src = c(href = google_fonts),
      stylesheet = ""
    ),
    # glyphicons
    htmltools::htmlDependency(
      name = "glyphicons",
      version = as.character(utils::packageVersion("bs4Dash")),
      src = c(file = "glyphicons"),
      stylesheet = "glyphicons.min.css",
      package = "bs4Dash"
    )
  )
  tagList(tag, bs4Dash_deps)
}