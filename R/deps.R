# Add dashboard dependencies to a tag object
add_bs4Dash_deps <- function(tag, options) {
  
  # put all necessary ressources here
  adminLTE3_js <- "adminlte.min.js"
  adminLTE3_css <- "adminlte.min.css"
  bs4Dash_css <- "bs4Dash.css"
  jquery_ui_js <- "jquery-ui.min.js"
  bootstrap_js <- "bootstrap.bundle.min.js"
  os_js <- "jquery.overlayScrollbars.min.js"
  os_css <- "OverlayScrollbars.min.css"
  fontawesome_css <- "https://use.fontawesome.com/releases/v5.0.13/css/"
  ionicons_css <- "https://unpkg.com/ionicons@4.4.2/dist/css/"
  google_fonts <- "https://fonts.googleapis.com/css?family=Source+Sans+Pro:300,400,400i,700"
  
  bs4Dash_version <- as.character(utils::packageVersion("bs4Dash"))
  
  bs4Dash_deps <- list(
    # jquery UI deps for sortable elements
    htmltools::htmlDependency(
      name = "jquery-ui", 
      version = "1.13.2",
      src = c(file = system.file("jquery-ui-1.13.2", package = "bs4Dash")),
      script = jquery_ui_js
    ),
    # bootstrap deps
    htmltools::htmlDependency(
      name = "bootstrap", 
      version = "4.5.2",
      src = c(file = "bootstrap-4.5.2"),
      script = bootstrap_js,
      package = "bs4Dash"
    ),
    # overlay scrollbars
    htmltools::htmlDependency(
      name = "os", 
      version = "1.13.0",
      src = c(file = "os-1.13.0"),
      script = os_js,
      stylesheet = os_css,
      package = "bs4Dash"
    ),
    # custom options
    if (!is.null(options)) {
      # additional options (this needs to be loaded before shinydashboardPlus deps)
      htmltools::htmlDependency(
        "options",
        bs4Dash_version,
        src = c(file = sprintf("bs4Dash-%s", bs4Dash_version)),
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
      version = bs4Dash_version,
      src = c(file = sprintf("bs4Dash-%s", bs4Dash_version)),
      script = if (getOption("shiny.minified", TRUE)) "bs4Dash.min.js" else "bs4Dash.js" ,
      stylesheet = bs4Dash_css,
      package = "bs4Dash"
    ),
    # fontawesome Commented since Shiny already loads it with icon...
    #htmltools::htmlDependency(
    #  name = "fontawesome",
    #  version = bs4Dash_version,
    #  src = c(href = fontawesome_css),
    #  stylesheet = "all.css"
    #),
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
      version = bs4Dash_version,
      src = c(href = google_fonts),
      stylesheet = ""
    ),
    # glyphicons
    htmltools::htmlDependency(
      name = "glyphicons",
      version = bs4Dash_version,
      src = c(file = "glyphicons"),
      stylesheet = "glyphicons.min.css",
      package = "bs4Dash"
    )
  )
  
  shiny::tagList(tag, bs4Dash_deps)
}
