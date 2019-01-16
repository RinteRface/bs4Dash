# Add an html dependency, without overwriting existing ones
appendDependencies <- function(x, value) {
  if (inherits(value, "html_dependency"))
    value <- list(value)
  
  old <- attr(x, "html_dependencies", TRUE)
  
  htmltools::htmlDependencies(x) <- c(old, value)
  x
}

# Add dashboard dependencies to a tag object
addDeps <- function(x, theme) {
  
  # put all necessary ressources here
  adminLTE3_js <- "adminlte.js"
  bs4Dash_js <- "bs4Dash.js"
  adminLTE3_css <- "adminlte.min.css"
  jquery_ui_js <- "jquery-ui.min.js"
  bootstrap_js <- "bootstrap.bundle.min.js"
  old_school_css <- "https://bootswatch.com/4/sketchy/"
  fontawesome_css <- "https://use.fontawesome.com/releases/v5.0.13/css/"
  ionicons_css <- "https://unpkg.com/ionicons@4.4.2/dist/css/"
  google_fonts <- "https://fonts.googleapis.com/css?family=Source+Sans+Pro:300,400,400i,700"
  
  dashboardDeps <- list(
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
      version = "4.1.0",
      src = c(file = system.file("bootstrap-4.1.0", package = "bs4Dash")),
      script = bootstrap_js
    ),
    # adminLTE3 deps
    htmltools::htmlDependency(
      name = "AdminLTE3", 
      version = "3.0.0",
      src = c(file = system.file("AdminLTE3-3.0.0", package = "bs4Dash")),
      script = adminLTE3_js,
      stylesheet = adminLTE3_css
    ),
    # bs4Dash custom js
    htmltools::htmlDependency(
      name = "bs4Dash",
      version = as.character(utils::packageVersion("bs4Dash")),
      src = c(file = system.file("bs4Dash-0.2.0", package = "bs4Dash")),
      script = bs4Dash_js
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
      version = as.character(utils::packageVersion("bs4Dash")),
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
    # old school skin
    if (theme) {
      htmltools::htmlDependency(
        name = "old_school",
        version = as.character(utils::packageVersion("bs4Dash")),
        src = c(href = old_school_css),
        stylesheet = "bootstrap.min.css"
      )
    }
  )
  appendDependencies(x, dashboardDeps)
}