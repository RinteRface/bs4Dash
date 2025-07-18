library(shiny)
library(bs4Dash)

shinyApp(
  ui = dashboardPage(
    scrollToTop = TRUE,
    header = dashboardHeader(),
    sidebar = dashboardSidebar(minified = TRUE, collapsed = TRUE),
    body = dashboardBody(
      lapply(1:20, box, width = 12, title = "box")
    ),
    controlbar = dashboardControlbar(),
    title = "Scroll to top"
  ),
  server = function(input, output) {}
)
