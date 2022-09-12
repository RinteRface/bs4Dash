library(shiny)
library(bs4Dash)
shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(),
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem(
          text = "Item 1"
        ),
        menuItem(
          text = "Item 2"
        )
      )
    ),
    body = dashboardBody(),
    controlbar = dashboardControlbar(
      collapsed = FALSE,
      div(class = "p-3", skinSelector()),
      pinned = TRUE
    ),
    title = "Skin Selector"
  ),
  server = function(input, output) { }
)
