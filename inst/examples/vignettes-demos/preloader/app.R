library(shiny)
library(bs4Dash)
library(waiter)

shinyApp(
  ui = dashboardPage(
    preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
    header = dashboardHeader(),
    sidebar = dashboardSidebar(),
    body = dashboardBody(
      actionButton("reload", "Reload")
    ),
    title = "Preloader"
  ),
  server = function(input, output, session) {
    observeEvent(input$reload, {
      session$reload()
    })
  }
)
