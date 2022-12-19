library(shiny)
library(bs4Dash)

shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(rightUi = userOutput("user")),
    sidebar = dashboardSidebar(),
    body = dashboardBody(),
    title = "User dropdown"
  ),
  server = function(input, output) {
    output$user <- renderUser({
      dashboardUser(
        name = "Divad Nojnarg", 
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg", 
        title = "bs4Dash",
        subtitle = "Author", 
        footer = p("The footer", class = "text-center"),
        fluidRow(
          dashboardUserItem(
            width = 6,
            "Item 1"
          ),
          dashboardUserItem(
            width = 6,
            "Item 2"
          )
        )
      )
    })
  }
)
