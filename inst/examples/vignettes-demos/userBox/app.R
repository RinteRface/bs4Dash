library(shiny)
library(bs4Dash)

shinyApp(
  ui = dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
      actionButton("update_box", "Update"),
      userBox(
        id = "userbox",
        title = userDescription(
          title = "Nadia Carmichael",
          subtitle = "lead Developer",
          type = 2,
          image = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",
        ),
        status = "primary",
        gradient = TRUE,
        background = "primary",
        boxToolSize = "xl",
        "Some text here!",
        footer = "The footer here!"
      )
    ),
    title = "userBox"
  ),
  server = function(input, output) {
    observeEvent(input$update_box, {
      updateBox(
        "userbox",
        action = "update",
        options = list(
          title = userDescription(
            title = "Jean Box",
            subtitle = "Developer",
            type = 1,
            image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
          ),
          status = "danger",
          background = NULL,
          solidHeader = FALSE,
          width = 4
        )
      )
    })
  }
)
