library(shiny)
library(bs4Dash)

shinyApp(
  ui = dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
      socialBox(
        id = "socialbox",
        title = userBlock(
          image = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
          title = "Social Box",
          subtitle = "example-01.05.2018"
        ),
        actionButton("update_box", "Refresh"),
        "Some text here!",
        br(),
        br(),
        tabsetPanel(
          tabPanel(
            "News",
            attachmentBlock(
              image = "https://www.sammobile.com/wp-content/uploads/2017/11/Camel.png",
              title = "Test",
              href = "https://google.com",
              "This is the content"
            )
          ),
          tabPanel(
            "Messages",
            userMessages(
              width = 12,
              status = "danger",
              userMessage(
                author = "Alexander Pierce",
                date = "20 Jan 2:00 pm",
                image = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
                type = "received",
                "Is this template really for free? That's unbelievable!"
              ),
              userMessage(
                author = "Sarah Bullock",
                date = "23 Jan 2:05 pm",
                image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
                type = "sent",
                "You better believe it!"
              )
            )
          )
        ),
        lapply(X = 1:10, FUN = function(i) {
          boxComment(
            image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
            title = paste("Comment", i),
            date = "01.05.2018",
            paste0("The ", i, "-th comment")
          )
        }),
        footer = "The footer here!"
      )
    ),
    title = "Social Box"
  ),
  server = function(input, output) {
    observeEvent(input$update_box, {
      updateBox(
        "socialbox",
        action = "update",
        options = list(
          title = userBlock(
            image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
            title = "Social Box updated",
            subtitle = "today"
          )
        )
      )
    })
  }
)
