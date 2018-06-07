if (interactive()) {
  library(shiny)
  library(shinyWidgets)

  shiny::shinyApp(
    ui = bs4DashPage(
      navbar = bs4DashNavbar(
        status = "primary",
        "I can write text in the navbar!"
      ),
      sidebar = bs4DashSidebar(
        title = "bs4Dash",
        brandColor = "primary",
        url = "http://www.google.fr",
        src = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
        elevation = 3,
        opacity = 0.2,
        bs4SidebarMenu(
          bs4SidebarMenuItemList(
            name = "Item List",
            icon = "gears",
            open = TRUE,
            active = TRUE,
            bs4SidebarMenuItem(
              name = "item 1",
              icon = "sliders",
              active = FALSE
            ),
            bs4SidebarMenuItem(
              name = "item 2",
              icon = "paint-brush",
              active = TRUE
            )
          ),
          bs4SidebarMenuItem(
            name = "item 3",
            icon = "paint-brush",
            active = FALSE
          )
        )
      ),
      controlbar = bs4DashControlbar(
        title = "My right sidebar",
        sliderInput("obs", "Number of observations:",
                    min = 0, max = 1000, value = 500
        )
      ),
      footer = bs4DashFooter(
        copyrights = "@DavidNojnarg",
        right_text = "2018"
      ),
      title = "test",
      plotOutput("distPlot")
    ),
    server = function(input, output) {
      output$distPlot <- renderPlot({
        hist(rnorm(input$obs))
      })
    }
  )
}
