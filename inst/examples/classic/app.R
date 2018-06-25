library(shiny)
library(bs4Dash)

shiny::shinyApp(
  ui = bs4DashPage(
    navbar = bs4DashNavbar(),
    sidebar = bs4DashSidebar(
      skin = "light",
      bs4SidebarMenu(
        bs4SidebarHeader("Main content"),
        bs4SidebarMenuItem(
          "Classic theme",
          tabName = "classic",
          icon = "desktop"
        )
      )
    ),
    controlbar = bs4DashControlbar(
      skin = "light"
    ),
    footer = bs4DashFooter(),
    title = "Classic theme",
    body = bs4DashBody(
      bs4TabItems(
        bs4TabItem(
          tabName = "classic",
          fluidRow(
            bs4Box(
              height = "600px",
              title = "Box 1"
            ),
            bs4Box(
              height = "600px",
              title = "Box 2"
            )
          )
        )
      )
    )
  ),
  server = function(input, output) {}
)