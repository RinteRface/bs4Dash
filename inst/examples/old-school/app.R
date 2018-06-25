library(shiny)
library(bs4Dash)

shiny::shinyApp(
  ui = bs4DashPage(
    old_school = TRUE,
    navbar = bs4DashNavbar(),
    sidebar = bs4DashSidebar(
      skin = "light",
      bs4SidebarMenu(
        bs4SidebarHeader("Main content"),
        bs4SidebarMenuItem(
          "Old-school theme",
          tabName = "old-school",
          icon = "desktop"
        )
      )
    ),
    controlbar = bs4DashControlbar(
      skin = "light"
    ),
    footer = bs4DashFooter(),
    title = "Old-school theme",
    body = bs4DashBody(
      bs4TabItems(
        bs4TabItem(
          tabName = "old-school",
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