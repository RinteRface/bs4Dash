library(shiny)
library(bs4Dash)
library(fresh)


# create the theme with a cyberpunk color palette
theme <- create_theme(
  bs4dash_color(
    green = "#3fff2d",
    blue = "#2635ff",
    red = "	#ff2b2b",
    yellow = "#feff6e",
    fuchsia = "#ff5bf8",
    navy = "#374c92",
    purple = "#615cbf",
    maroon = "#b659c9",
    lightblue = "#5691cc"
  ),
  bs4dash_sidebar_dark(
    bg = "#D8DEE9",
    hover_bg = "#81A1C1",
    color = "#2E3440"
  ),
  bs4dash_vars(
    content_bg = "#aaaaaa"
  )
)

# create tribble for box global config
box_config <- tibble::tribble(
  ~background, ~labelStatus,
  "danger", "warning",
  "purple", "success",
  "success", "primary",
  "warning", "danger",
  "fuchsia", "info"
)

# box factory function
box_factory <- function(background, labelStatus) {
  box(
    title = "Cyberpunk Box", 
    collapsible = TRUE, 
    background = background,
    height = "200px",
    label = boxLabel(1, labelStatus)
  )
}

# pmap magic
boxes <- purrr::pmap(box_config, box_factory)

shinyApp(
  ui = dashboardPage(
    freshTheme = theme,
    header = dashboardHeader(
      leftUi = dropdownMenu(
        type = "messages", 
        badgeStatus = "success",
        messageItem(
          from = "Support Team",
          message = "This is the content of a message.",
          time = "5 mins"
        ),
        messageItem(
          from = "Support Team",
          message = "This is the content of another message.",
          time = "2 hours"
        )
      )
    ),
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem("Item 1", badgeLabel = icon("heart"), badgeColor = "info"),
        menuItem("Item 2", badgeLabel = icon("poo"), badgeColor = "primary")
      )
    ),
    body = dashboardBody(boxes),
    controlbar = dashboardControlbar(),
    title = "Fresh theming"
  ),
  server = function(input, output) { }
)
