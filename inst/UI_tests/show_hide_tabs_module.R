library(shiny)
library(bs4Dash)

testUI <- function(id){
  ns <- NS(id)
  tagList(
    bs4TabSetPanel(
      id = "tabs",
      side = "left",
      bs4TabPanel(
        tabName = "Tab 1", 
        active = FALSE,
        "Content 1"
      ),
      bs4TabPanel(
        tabName = "Tab 2", 
        active = TRUE,
        "Content 2"
      ),
      bs4TabPanel(
        tabName = "Tab 3", 
        active = FALSE,
        "Content 3"
      )
    ),
    br(),
    actionButton(ns("hideTab"), "Hide 'Tab 1' tab"),
    actionButton(ns("showTab"), "Show 'Tab ' tab"),
  )
}

test <- function(input, output, session){
  observeEvent(input$hideTab, {
    bs4HideTab(inputId = "tabs", target = "Tab 1")
  })
  
  observeEvent(input$showTab, {
    bs4ShowTab(inputId = "tabs", target = "Tab 1")
  })
}


ui <- bs4DashPage(
  body = bs4DashBody(testUI("test"))
)

server <- function(input, output, session) {
  callModule(test, "test")
}

shinyApp(ui, server)