#' Update a \link{bs4TabSetPanel}
#'
#' @param session shiny session.
#' @param inputId \link{bs4TabSetPanel} unique id.
#' @param selected the tab to be selected.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  
#'  shinyApp(
#'   ui = bs4DashPage(
#'    sidebar_collapsed = FALSE,
#'    controlbar_collapsed = TRUE,
#'    enable_preloader = FALSE,
#'    loading_duration =  2,
#'    navbar = bs4DashNavbar(skin = "dark"),
#'    body = bs4DashBody(
#'      bs4TabSetPanel(
#'        id = "tabset1",
#'        side = "left",
#'        bs4TabPanel(
#'          tabName = "Tab 1", 
#'          active = FALSE,
#'          numericInput("val", "Value:", 10, min = 1, max = 100),
#'          verbatimTextOutput("value")
#'        ),
#'        bs4TabPanel(
#'          tabName = "Tab 2", 
#'          active = TRUE,
#'          "Content 2"
#'        ),
#'        bs4TabPanel(
#'          tabName = "Tab 3", 
#'          active = FALSE,
#'          checkboxGroupInput(
#'            inline = TRUE,
#'            "variable", "Variables to show:",
#'            c("Cylinders" = "cyl",
#'              "Transmission" = "am",
#'              "Gears" = "gear")
#'          ),
#'          tableOutput("data")
#'        )
#'      ),
#'      bs4TabSetPanel(
#'        id = "tabset2",
#'        side = "left",
#'        vertical = TRUE,
#'        bs4TabPanel(
#'          tabName = "Tab 1", 
#'          active = FALSE,
#'          sliderInput(
#'            "obs", 
#'            "Number of observations:",
#'            min = 0,
#'            max = 1000, 
#'            value = 500
#'          ),
#'          plotOutput("distPlot")
#'        ),
#'        bs4TabPanel(
#'          tabName = "Tab 2", 
#'          active = TRUE,
#'          radioButtons(
#'            inline = TRUE,
#'            "rb", "Choose one:",
#'            choiceNames = list(
#'              icon("calendar"),
#'              HTML("<p style='color:red;'>Red Text</p>"),
#'              "Normal text"
#'            ),
#'            choiceValues = list(
#'              "icon", "html", "text"
#'            )
#'          ),
#'          textOutput("txt")
#'        ),
#'        bs4TabPanel(
#'          tabName = "Tab 3", 
#'          active = FALSE,
#'          "Content 3"
#'        )
#'      )
#'    ),
#'    sidebar = bs4DashSidebar(
#'      skin = "light",
#'      sliderInput(
#'        inputId = "controller",
#'        label = "Update the first tabset",
#'        min = 1,
#'        max = 3,
#'        value = 2
#'      ),
#'      br(),
#'      sliderInput(
#'        inputId = "controller2",
#'        label = "Update the second tabset",
#'        min = 1,
#'        max = 3,
#'        value = 3
#'      )
#'    ),
#'    controlbar = bs4DashControlbar(skin = "light"),
#'    footer = bs4DashFooter()
#'  ),
#'  server = function(input, output, session) {
#'    
#'    # update tabset1
#'    observeEvent(input$controller, {
#'      updatebs4TabSetPanel(
#'        session, 
#'        inputId = "tabset1", 
#'        selected = input$controller
#'      )
#'    })
#'    
#'    # update tabset 2
#'    observeEvent(input$controller2, {
#'      updatebs4TabSetPanel(
#'        session, 
#'        inputId = "tabset2", 
#'        selected = input$controller2
#'      )
#'    })
#'    
#'    output$distPlot <- renderPlot({
#'      hist(rnorm(input$obs))
#'    })
#'    
#'    output$data <- renderTable({
#'      mtcars[, c("mpg", input$variable), drop = FALSE]
#'    }, rownames = TRUE)
#'    
#'    output$txt <- renderText({
#'      paste("You chose", input$rb)
#'    })
#'    
#'    output$value <- renderText({ input$val })
#'    
#'   }
#'  )
#' }
updatebs4TabSetPanel <- function (session, inputId, selected = NULL) {
  message <- dropNulls(list(value = selected))
  # this functions is linked to the 
  # inst/bs4Dash/update-tabs.js function
  session$sendCustomMessage(type = inputId, message = message)
}



#' Change the selected sidebar tab on the client
#'
#' This function controls the active tab of \code{\link{bs4TabItems}} from the
#' server. It behaves just like \code{\link{updatebs4TabSetPanel}}.
#'
#' @inheritParams updatebs4TabSetPanel
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'    ui = bs4DashPage(
#'      sidebar_collapsed = FALSE,
#'      controlbar_collapsed = TRUE,
#'      enable_preloader = FALSE,
#'      navbar = bs4DashNavbar(skin = "dark"),
#'      body = bs4DashBody(
#'        bs4TabItems(
#'          bs4TabItem(
#'            tabName = "tab1",
#'            sliderInput("obs", "Number of observations:",
#'                        min = 0, max = 1000, value = 500
#'            ),
#'            plotOutput("distPlot")
#'          ),
#'          bs4TabItem(
#'            tabName = "tab2",
#'            checkboxGroupInput("variable", "Variables to show:",
#'                               c("Cylinders" = "cyl",
#'                                 "Transmission" = "am",
#'                                 "Gears" = "gear")),
#'            tableOutput("data")
#'          ),
#'          bs4TabItem(
#'            tabName = "tab3",
#'            checkboxInput("val", "Some value", FALSE),
#'            textOutput("value")
#'          )
#'        )
#'      ),
#'      sidebar = bs4DashSidebar(
#'        skin = "light",
#'        bs4SidebarMenu(
#'          id = "sidebar",
#'          bs4SidebarMenuItem(
#'            text = "Tab 1",
#'            tabName = "tab1",
#'            icon = "card"
#'          ),
#'          bs4SidebarMenuItem(
#'            text = "Tab 2",
#'            tabName = "tab2",
#'            icon = "card"
#'          ),
#'          bs4SidebarMenuItem(
#'            text = "Tab 3",
#'            tabName = "tab3",
#'            icon = "card"
#'          )
#'        )
#'      ),
#'      controlbar = bs4DashControlbar(
#'        skin = "light",
#'        sliderInput(
#'          inputId = "controller",
#'          label = "Update the first tabset",
#'          min = 1,
#'          max = 3,
#'          value = 2
#'        )
#'      ),
#'      footer = bs4DashFooter()
#'    ),
#'    server = function(input, output, session) {
#'      
#'      # update tabset1
#'      observeEvent(input$controller, {
#'        updatebs4TabSetPanel(
#'          session, 
#'          inputId = "sidebar", 
#'          selected = input$controller
#'        )
#'      })
#'      
#'      output$distPlot <- renderPlot({
#'        hist(rnorm(input$obs))
#'      })
#'      
#'      output$data <- renderTable({
#'        mtcars[, c("mpg", input$variable), drop = FALSE]
#'      }, rownames = TRUE)
#'      
#'      output$value <- renderText({ input$val })
#'      
#'    }
#'  )
#' }
#' @export
updatebs4TabItems <- updatebs4TabSetPanel