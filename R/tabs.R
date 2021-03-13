#' Create a tabsetPanel
#' 
#' Imported by \link{bs4TabCard} but can be used alone. This is a modified shiny::tabsetPanel,
#' to handle bootstrap 4. This function will be upgraded 
#' starting from shiny 1.7.0 (support Bootstrap 4 tabs). 
#'
#' @inheritParams shiny::tabsetPanel
#' @param type \describe{
#'   \item{`"tabs"`}{Standard tab look}
#'   \item{`"pills"`}{Selected tabs use the background fill color}
#' }
#' @param vertical Whether to displays tabs vertically. Default to FALSE.
#' @param side Tabs side: \code{"left" or "right"}.
#' @param .list In case of programmatically generated items. See example.
#' 
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'
#'  shinyApp(
#'    ui = dashboardPage(
#'     header = dashboardHeader(),
#'     sidebar = dashboardSidebar(),
#'     controlbar = dashboardControlbar(),
#'     footer = dashboardFooter(),
#'     title = "Bootstrap 4 tabsetPanel",
#'     body = dashboardBody(
#'      # manually inserted panels
#'      tabsetPanel(
#'       id = "tabcard",
#'       tabPanel(
#'        title = "Tab 1", 
#'        "Content 1"
#'       ),
#'       tabPanel(
#'        title = "Tab 2", 
#'        "Content 2"
#'       ),
#'       tabPanel(
#'        title = "Tab 3", 
#'        "Content 3"
#'       )
#'      ),
#'      
#'      br(), br(),
#'      # programmatically inserted panels
#'      tabsetPanel(
#'        id = "tabset",
#'        .list = lapply(1:3, function(i) {
#'          tabPanel(
#'            title = paste0("Tab", i), 
#'            active = FALSE,
#'            paste("Content", i)
#'          )
#'        })
#'       )
#'     )
#'    ),
#'    server = function(input, output) {}
#'  )
#'  
#'  # update tabsetPanel
#'  shinyApp(
#'   ui = dashboardPage(
#'    title = "updateTabsetPanel",
#'    header = dashboardHeader(),
#'    body = dashboardBody(
#'      tabsetPanel(
#'        id = "tabset1",
#'        selected = "Tab 2",
#'        tabPanel(
#'          title = "Tab 1", 
#'          numericInput("val", "Value:", 10, min = 1, max = 100),
#'          verbatimTextOutput("value")
#'        ),
#'        tabPanel(
#'          title = "Tab 2", 
#'          "Content 2"
#'        ),
#'        tabPanel(
#'          title = "Tab 3", 
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
#'      uiOutput("tabSetPanel2")
#'    ),
#'    sidebar = dashboardSidebar(
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
#'    controlbar = dashboardControlbar(collapsed = FALSE),
#'    footer = dashboardFooter()
#'  ),
#'  server = function(input, output, session) {
#'  
#'    output$tabSetPanel2 <- renderUI({
#'     tabsetPanel(
#'       id = "tabset2",
#'       tabPanel(
#'         title = "Tab 1", 
#'         p("Tab 1 ")
#'       ),
#'       tabPanel(
#'         title = "Tab 2", 
#'         p("Tab 2")
#'       ),
#'       tabPanel(
#'         title = "Tab 3", 
#'         p("Tab 3")
#'       )
#'     )
#'    })
#'    
#'    # update tabset1
#'    observeEvent(input$controller, {
#'      updateTabsetPanel(
#'        session, 
#'        inputId = "tabset1", 
#'        selected = paste("Tab", input$controller)
#'      )
#'    }, ignoreInit = TRUE)
#'    
#'    # update tabset 2
#'    observeEvent(input$controller2, {
#'      updateTabsetPanel(
#'        session, 
#'        inputId = "tabset2", 
#'        selected = paste("Tab", input$controller2)
#'      )
#'    }, ignoreInit = TRUE)
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
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
tabsetPanel <- function(..., id = NULL, selected = NULL, 
                        type = c("tabs", "pills"), position = NULL, 
                        vertical = FALSE, side = "left", .list = NULL) {
  
  items <- c(list(...), .list)
  type <- match.arg(type)
  
  # We run the Shiny tabsetPanel function, to edit it later. This
  # is to avoid to rewrite all internal functions...
  temp_tabset <- bs3_tabsetPanel(
    tabs = items,
    id = id,
    selected = selected,
    type = type,
    position = position
  )
  # Some edit below since Bootstrap 4 significantly changed the layout
  nav_items <- temp_tabset$children[[1]]$children[[1]]
  found_active <- FALSE
  bs4_nav_items <- lapply(nav_items, function(x) {
    if (!is.null(x$attribs$class)) {
      if (grep(x = x$attribs$class, pattern = "active")) {
        x$attribs$class <- NULL
        found_active <- TRUE
      }
    }
    x$attribs$class <- if (is.null(x$attribs$class)) {
      "nav-item"
    } else {
      paste("nav-item",  x$attribs$class)
    }
    x$children[[1]]$attribs$class <- if (found_active) {
      "nav-link active"
    } else {
      "nav-link"
    }
    x
  })
  
  # replace href by data-target to avoid the shiny-server base href issue
  # (unable to switch between tabs)
  bs4_nav_items <- lapply(bs4_nav_items, function(item) {
    if (item$attribs$class == "nav-item dropdown") {
      item$children[[2]]$children[[1]] <- lapply(item$children[[2]]$children[[1]], function(subitem) {
        subitem$attribs$`data-target` <- subitem$attribs$href
        subitem$attribs$href <- "#"
        subitem
      })
    } else {
      item$children[[1]]$attribs$`data-target` <- item$children[[1]]$attribs$href
      item$children[[1]]$attribs$href <- "#"
    }
    item
  })
  
  # vertical layout
  if (vertical) {
    temp_tabset$children[[1]]$attribs$class <- paste0(
      temp_tabset$children[[1]]$attribs$class,
      " flex-column"
    )
    temp_tabset$children[[1]]$attribs$`aria-orientation` <- "vertical"
  }
  
  temp_tabset$children[[1]]$children[[1]] <- bs4_nav_items
  
  if (vertical) {
    
    tabsetMenu <- temp_tabset$children[[1]]
    tabsetContent <- temp_tabset$children[[2]]
    
    if (side == "left") {
      shiny::tagList(
        shiny::column(width = 2, tabsetMenu),
        shiny::column(width = 10, tabsetContent)
      )
    } else {
      shiny::tagList(
        shiny::column(width = 10, tabsetContent),
        shiny::column(width = 2, tabsetMenu)
      )
    }
  } else {
    temp_tabset
  }
  
}





#' Insert a \link{tabPanel} in a \link{tabsetPanel}
#'
#' @param inputId  \link{tabsetPanel} id.
#' @param tab \link{tabPanel} to insert.
#' @param target \link{tabPanel} after of before which the new tab will be inserted.
#' @param position Insert before or after: \code{c("before", "after")}.
#' @param select Whether to select the newly inserted tab. FALSE by default.
#' @param session Shiny session object.
#' 
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     header = dashboardHeader(),
#'     sidebar = dashboardSidebar(),
#'     controlbar = dashboardControlbar(),
#'     footer = dashboardFooter(),
#'     title = "Handle tabs",
#'     body = dashboardBody(
#'       actionButton("add", "Add 'Dynamic' tab"),
#'       actionButton("remove", "Remove 'Foo' tab"),
#'       actionButton("hideTab", "Hide 'Foo' tab"),
#'       actionButton("showTab", "Show 'Foo' tab"),
#'       br(), br(),
#'       tabBox(
#'         id = "tabs",
#'         title = "A card with tabs",
#'         selected = "Bar",
#'         status = "primary",
#'         solidHeader = FALSE, 
#'         type = "tabs",
#'         tabPanel("Hello", "This is the hello tab"),
#'         tabPanel("Foo", "This is the foo tab"),
#'         tabPanel("Bar", "This is the bar tab")
#'       )
#'     )
#'   ),
#'   server = function(input, output, session) {
#'     observeEvent(input$add, {
#'       insertTab(
#'         inputId = "tabs",
#'         tabPanel("Dynamic", "This a dynamically-added tab"),
#'         target = "Bar",
#'         select = TRUE
#'       )
#'     })
#'     
#'     observeEvent(input$remove, {
#'       removeTab(inputId = "tabs", target = "Foo")
#'     })
#'     
#'     observeEvent(input$hideTab, {
#'       hideTab(inputId = "tabs", target = "Foo")
#'     })
#'     
#'     observeEvent(input$showTab, {
#'       showTab(inputId = "tabs", target = "Foo")
#'     })
#'   }
#'  )
#' }
insertTab <- function(inputId, tab, target, position = c("before", "after"),
                         select = FALSE, session = shiny::getDefaultReactiveDomain()) {
  
  force(target)
  force(select)
  position <- match.arg(position)
  inputId <- session$ns(inputId)
  item <- buildTabItem(
    "id", 
    "tsid", 
    TRUE, 
    divTag = tab, 
    textFilter = if (is.character(tab)) navbarMenuTextFilter else NULL
  )
  
  item$liTag$attribs$class <- "nav-item"
  item$liTag$children[[1]]$attribs$class <- "nav-link"
  
  callback <- function() {
    session$sendInsertTab(
      inputId = inputId, 
      liTag = processDeps(item$liTag, session), 
      divTag = processDeps(item$divTag, session), 
      menuName = NULL, 
      target = target, 
      position = position, 
      select = select
    )
  }
  
  session$onFlush(callback, once = TRUE)
  
}
