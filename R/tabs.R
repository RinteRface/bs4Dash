#' Create a tabsetPanel
#' 
#' Imported by \link{bs4TabCard} but can be used alone. This is a modified shiny::tabsetPanel,
#' to handle bootstrap 4. 
#'
#' @inheritParams shiny::tabsetPanel
#' 
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'     navbar = bs4DashNavbar(),
#'     sidebar = bs4DashSidebar(),
#'     controlbar = bs4DashControlbar(),
#'     footer = bs4DashFooter(),
#'     title = "test",
#'     body = bs4DashBody(
#'      
#'      # manually inserted panels
#'      bs4TabsetPanel(
#'       id = "tabcard",
#'       side = "left",
#'       tabPanel(
#'        tabName = "Tab 1", 
#'        active = FALSE,
#'        "Content 1"
#'       ),
#'       tabPanel(
#'        tabName = "Tab 2", 
#'        active = TRUE,
#'        "Content 2"
#'       ),
#'       tabPanel(
#'        tabName = "Tab 3", 
#'        active = FALSE,
#'        "Content 3"
#'       )
#'      ),
#'      
#'      br(), br(),
#'      # programmatically inserted panels
#'      bs4TabsetPanel(
#'        id = "tabset",
#'        side = "left",
#'        .list = lapply(1:3, function(i) {
#'          bs4TabPanel(
#'            tabName = paste0("Tab", i), 
#'            active = FALSE,
#'            paste("Content", i)
#'          )
#'        })
#'       ),
#'       
#'       br(), br(),
#'       # vertical tabset
#'       bs4TabsetPanel(
#'        id = "verttabset",
#'        side = "left",
#'        vertical = TRUE,
#'        .list = lapply(1:3, function(i) {
#'          bs4TabPanel(
#'            tabName = paste0("Tab", i), 
#'            active = FALSE,
#'            paste("Content", i)
#'          )
#'        })
#'       )
#'     )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4TabsetPanel <- function(..., id = NULL, selected = NULL, 
                           type = c("tabs", "pills"), position = NULL) {
  type <- match.arg(type)
  
  # We run the Shiny tabsetPanel function, to edit it later. This
  # is to avoid to rewrite all internal functions...
  temp_tabset <- shiny::tabsetPanel(
    ...,
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
  
  temp_tabset$children[[1]]$children[[1]] <- bs4_nav_items
  temp_tabset
}




#' Update a \link{bs4TabsetPanel}
#'
#' @param session shiny session.
#' @param inputId \link{bs4TabsetPanel} unique id.
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
#'      bs4TabsetPanel(
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
#'      uiOutput("tabSetPanel2")
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
#'    output$tabSetPanel2 <- renderUI({
#'     bs4TabsetPanel(
#'       id = "tabset2",
#'       side = "left",
#'       bs4TabPanel(
#'         tabName = "Tab 1", 
#'         active = FALSE,
#'         p("Tab 1 ")
#'       ),
#'       bs4TabPanel(
#'         tabName = "Tab 2", 
#'         active = FALSE,
#'         p("Tab 2")
#'       ),
#'       bs4TabPanel(
#'         tabName = "Tab 3", 
#'         active = FALSE,
#'         p("Tab 3")
#'       )
#'     )
#'    })
#'    
#'    # update tabset1
#'    observeEvent(input$controller, {
#'      updatebs4TabSetPanel(
#'        session, 
#'        inputId = "tabset1", 
#'        selected = paste("Tab", input$controller)
#'      )
#'    }, ignoreInit = TRUE)
#'    
#'    # update tabset 2
#'    observeEvent(input$controller2, {
#'      updatebs4TabSetPanel(
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
updatebs4TabSetPanel <- function (session, inputId, selected = NULL) {
  message <- dropNulls(list(value = selected))
  # this functions is linked to the 
  # inst/bs4Dash/update-tabs.js function
  session$sendInputMessage(inputId, message = message)
}





#' Insert a \link{bs4TabPanel} in a \link{bs4TabsetPanel}
#'
#' @param inputId  \link{bs4TabsetPanel} id.
#' @param tab \link{bs4TabPanel} to insert.
#' @param target \link{bs4TabPanel} after of before which the new tab will be inserted.
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
#'  ui <-  bs4DashPage(
#'    sidebar_collapsed = T,
#'    sidebar = bs4DashSidebar(),
#'    bs4DashFooter(),
#'    body = bs4DashBody(
#'      actionButton("add1","ADD tabset 1"),
#'      bs4TabsetPanel(
#'        id = "tabset1", 
#'        side = "left",
#'        bs4TabPanel(
#'          tabName = "Tab 1",
#'          active = TRUE,
#'          p("Text 1"),
#'        ),
#'        bs4TabPanel(
#'          tabName = "Tab 2",
#'          active = FALSE,
#'          p("Text 2"),
#'        )
#'      ),
#'      actionButton("add2","ADD tabset 2"),
#'      bs4TabsetPanel(
#'        id = "tabset2", 
#'        side = "left",
#'        bs4TabPanel(
#'          tabName = "Tab 1",
#'          active = TRUE,
#'          p("Text 1"),
#'        ),
#'        bs4TabPanel(
#'          tabName = "Tab 2",
#'          active = FALSE,
#'          p("Text 2"),
#'        )
#'      )
#'    )
#'  )
#'  
#'  server <- function(input, output, session) {
#'    
#'    observeEvent(input$add1, {
#'      bs4InsertTab(
#'        inputId = "tabset1",
#'        bs4TabPanel(tabName = "Dynamic", "I am inserted"),
#'        target = "Tab 1",
#'        position = "after",
#'        select = FALSE
#'      )
#'    })
#'    
#'    observeEvent(input$add2, {
#'      bs4InsertTab(
#'        inputId = "tabset2",
#'        bs4TabPanel(tabName = "Dynamic", "I am inserted and active"),
#'        target = "Tab 1",
#'        position = "before",
#'        select = TRUE
#'      )
#'    })
#'    
#'  }
#'  shinyApp(ui, server)
#'  
#'  # with Datatable to test the Shiny.renderContent feature
#'  library(shiny)
#'  library(bs4Dash)
#'  library(DT)
#'  
#'  ui <-  bs4DashPage(
#'    sidebar_collapsed = T,
#'    sidebar = bs4DashSidebar(),
#'    bs4DashFooter(),
#'    body = bs4DashBody(
#'      actionButton("add", "Add 'Dynamic' tab"),
#'      bs4TabsetPanel(
#'        id = "tabset", 
#'        side = "left",
#'        bs4TabPanel(
#'          tabName = "default",
#'          "Tab 1"
#'        )
#'      )
#'    )
#'  )
#'  
#'  server <- function(input, output, session) {
#'    
#'    output$tbl = renderDT(
#'      iris, options = list(lengthChange = FALSE)
#'    )
#'    
#'    observeEvent(input$add, {
#'      bs4InsertTab(
#'        inputId = "tabset",
#'        bs4TabPanel(
#'          tabName = "DT", 
#'          dataTableOutput("tbl")
#'        ),
#'        target = "default",
#'        position = "after",
#'        select = TRUE
#'      )
#'    })
#'  }
#'  shinyApp(ui, server)
#' }
bs4InsertTab <- function(inputId, tab, target, position = c("before", "after"),
                         select = FALSE, session = shiny::getDefaultReactiveDomain()) {
  
  if (!(class(tab[[2]]) %in% c("shiny.tag" , "shiny.tag.list"))) stop("tab must be a shiny tag")
  
  ns <- inputId
  
  # we need to create a new id not to overlap with the updatebs4TabSetPanel id
  # prefix by insert_ makes sense
  inputId <- paste0("insert_", inputId)
  
  position <- match.arg(position)
  
  # create the corresponding tablink
  tabId <- gsub(" ", "", tab[[2]]$attribs$id, fixed = TRUE)
  
  tabLink <- shiny::tags$li(
    class = "nav-item",
    shiny::a(
      class = "nav-link",
      href = paste0("#", ns, "-", tabId),
      `data-toggle` = "tab",
      tab[[2]]$attribs$id
    )
  )
  tabLink <- force(tabLink)
  
  # prefix the tab id by the id of the wrapping tabsetpanel
  tab[[2]]$attribs$id <- paste0(ns, "-", tabId)
  tabId <- tab[[2]]$attribs$id
  
  # force to render shiny.tag and convert it to character
  # since text does not accept anything else
  tab <- force(tab[[2]])
  
  # remove all whitespace from the target name
  target <- gsub(" ", "", target, fixed = TRUE)
  
  # below, processDeps is necessary to make sure that
  # tab content render well. It is used in combination with
  # the Shiny.renderContent method on the js side.
  message <- dropNulls(
    list(
      value = processDeps(tab, session),
      id = tabId,
      link = processDeps(tabLink, session),
      target = target,
      position = position,
      select = tolower(select),
      ns = ns
    )
  )
  session$sendCustomMessage(type = inputId, message = message)
}



#' Remove a \link{bs4TabPanel} in a \link{bs4TabSetPanel}
#'
#' @param inputId  \link{bs4TabSetPanel} id.
#' @param target \link{bs4TabPanel} to remove.
#' @param session Shiny session object.
#' 
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  ui <-  bs4DashPage(
#'    sidebar_collapsed = TRUE,
#'    sidebar = bs4DashSidebar(),
#'    bs4DashFooter(),
#'    body = bs4DashBody(
#'      actionButton("remove1","Remove tab 1"),
#'      bs4TabSetPanel(
#'        id = "tabset1", 
#'        side = "left",
#'        bs4TabPanel(
#'          tabName = "Tab 1",
#'          active = TRUE,
#'          p("Text 1"),
#'        ),
#'        bs4TabPanel(
#'          tabName = "Tab 2",
#'          active = FALSE,
#'          p("Text 2"),
#'        )
#'      ),
#'      actionButton("remove2","Remove tab 2"),
#'      bs4TabSetPanel(
#'        id = "tabset2", 
#'        side = "left",
#'        bs4TabPanel(
#'          tabName = "Tab 1",
#'          active = TRUE,
#'          p("Text 1"),
#'        ),
#'        bs4TabPanel(
#'          tabName = "Tab 2",
#'          active = FALSE,
#'          p("Text 2"),
#'        )
#'      )
#'    )
#'  )
#'  
#'  server <- function(input, output, session) {
#'    
#'    observeEvent(input$remove1, {
#'      bs4RemoveTab(
#'        inputId = "tabset1",
#'        target = "Tab 1"
#'      )
#'    })
#'    
#'    observeEvent(input$remove2, {
#'      bs4RemoveTab(
#'        inputId = "tabset2",
#'        target = "Tab 2",
#'      )
#'    })
#'    
#'  }
#'  shinyApp(ui, server)
#' }
bs4RemoveTab <- function(inputId, target, session = shiny::getDefaultReactiveDomain()) {
  
  # tabsetpanel namespace
  ns <- inputId
  
  # we need to create a new id not to overlap with the updatebs4TabSetPanel id
  # prefix by remove_ makes sense
  inputId <- paste0("remove_", inputId)
  
  # remove all whitespace from the target name
  target <- gsub(" ", "", target, fixed = TRUE)
  
  message <- dropNulls(
    list(
      target = target,
      ns = ns
    )
  )
  session$sendCustomMessage(type = inputId, message = message)
  
}



#' Dynamically hide/show a bs4TabPanel
#'
#' @param inputId The id of the \link{bs4TabSetPanel} in which to find target.
#' @param target The value of the \link{bs4TabPanel} to be hidden/shown. 
#' @param select Should target be selected upon being shown?
#' @param session The shiny session within which to call this function.
#' 
#' @export
#' @rdname toggleTabs
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  ui <- bs4DashPage(
#'    body = bs4DashBody(
#'      bs4TabSetPanel(
#'        id = "tabs",
#'        side = "left",
#'        bs4TabPanel(
#'          tabName = "Tab 1", 
#'          active = FALSE,
#'          "Content 1"
#'        ),
#'        bs4TabPanel(
#'          tabName = "Tab 2", 
#'          active = TRUE,
#'          "Content 2"
#'        ),
#'        bs4TabPanel(
#'          tabName = "Tab 3", 
#'          active = FALSE,
#'          "Content 3"
#'        )
#'      ),
#'      br(),
#'      actionButton("hideTab", "Hide 'Tab 1' tab"),
#'      actionButton("showTab", "Show 'Tab ' tab"),
#'      actionButton("hideTab2", "Hide 'Tab 2'"),
#'      actionButton("showTab2", "Show 'Tab 2'")
#'    )
#'  )
#'  
#'  server <- function(input, output, session) {
#'    observeEvent(input$hideTab, {
#'      bs4HideTab(inputId = "tabs", target = "Tab 1")
#'    })
#'    
#'    observeEvent(input$showTab, {
#'      bs4ShowTab(inputId = "tabs", target = "Tab 1")
#'    })
#'    
#'    observeEvent(input$hideTab2, {
#'      bs4HideTab(inputId = "tabs", target = "Tab 2")
#'    })
#'    
#'    observeEvent(input$showTab2, {
#'      bs4ShowTab(inputId = "tabs", target = "Tab 2", select = TRUE)
#'    })
#'    
#'  }
#'  
#'  shinyApp(ui, server)
#' }
bs4HideTab <- function(inputId, target, session = shiny::getDefaultReactiveDomain()) {
  
  # tabsetpanel namespace
  ns <- inputId
  
  # we need to create a new id not to overlap with the updatebs4TabSetPanel id
  # prefix by hide_ makes sense
  inputId <- paste0("hide_", inputId)
  
  # remove all white spaces from the target name
  target <- gsub(" ", "", target, fixed = TRUE)
  
  message <- dropNulls(
    list(
      target = target,
      ns = ns
    )
  )
  session$sendCustomMessage(type = inputId, message = message)
}



#' @rdname toggleTabs
#' @export
bs4ShowTab <- function(inputId, target, select = FALSE,
                       session = shiny::getDefaultReactiveDomain()) {
  force(target)
  # show the tab if selected (we can do that on the R side)
  if (select) updatebs4TabSetPanel(session, inputId, selected = target)
  
  # tabsetpanel namespace
  ns <- inputId
  # we need to create a new id not to overlap with the updatebs4TabSetPanel id
  # prefix by show_ makes sense
  inputId <- paste0("show_", inputId)
  
  # remove all white spaces from the target name
  target <- gsub(" ", "", target, fixed = TRUE)
  
  message <- dropNulls(
    list(
      target = target,
      ns = ns
    )
  )
  session$sendCustomMessage(type = inputId, message = message)
}