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
#'     bs4TabSetPanel(
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





#' Insert a \link{bs4TabPanel} in a \link{bs4TabSetPanel}
#'
#' @param inputId  \link{bs4TabSetPanel} id.
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
#'      actionButton("add2","ADD tabset 2"),
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
#'      bs4TabSetPanel(
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
#'          ),
#'          bs4TabItem(
#'            tabName = "tab4",
#'            "Nothing special here!"
#'          ),
#'          bs4TabItem(
#'            tabName = "tab5",
#'            "Tab 5"
#'          ),
#'          bs4TabItem(
#'            tabName = "tab6",
#'            "Tab 6"
#'          ),
#'          bs4TabItem(
#'            tabName = "tab7",
#'            "Tab 7"
#'          )
#'        )
#'      ),
#'      sidebar = bs4DashSidebar(
#'        skin = "light",
#'        inputId = "sidebarState",
#'        bs4SidebarMenu(
#'          id = "sidebar",
#'          bs4SidebarMenuItem(
#'            text = "Tab 1",
#'            tabName = "tab1",
#'            icon = "shuttle-van"
#'          ),
#'          bs4SidebarMenuItem(
#'            text = "Tab 2",
#'            tabName = "tab2",
#'            icon = "space-shuttle",
#'            selected = TRUE
#'          ),
#'          bs4SidebarMenuItem(
#'            text = "Item List 1",
#'            icon = "bars",
#'            startExpanded = TRUE,
#'            bs4SidebarMenuSubItem(
#'              text = "Item 3",
#'              tabName = "tab3",
#'              icon = "circle-thin"
#'            ),
#'            bs4SidebarMenuSubItem(
#'              text = "Item 4",
#'              tabName = "tab4",
#'              icon = "circle-thin"
#'            )
#'          ),
#'          bs4SidebarMenuItem(
#'            text = "Item List 2",
#'            icon = "bars",
#'            startExpanded = FALSE,
#'            bs4SidebarMenuSubItem(
#'              text = "Item 5",
#'              tabName = "tab5",
#'              icon = "circle-thin"
#'            ),
#'            bs4SidebarMenuSubItem(
#'              text = "Item 6",
#'              tabName = "tab6",
#'              icon = "circle-thin"
#'            )
#'          ),
#'          bs4SidebarMenuItem(
#'            text = "Tab 7",
#'            tabName = "tab7",
#'            icon = "home"
#'          )
#'        )
#'      ),
#'      controlbar = bs4DashControlbar(
#'        skin = "light",
#'        sliderInput(
#'          inputId = "controller",
#'          label = "Update the first tabset",
#'          min = 1,
#'          max = 6,
#'          value = 2
#'        )
#'      ),
#'      footer = bs4DashFooter()
#'    ),
#'    server = function(input, output, session) {
#'      observe(print(input$sidebarItemExpanded))
#'      observe(print(input$sidebar))
#'      
#'      # update tabset1
#'      observeEvent(input$controller, {
#'        updatebs4TabItems(
#'          session, 
#'          inputId = "sidebar", 
#'          selected = paste0("tab", input$controller)
#'        )
#'      }, ignoreInit = TRUE)
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





#' Programmatically toggle a bs4Card sidebar
#'
#' @param session Shiny session object.
#' @param inputId Card sidebar id.
#' 
#' @export
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = bs4DashPage(
#'     sidebar_collapsed = FALSE,
#'     controlbar_collapsed = TRUE,
#'     enable_preloader = FALSE,
#'     navbar = bs4DashNavbar(skin = "dark"),
#'     body = bs4DashBody(
#'       bs4Card(
#'         title = "Closable Box with gradient", 
#'         closable = TRUE, 
#'         width = 12,
#'         height = "500px",
#'         solidHeader = FALSE, 
#'         collapsible = TRUE,
#'         actionButton("update", "Toggle card sidebar"),
#'         sidebar = bs4CardSidebar(
#'           inputId = "mycardsidebar",
#'           p("Sidebar Content")
#'         )
#'       )
#'     ),
#'     sidebar = bs4DashSidebar(),
#'     controlbar = bs4DashControlbar(),
#'     footer = bs4DashFooter()
#'   ),
#'   server = function(input, output, session) {
#'     observe(print(input$mycardsidebar))
#'     
#'     observeEvent(input$update, {
#'       updatebs4CardSidebar(session, inputId = "mycardsidebar")
#'     })
#'     
#'   }
#'  )
#' }
updatebs4CardSidebar <- function(session, inputId) {
  session$sendInputMessage(inputId, NULL)
}