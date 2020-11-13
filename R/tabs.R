#' Create a tabSetPanel
#' 
#' Imported by \link{bs4TabCard} but can be used alone.
#'
#' @param ... Slot for \link{bs4TabPanel}.
#' @param id Unique \link{bs4TabSetPanel} id. NULL by default. Set a value
#'  to get the currently selected tab.
#' @param side Side of the box the tabs should be on (\code{"left"} or
#'   \code{"right"}). Default to "left".
#' @param tabStatus The status of the tabs buttons over header. "primary", "secondary", "success", "warning", "danger", "white", "light", "dark", "transparent".
#'  NULL by default, "light" if status is set.   
#'  A vector is possible with a colour for each tab button
#' @param .list When elements are programmatically added, pass them here instead of in ...
#' @param vertical Whether to display tabs in a vertical mode. FALSE by default.
#' @param type TabPanel type: "tabs" or "pills". "pills" is the default if type is NULL.
#' 
#' @inheritParams bs4Card
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
#'      bs4TabSetPanel(
#'       id = "tabcard",
#'       side = "left",
#'       bs4TabPanel(
#'        tabName = "Tab 1", 
#'        active = FALSE,
#'        "Content 1"
#'       ),
#'       bs4TabPanel(
#'        tabName = "Tab 2", 
#'        active = TRUE,
#'        "Content 2"
#'       ),
#'       bs4TabPanel(
#'        tabName = "Tab 3", 
#'        active = FALSE,
#'        "Content 3"
#'       )
#'      ),
#'      
#'      br(), br(),
#'      # programmatically inserted panels
#'      bs4TabSetPanel(
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
#'       bs4TabSetPanel(
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
bs4TabSetPanel <- function(..., id = NULL, side = "left", status = NULL, tabStatus = NULL, 
                           .list = NULL, vertical = FALSE, type = NULL) {
  
  # pills are the default
  if (is.null(type)) type <- "pills"
  
  # to make tab ids in the namespace of the tabSetPanel
  if (is.null(id)) id <- paste0("tabs_", round(stats::runif(1, min = 0, max = 1e9)))
  ns <- shiny::NS(id)
  
  tabs <- c(list(...), .list)
  found_active <- FALSE
  selected <- NULL
  tabStatus <- if (!is.null(tabStatus)) rep(tabStatus, length.out = length(tabs))
  # handle tabs
  tabSetPanelItem <- lapply(seq_along(tabs), FUN = function(i) {
    
    tabName <- tabs[[i]][[1]]
    tabsTag <- tabs[[i]][[2]]
    
    tabClass <- tabsTag$attribs$class
    
    # make sure that if the user set 2 tabs active at the same time, 
    # only the first one is selected
    active <- sum(grep(x = tabClass, pattern = "active")) == 1
    if (!found_active) {
      if (active) {
        found_active <<- TRUE
        selected <<- i - 1
        # if no items are selected, we select the first
      } else {
        selected <<- 0
      }
      # do not allow more than 1 active item
    } else {
      if (active) {
        stop("Cannot set 2 active tabs at the same time.")
      }
    }
    
    id <- tabsTag$attribs$id
    
    shiny::tags$li(
      class = if (!is.null(status) & is.null(tabStatus[i])) {
        "nav-item bg-light"
      } else if (!is.null(tabStatus[i])) {
        paste0("nav-item bg-", tabStatus[i])
      } else {
        "nav-item"
      },
      shiny::tags$a(
        class = if (active) "nav-link active" else "nav-link",
        href = paste0("#", ns(id)),
        id = paste0(ns(id), "-tab"),
        `data-toggle` = "tab",
        role = "tab",
        `aria-controls` = ns(id),
        `aria-selected` = if (active) "true" else "false",
        tabName
      )
    )
  })
  
  tabSetCl <- "nav"
  tabSetCl <- if (type == "tabs") {
    paste0(tabSetCl, " nav-tabs")
  } else if (type == "pills") {
    paste0(tabSetCl, " nav-pills")
  }
  
  # side
  if (side == "right") {
    tabSetCl <- paste0(tabSetCl, " ml-auto p-2")
  } else {
    tabSetCl <- paste0(tabSetCl, " p-2")
  }
  
  # support vertical tabs
  if (vertical) tabSetCl <- paste0(tabSetCl, " flex-column")
  
  tabSetMenu <- shiny::tags$ul(
    id = id,
    class = "tabsetpanel",
    class = tabSetCl,
    `aria-orientation` = if (vertical) "vertical" else NULL
  )
  tabSetMenu <- shiny::tagAppendChildren(tabSetMenu, tabSetPanelItem)
  
  # content
  tabSetContent <- shiny::tags$div(
    class = "tab-content",
    lapply(seq_along(tabs), FUN = function(i) {
      
      # put the correct namespace on ids
      tabs[[i]][[2]]$attribs$id <- ns(tabs[[i]][[2]]$attribs$id)
      tabs[[i]][[2]]$attribs$`aria-labelledby` <- ns(tabs[[i]][[2]]$attribs$`aria-labelledby`)
      tabs[[i]][[2]]
    })
  )
  
  # Wrapper
  if (vertical) {
    if (side == "left") {
      shiny::fluidRow(
        shiny::column(width = 3, tabSetMenu),
        shiny::column(width = 9, tabSetContent)
      )
    } else {
      shiny::fluidRow(
        shiny::column(width = 9, tabSetContent),
        shiny::column(width = 3, tabSetMenu)
      )
    }
  } else {
    shiny::tagList(tabSetMenu, tabSetContent)
  }
}



#' Create a tabPanel
#' 
#' To be included in a bs4TabCard
#'
#' @param tabName Tab name: it will be also passed as the id argument. Must be unique.
#' @param ... Tab content.
#' @param active Whether the tab is active or not. FALSE bu default.
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4TabPanel <- function(tabName, ..., active = FALSE) {
  
  
  id <- tabName
  # handle punctuation
  id <- gsub(x = id, pattern = "[[:punct:]]", replacement = "")
  # handle tab names with space
  id <- gsub(x = id, pattern = " ", replacement = "")
  
  tabPanelTag <- shiny::tags$div(
    class = if (active) "tab-pane fade active" else "tab-pane fade",
    id = id,
    role = "tabpanel",
    `aria-labelledby` = paste0(id, "-tab"),
    ...
  )
  return(list(tabName, tabPanelTag))
}





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