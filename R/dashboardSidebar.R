#' Create a Boostrap 4 dashboard main sidebar
#'
#' Build an adminLTE3 dashboard main sidebar
#'
#' @param ... Slot for \link{bs4SidebarMenu}.
#' @param disable If \code{TRUE}, the sidebar will be disabled.
#' @param width The width of the sidebar. This must either be a number which
#'   specifies the width in pixels, or a string that specifies the width in CSS
#'   units.
#' @param skin Sidebar skin. "dark" or "light".
#' @param status Sidebar status. "primary", "danger", "warning", "success", "info".
#' @param elevation Sidebar elevation. 4 by default (until 5).
#' @param collapsed If \code{TRUE}, the sidebar will be collapsed on app startup.
#' @param minified Whether to slightly close the sidebar but still show item icons. Default
#' to TRUE.
#' @param expandOnHover Whether to expand the sidebar om hover. TRUE by default.
#' @param fixed Whether to fix the sidebar. Default to TRUE.
#' @param id Recover the state of the sidebar.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashSidebar <- function(..., disable = FALSE, width = NULL, 
                           skin = "dark", status = "primary",
                           elevation = 4, collapsed = FALSE,
                           minified = TRUE, expandOnHover = TRUE,
                           fixed = TRUE, id = NULL) {
  
  # If we're restoring a bookmarked app, this holds the value of whether or not the
  # sidebar was collapsed. If this is not the case, the default is whatever the user
  # specified in the `collapsed` argument.
  dataValue <- shiny::restoreInput(id = id, default = collapsed)
  if (disable) dataValue <- TRUE # this is a workaround to fix #209
  dataValueString <- if (dataValue) "true" else "false"


  # sidebar content
  contentTag <- shiny::tags$div(
    class = "sidebar",
    id = "sidebarItemExpanded",
    shiny::tags$nav(
      class = "mt-2",
      ...
    )
  )

  sidebarTag <- shiny::tags$aside(
    id = id,
    `data-fixed` = tolower(fixed),
    `data-minified` = if (minified) "true" else "false", 
    `data-collapsed` = dataValueString, 
    class = paste0(
      "main-sidebar sidebar-", skin, "-", 
      status, " elevation-", elevation,
      if (expandOnHover) NULL else " sidebar-no-expand"
    ),
    style = if (disable) "display: none;"
   )

  sidebarTag <- shiny::tagAppendChildren(sidebarTag, contentTag)
  sidebarTag
  
  customCSS <- shiny::singleton(
    shiny::tags$style(
      ".content-wrapper, .main-footer, .main-header {
          margin-left: 0px;
       }
      "
    )
  )
  
  if (disable) shiny::tagList(customCSS, sidebarTag) else sidebarTag
}




#' Function to programmatically toggle the state of the sidebar
#'
#' @param inputId Sidebar id.
#' @param session Shiny session object.
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shiny::shinyApp(
#'    ui = dashboardPage(
#'      controlbar_collapsed = FALSE,
#'      controlbar_overlay = TRUE,
#'      navbar = dashboardHeader(),
#'      sidebar = dashboardSidebar(inputId = "sidebar"),
#'      body = dashboardBody(
#'        actionButton(inputId = "controlbarToggle", label = "Toggle Sidebar")
#'      )
#'    ),
#'    server = function(input, output, session) {
#'      
#'      observeEvent(input$sidebar, {
#'        if (input$sidebar) {
#'          showModal(modalDialog(
#'            title = "Alert",
#'            "The sidebar is opened.",
#'            easyClose = TRUE,
#'            footer = NULL
#'          ))
#'        }
#'      })
#'      
#'      observeEvent(input$controlbarToggle, {
#'        updatebs4Sidebar(inputId = "sidebar", session = session)
#'      })
#'      
#'      observe({
#'        print(input$sidebar)
#'      })
#'    }
#'  )
#' }
updatebs4Sidebar <- function(inputId, session) {
  session$sendInputMessage(inputId, NULL)
}




#' Create a Boostrap 4 dashboard main sidebar menu
#'
#' Build an adminLTE3 dashboard main sidebar menu
#'
#' @param ... Slot for \link{bs4SidebarMenuItem} or \link{bs4SidebarHeader}.
#' @param id For \link{bs4SidebarMenu}, if \code{id} is present, this id will be
#'   used for a Shiny input value, and it will report which tab is selected. For
#'   example, if \code{id="tabs"}, then \code{input$tabs} will be the
#'   \code{tabName} of the currently-selected \link{bs4SidebarMenuItem}.
#' @param .list An optional list containing items to put in the menu Same as the
#' \code{...} arguments, but in list format. This can be useful when working
#' with programmatically generated items.
#' @param flat Whether sidebar items should have a flat design. FALSE by default.
#' @param compact Whether items should be compacted. FALSE by default.
#' @param child_indent Whether to indent children. TRUE by default.
#' @param legacy Whether to use the old adminLTE2 item selection display. Default
#' to FALSE.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'    ui = bs4DashPage(
#'      header = bs4DashNavbar(skin = "light"),
#'      body = bs4DashBody(),
#'      sidebar = bs4DashSidebar(
#'        skin = "light",
#'        bs4SidebarMenu(
#'          id = "test",
#'          bs4SidebarMenuItem(
#'            tabName = "tab1",
#'            text = "Tab 1"
#'          ),
#'          bs4SidebarMenuItem(
#'            tabName = "tab2",
#'            text = "Tab 2"
#'          ),
#'          bs4SidebarMenuItem(
#'            text = "Click me pleaaaaase",
#'            bs4SidebarMenuSubItem(
#'              tabName = "subtab1",
#'              text = "Tab 3"
#'            ),
#'            bs4SidebarMenuSubItem(
#'              tabName = "subtab2",
#'              text = "Tab 4"
#'            )
#'          )
#'        )
#'      ),
#'      controlbar = bs4DashControlbar(skin = "light"),
#'      footer = bs4DashFooter()
#'    ),
#'    server = function(input, output, session) {
#'      observeEvent(input$test, {
#'        if (input$test == "subtab1") {
#'          showModal(modalDialog(
#'            title = "Thank you so much",
#'            "You clicked me! This event is the result of
#'            an input bound to the menu. By adding an id to the
#'            bs4SidebarMenu, input$id will give the currently selected
#'            tab. This is useful to trigger some events.",
#'            easyClose = TRUE,
#'            footer = NULL
#'          ))
#'        }
#'      })
#'    }
#'  )
#' }
#'
#' @export
bs4SidebarMenu <- function(..., id = NULL, .list = NULL, flat = FALSE, 
                           compact = FALSE, childIndent = TRUE, legacy = FALSE) {
  
  if (is.null(id)) id <- paste0("tabs_", round(stats::runif(1, min = 0, max = 1e9)))
  
  # make sure only 1 item is selected at start
  items <- c(list(...), .list)
  items <- findSidebarItem(items, "nav-item")
  selectedItems <- dropNulls(lapply(seq_along(items), function(i) {
    if (length(items[[i]]$children[[1]]$attribs$`data-start-selected`) > 0) TRUE else NULL
  }))
  if (length(selectedItems) > 1) stop("Only 1 item may be selected at start!")
  
  menuCl <- "nav nav-pills nav-sidebar flex-column"
  if (flat) menuCl <- paste0(menuCl, " nav-flat")
  if (compact) menuCl <- paste0(menuCl, " nav-compact")
  if (childIndent) menuCl <- paste0(menuCl, " nav-child-indent")
  if (legacy) menuCl <- paste0(menuCl, " nav-legacy")
  
  # menu Tag
  shiny::tags$ul(
    class = menuCl,
    `data-widget` = "treeview",
    id = "sidebar-menu",
    role = "menu",
    `data-accordion` = "true",
    ...,
    # This is a 0 height div, whose only purpose is to hold the tabName of the currently
    # selected menuItem in its `data-value` attribute. This is the DOM element that is
    # bound to tabItemInputBinding in the JS side.
    shiny::tags$div(
      id = id, 
      class = "sidebarMenuSelectedTabItem", 
      `data-value` = "null"
    )
  )
  
}



#' Internally used by \link{bs4SidebarMenu} to find treeview items
#' and normal items.
#' @param items List to search in.
#' @param regex Regex to apply.
findSidebarItem <- function(items, regex) {
  dropNulls(lapply(seq_along(items), function(i) {
    isNavItem <- length(grep(regex, items[[i]]$attribs$class, perl = TRUE)) > 0
    if (isNavItem) items[[i]]
  }))
}




#' Create a Boostrap 4 dashboard main sidebar menu item
#'
#' Build an adminLTE3 dashboard main sidebar menu item
#'
#' @param text Item name.
#' @param ... \link{bs4SidebarMenuSubItem}.
#' @param icon An icon tag, created by \code{\link[shiny]{icon}}. If
#'   \code{NULL}, don't display an icon.
#' @param tabName Should correspond exactly to the tabName given in \code{\link{bs4TabItem}}.
#' @param href An link address. Not compatible with \code{tabName}.
#' @param newtab If \code{href} is supplied, should the link open in a new
#'   browser tab?
#' @param selected If \code{TRUE}, this \code{bs4SidebarMenuItem}
#'  will start selected. If no item have \code{selected=TRUE}, then the first
#'  \code{bs4SidebarMenuItem} will start selected.
#' @param expandedName A unique name given to each \code{menuItem} that serves
#'   to indicate which one (if any) is currently expanded. (This is only applicable
#'   to \code{menuItem}s that have children and it is mostly only useful for
#'   bookmarking state.)
#' @param startExpanded Whether to expand the \link{bs4SidebarMenuItem} at start.
#' @param condition When using \link{bs4SidebarMenuItem} with \link[shiny]{conditionalPanel},
#' write the condition here (see \url{https://github.com/RinteRface/bs4Dash/issues/35}).
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @note See examples for a use case of the condition parameter.
#'
#' @export
#' @examples 
#' if (interactive()) {
#'  # sidebarItem with conditional value
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  ui <- bs4DashPage(
#'   bs4DashNavbar(),
#'   bs4DashSidebar(
#'     bs4SidebarMenu(
#'       id = "sidebarMenu",
#'       bs4SidebarMenuItem(
#'         text = "Tab 1",
#'         tabName = "tab1"
#'       ),
#'       bs4SidebarMenuItem(
#'         condition = "input.show == true",
#'         text = "Tab 2",
#'         tabName = "tab2"
#'       )
#'     )
#'   ),
#'   bs4DashBody(
#'     bs4TabItems(
#'       bs4TabItem(
#'         tabName = "tab1",
#'         h1("Welcome!"),
#'         checkboxInput("show", "Show Tab 2", FALSE)
#'       ),
#'       bs4TabItem(
#'         tabName = "tab2",
#'         h1("Hey! You found me!")
#'       )
#'     )
#'    )
#'   )
#'   server <- function(input, output){}
#'   shinyApp(ui = ui, server = server)
#' }
bs4SidebarMenuItem <- function(text, ..., icon = NULL, tabName = NULL, href = NULL, 
                               newTab = TRUE, selected = NULL,
                               expandedName = as.character(gsub("[[:space:]]", "", text)), 
                               startExpanded = FALSE, condition = NULL) {
  
  subItems <- list(...)
  
  if (!is.null(icon)) {
    tagAssert(icon, type = "i")
    icon$attribs$cl <- paste0(icon$attribs$cl, " nav-icon")
  }
  
  if (!is.null(href) + !is.null(tabName) + (length(subItems) > 0) != 1 ) {
    stop("Must have either href, tabName, or sub-items (contained in ...).")
  }
  
  # classic menuItem with 1 element
  if (length(subItems) == 0) {
    return(
      shiny::tags$li(
        class = "nav-item",
        `data-display-if` = condition,
        shiny::tags$a(
          class = "nav-link",
          id = if (!is.null(tabName)) {
            paste0("tab-", tabName)
          },
          href = if (is.null(href)) {
            if (!is.null(tabName)) {
              paste0("#shiny-tab-", tabName)
            } else {
              "#"
            }
          },
          target = if (!is.null(href)) {
            if (newTab) "_blank"
          },
          `data-toggle` = "tab",
          `data-value` = if (!is.null(tabName)) tabName,
          # needed by leftSidebar.js
          `data-start-selected` = if (isTRUE(selected)) 1 else NULL,
          icon,
          shiny::tags$p(text)
        )
      )
    )
    # in case we have multiple subitems
  } else {
    
    # add special class for leftSidebar.js
    for (i in seq_along(subItems)) {
      subItems[[i]]$children[[1]]$attribs$class <- paste(
        subItems[[i]]$children[[1]]$attribs$class,
        "treeview-link"
      )
    }
    
    # If we're restoring a bookmarked app, this holds the value of what menuItem (if any)
    # was expanded (this has be to stored separately from the selected menuItem, since
    # these actually independent in AdminLTE). If no menuItem was expanded, `dataExpanded`
    # is NULL. However, we want to this input to get passed on (and not dropped), so we
    # do `%OR% ""` to assure this.
    default <- if (startExpanded) expandedName else ""
    dataExpanded <- shiny::restoreInput(id = "sidebarItemExpanded", default) %OR% ""
    
    # If `dataExpanded` is not the empty string, we need to check that it is eqaul to the
    # this menuItem's `expandedName``
    isExpanded <- nzchar(dataExpanded) && (dataExpanded == expandedName)
    
    # handle case of multiple selected subitems and raise an error if so...
    selectedItems <- dropNulls(lapply(seq_along(subItems), function(i) {
      if (length(subItems[[i]]$children[[1]]$attribs$`data-start-selected`) > 0) TRUE else NULL
    }))
    if (length(selectedItems) > 1) stop("Only 1 subitem may be selected!")
    
    shiny::tags$li(
      class = paste0("nav-item has-treeview", if (isExpanded) " menu-open" else ""),
      shiny::tags$a(
        href = "#",
        class = "nav-link",
        `data-start-selected` = if (isTRUE(selected)) 1 else NULL,
        icon,
        shiny::tags$p(
          text,
          shiny::tags$i(class = "right fas fa-angle-left")
        )
      ),
      shiny::tags$ul(
        class = "nav nav-treeview",
        `data-expanded` = expandedName,
        subItems
      )
    )
  }
  
}





#' Create a Boostrap 4 dashboard main sidebar menu sub-item
#'
#' Build an adminLTE3 dashboard main sidebar menu sub-item
#'
#' @param text Item name.
#' @param tabName Should correspond exactly to the tabName given in \code{\link{bs4TabItem}}.
#' @param href An link address. Not compatible with \code{tabName}.
#' @param newtab If \code{href} is supplied, should the link open in a new
#'   browser tab?
#' @param icon An icon tag, created by \code{\link[shiny]{icon}}. If
#'   \code{NULL}, don't display an icon.
#' @param selected If \code{TRUE}, this \code{bs4SidebarMenuSubItem}
#'   will start selected. If no item have \code{selected=TRUE}.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
#' @examples 
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'      header = bs4DashNavbar(),
#'      sidebar = bs4DashSidebar(
#'        bs4SidebarMenu(
#'          bs4SidebarHeader("List of items 1"),
#'          bs4SidebarMenuItem(
#'            text = "Item List",
#'            icon = shiny::icon("bars"),
#'            startExpanded = TRUE,
#'            bs4SidebarMenuSubItem(
#'              text = "Item 1",
#'              tabName = "item1",
#'              icon = shiny::icon("circle-thin")
#'            ),
#'            bs4SidebarMenuSubItem(
#'              text = "Item 2",
#'              tabName = "item2",
#'              icon = shiny::icon("circle-thin")
#'            )
#'          ),
#'          bs4SidebarHeader("Classic Items"),
#'          bs4SidebarMenuItem(
#'            text = "Item 3",
#'            tabName = "item3"
#'          ),
#'          bs4SidebarHeader("List of items 2"),
#'          bs4SidebarMenuItem(
#'            text = "Item List 2",
#'            icon = shiny::icon("bars"),
#'            startExpanded = FALSE,
#'            bs4SidebarMenuSubItem(
#'              text = "Item 4",
#'              tabName = "item4",
#'              icon = shiny::icon("circle-thin")
#'            ),
#'            bs4SidebarMenuSubItem(
#'              text = "Item 5",
#'              tabName = "item5",
#'              icon = shiny::icon("circle-thin"),
#'              selected = TRUE
#'            )
#'          )
#'        )
#'      ),
#'      controlbar = bs4DashControlbar(),
#'      footer = bs4DashFooter(),
#'      title = "test",
#'      body = bs4DashBody(
#'        bs4TabItems(
#'          bs4TabItem(
#'            tabName = "item1",
#'            bs4Card(
#'              title = "Card 1", 
#'              closable = TRUE, 
#'              width = 6,
#'              solidHeader = TRUE, 
#'              status = "primary",
#'              collapsible = TRUE,
#'              p("Box Content")
#'            )
#'          ),
#'          bs4TabItem(
#'            tabName = "item2",
#'            bs4Card(
#'              title = "Card 2", 
#'              closable = TRUE, 
#'              width = 6,
#'              solidHeader = TRUE, 
#'              status = "warning",
#'              collapsible = TRUE,
#'              p("Box Content")
#'            )
#'          ),
#'          bs4TabItem(
#'            tabName = "item3",
#'            bs4Card(
#'              title = "Card 3", 
#'              closable = TRUE, 
#'              width = 6,
#'              solidHeader = TRUE, 
#'              status = "danger",
#'              collapsible = TRUE,
#'              p("Box Content")
#'            )
#'          ),
#'          bs4TabItem(
#'            tabName = "item4",
#'            bs4Card(
#'              title = "Card 4", 
#'              closable = TRUE, 
#'              width = 6,
#'              solidHeader = TRUE, 
#'              status = "info",
#'              collapsible = TRUE,
#'              p("Box Content")
#'            )
#'          ),
#'          bs4TabItem(
#'            tabName = "item5",
#'            bs4Card(
#'              title = "Card 5", 
#'              closable = TRUE, 
#'              width = 6,
#'              solidHeader = TRUE, 
#'              status = "success",
#'              collapsible = TRUE,
#'              p("Box Content")
#'            )
#'          )
#'        )
#'      )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
bs4SidebarMenuSubItem <- function(text, tabName = NULL, href = NULL, 
                                  newTab = NULL, icon = NULL, selected = NULL) {
  
  if (!is.null(icon)) {
    tagAssert(icon, type = "i")
    icon$attribs$cl <- paste0(icon$attribs$cl, " nav-icon")
  }
  
  shiny::tags$li(
    class = "nav-item",
    shiny::tags$a(
      class = "nav-link",
      id = if (!is.null(tabName)) {
        paste0("tab-", tabName)
      },
      href = if (is.null(href)) {
        if (!is.null(tabName)) {
          paste0("#shiny-tab-", tabName)
        } else {
          "#"
        }
      },
      target = if (!is.null(href)) {
        if (newTab) "_blank"
      },
      `data-toggle` = "tab",
      `data-value` = tabName,
      # below this is needed by leftSidebar.js
      `data-start-selected` = if (isTRUE(selected)) 1 else NULL,
      icon,
      shiny::tags$p(text)
    )
  )
}


#' Create a Boostrap 4 dashboard main sidebar header
#'
#' Build an adminLTE3 dashboard main sidebar header
#'
#' @param title SidebarHeader title.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4SidebarHeader <- function(title) {
  shiny::tags$li(class = "nav-header", title)
}



#' Create a Boostrap 4 dashboard main sidebar user panel
#'
#' Build an adminLTE3 dashboard main sidebar user panel
#'
#' @param name Name of the user.
#' @param image A filename or URL to use for an image of the person. If it is a
#' local file, the image should be contained under the www/ subdirectory of
#' the application.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4SidebarUserPanel <- function(name, image = NULL) {
  shiny::tags$div(
    class = "user-panel mt-3 pb-3 mb-3 d-flex",
    if (!is.null(image)) {
      shiny::tags$div(
        class = "image",
        shiny::img(src = image, class = "img-circle elevation-2")
      )
    },
    shiny::tags$div(
      class = "info",
      shiny::a(class = "d-block", href = "#", name)
    )
  )
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
