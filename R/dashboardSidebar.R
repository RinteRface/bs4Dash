#' Create a Boostrap 4 dashboard main sidebar
#'
#' Build an adminLTE3 dashboard main sidebar
#'
#' @param ... Slot for \link{bs4SidebarMenu}.
#' @param inputId Recover the state of the sidebar.
#' @param disable If \code{TRUE}, the sidebar will be disabled.
#' @param title Sidebar title.
#' @param skin Sidebar skin. "dark" or "light"
#' @param status Sidebar status. "primary", "danger", "warning", "success", "info".
#' @param brandColor Brand color. NULL by default: "primary", "danger", "warning",
#' "success", "info", "white" or "gray-light".
#' @param url Sidebar brand link.
#' @param src Sidebar brand image.
#' @param elevation Sidebar elevation. 4 by default (until 5).
#' @param opacity Sidebar brand opacity. From 0 to 1. 0.8 by default.
#' @param expand_on_hover Whether to expand the sidebar om hover. TRUE by default.
#' @param fixed Whether to fix the sidebar. Default to TRUE.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashSidebar <- function(..., inputId = NULL, disable = FALSE, 
                           title = NULL, skin = "dark", status = "primary",
                           brandColor = NULL, url = NULL, src = NULL,
                           elevation = 4, opacity = .8, expand_on_hover = TRUE,
                           fixed = TRUE) {

  # brand logo
  brandTag <- if (!is.null(title)) {
    shiny::tags$a(
      class = if (!is.null(brandColor)) paste0("brand-link bg-", brandColor) else "brand-link",
      href = url,
      shiny::tags$img(
        src = src,
        class = "brand-image img-circle elevation-3",
        style = paste0("opacity: ", opacity)
      ),
      shiny::tags$span(class = "brand-text font-weight-light", title)
    )
  }

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
    id = inputId,
    `data-fixed` = tolower(fixed),
    class = paste0(
      "main-sidebar sidebar-", skin, "-", 
      status, " elevation-", elevation,
      if (expand_on_hover) NULL else " sidebar-no-expand"
    ),
    style = if (disable) "display: none;"
   )

  sidebarTag <- shiny::tagAppendChildren(sidebarTag, brandTag, contentTag)
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
  
  
  ## change sidebar width
  #shiny::tagList(
  #  shiny::singleton(
  #    shiny::tags$head(
  #      shiny::tags$style(
  #        shiny::HTML(
  #          paste0(
  #            ".main-sidebar, .main-sidebar:before {
  #              transition: margin-left 0.3s ease-in-out, width 0.3s ease-in-out;
  #              width: ", width, "px;
  #            }
  #            .sidebar-mini.sidebar-collapse .main-sidebar:hover {
  #                width: ", width, "px;
  #            }
  #            @media (min-width: 768px) {
  #              .content-wrapper,
  #              .main-footer,
  #              .main-header {
  #                transition: margin-left 0.3s ease-in-out;
  #                margin-left: ", width, "px;
  #                z-index: 3000;
  #              }
  #            }
  #            .nav-sidebar:hover {
  #              overflow: hidden;
  #            }
  #            "
  #          )
  #        )
  #      )
  #    )
  #  ),
  #  sidebarTag
  #)
  
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
#' @param flat Whether sidebar items should have a flat design. FALSE by default.
#' @param compact Whether items should be compacted. FALSE by default.
#' @param child_indent Whether to indent children. TRUE by default
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
#'      sidebar_collapsed = TRUE,
#'      controlbar_collapsed = TRUE,
#'      enable_preloader = FALSE,
#'      loading_duration =  2,
#'      navbar = bs4DashNavbar(skin = "light"),
#'      body = bs4DashBody(
#'        
#'      ),
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
#'            ),bs4SidebarMenuSubItem(
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
bs4SidebarMenu <- function(..., id = NULL, flat = FALSE, 
                           compact = FALSE, child_indent = TRUE) {
  
  if (is.null(id)) id <- paste0("tabs_", round(stats::runif(1, min = 0, max = 1e9)))
  
  # make sure only 1 item is selected at start
  items <- list(...)
  items <- findSidebarItem(items, "nav-item")
  selectedItems <- dropNulls(lapply(seq_along(items), function(i) {
    if (length(items[[i]]$children[[1]]$attribs$`data-start-selected`) > 0) TRUE else NULL
  }))
  if (length(selectedItems) > 1) stop("Only 1 item may be selected at start!")
  
  menuCl <- "nav nav-pills nav-sidebar flex-column"
  if (flat) menuCl <- paste0(menuCl, " nav-flat")
  if (compact) menuCl <- paste0(menuCl, " nav-compact")
  if (child_indent) menuCl <- paste0(menuCl, " nav-child-indent")
  
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
#' @param tabName Should correspond exactly to the tabName given in \code{\link{bs4TabItem}}.
#' @param icon Item icon.
#' @param expandedName A unique name given to each \code{menuItem} that serves
#'   to indicate which one (if any) is currently expanded. (This is only applicable
#'   to \code{menuItem}s that have children and it is mostly only useful for
#'   bookmarking state.)
#' @param startExpanded Whether to expand the \link{bs4SidebarMenuItem} at start.
#' @param condition When using \link{bs4SidebarMenuItem} with \link[shiny]{conditionalPanel},
#' write the condition here (see \url{https://github.com/RinteRface/bs4Dash/issues/35}).
#' @param selected If \code{TRUE}, this \code{bs4SidebarMenuItem}
#'  will start selected. If no item have \code{selected=TRUE}, then the first
#'  \code{bs4SidebarMenuItem} will start selected.
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
bs4SidebarMenuItem <- function(text, ..., tabName = NULL, icon = NULL, 
                               expandedName = as.character(gsub("[[:space:]]", "", text)), 
                               startExpanded = FALSE, condition = NULL, selected = NULL) {
  
  subitems <- list(...)
  
  # classic menuItem with 1 element
  if (length(subitems) == 0) {
    return(
      shiny::tags$li(
        class = "nav-item",
        `data-display-if` = condition,
        shiny::tags$a(
          class = "nav-link",
          id = paste0("tab-", tabName),
          href = paste0("#shiny-tab-", tabName),
          `data-toggle` = "tab",
          `data-value` = tabName,
          # needed by leftSidebar.js
          `data-start-selected` = if (isTRUE(selected)) 1 else NULL,
          shiny::tags$i(class = paste0("nav-icon fa fa-", icon)),
          shiny::tags$p(text)
        )
      )
    )
    # in case we have multiple subitems
  } else {
    
    # add special class for leftSidebar.js
    for (i in seq_along(subitems)) {
      subitems[[i]]$children[[1]]$attribs$class <- paste(
        subitems[[i]]$children[[1]]$attribs$class,
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
    selectedItems <- dropNulls(lapply(seq_along(subitems), function(i) {
      if (length(subitems[[i]]$children[[1]]$attribs$`data-start-selected`) > 0) TRUE else NULL
    }))
    if (length(selectedItems) > 1) stop("Only 1 subitem may be selected!")
    
    shiny::tags$li(
      class = paste0("nav-item has-treeview", if (isExpanded) " menu-open" else ""),
      shiny::tags$a(
        href = "#",
        class = "nav-link",
        `data-start-selected` = if (isTRUE(selected)) 1 else NULL,
        shiny::tags$i(class = paste0("nav-icon fas fa-", icon)),
        shiny::tags$p(
          text,
          shiny::tags$i(class = "right fas fa-angle-left")
        )
      ),
      shiny::tags$ul(
        class = "nav nav-treeview",
        `data-expanded` = expandedName,
        subitems
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
#' @param icon Item icon.
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
#'      navbar = bs4DashNavbar(),
#'      sidebar = bs4DashSidebar(
#'        bs4SidebarMenu(
#'          bs4SidebarHeader("List of items 1"),
#'          bs4SidebarMenuItem(
#'            text = "Item List",
#'            icon = "bars",
#'            startExpanded = TRUE,
#'            bs4SidebarMenuSubItem(
#'              text = "Item 1",
#'              tabName = "item1",
#'              icon = "circle-thin"
#'            ),
#'            bs4SidebarMenuSubItem(
#'              text = "Item 2",
#'              tabName = "item2",
#'              icon = "circle-thin"
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
#'            icon = "bars",
#'            startExpanded = FALSE,
#'            #active = FALSE,
#'            bs4SidebarMenuSubItem(
#'              text = "Item 4",
#'              tabName = "item4",
#'              icon = "circle-thin"
#'            ),
#'            bs4SidebarMenuSubItem(
#'              text = "Item 5",
#'              tabName = "item5",
#'              icon = "circle-thin",
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
bs4SidebarMenuSubItem <- function(text, tabName = NULL, icon = NULL, selected = NULL) {
  shiny::tags$li(
    class = "nav-item",
    shiny::tags$a(
      class = "nav-link",
      id = paste0("tab-", tabName),
      href = paste0("#shiny-tab-", tabName),
      `data-toggle` = "tab",
      `data-value` = tabName,
      # below this is needed by leftSidebar.js
      `data-start-selected` = if (isTRUE(selected)) 1 else NULL,
      shiny::tags$i(class = paste0("nav-icon fa fa-", icon)),
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
  shiny::tags$li(
    class = "nav-header",
    title
  )
}



#' Create a Boostrap 4 dashboard main sidebar user panel
#'
#' Build an adminLTE3 dashboard main sidebar user panel
#'
#' @param img User panel image path or url.
#' @param text User panel text.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4SidebarUserPanel <- function(img = NULL, text = NULL) {
  shiny::tags$div(
    class = "user-panel mt-3 pb-3 mb-3 d-flex",
    shiny::tags$div(
      class = "image",
      shiny::img(src = img, class = "img-circle elevation-2")
    ),
    shiny::tags$div(
      class = "info",
      shiny::a(class = "d-block", href = "#", text)
    )
  )
}
