#' Create a Boostrap 4 dashboard main sidebar
#'
#' \link{dashboardSidebar} creates an adminLTE3 dashboard main sidebar to
#' insert in the sidebar slot of \link{dashboardPage}.
#'
#' @param ... Slot for \link{sidebarMenu}.
#' @param disable If \code{TRUE}, the sidebar will be disabled.
#' @param width The width of the sidebar. This must either be a number which
#'   specifies the width in pixels, or a string that specifies the width in CSS
#'   units.
#' @param skin Sidebar skin. "dark" or "light".
#' @param status Sidebar status. Valid statuses are defined as follows:
#' \itemize{
#'   \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item \code{gray-dark}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item \code{gray}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item \code{white}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#fff")}.
#'   \item \code{indigo}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6610f2")}.
#'   \item \code{lightblue}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3c8dbc")}.
#'   \item \code{navy}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#001f3f")}.
#'   \item \code{purple}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#605ca8")}.
#'   \item \code{fuchsia}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#f012be")}.
#'   \item \code{pink}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#e83e8c")}.
#'   \item \code{maroon}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#d81b60")}.
#'   \item \code{orange}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ff851b")}.
#'   \item \code{lime}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#01ff70")}.
#'   \item \code{teal}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#39cccc")}.
#'   \item \code{olive}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3d9970")}.
#' }
#' @param elevation Sidebar elevation. 4 by default (until 5).
#' @param collapsed If \code{TRUE}, the sidebar will be collapsed on app startup.
#' @param minified Whether to slightly close the sidebar but still show item icons. Default
#' to TRUE.
#' @param expandOnHover Whether to expand the sidebar om hover. TRUE by default.
#' @param fixed Whether to fix the sidebar. Default to TRUE.
#' @param id Recover the state of the sidebar.
#' @param customArea Sidebar bottom space area. Only works if sidebar is fixed.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' @rdname dashboardSidebar
#' @seealso \link{dashboardBody}
#'
#' @export
bs4DashSidebar <- function(..., disable = FALSE, width = NULL,
                           skin = "dark", status = "primary",
                           elevation = 4, collapsed = FALSE,
                           minified = TRUE, expandOnHover = TRUE,
                           fixed = TRUE, id = NULL, customArea = NULL) {
  if (is.null(id)) id <- "sidebarId"
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

  # Handle different width
  customCSS <- NULL
  if (!is.null(width)) {
    customCSS <- shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          gsub(
            "_WIDTH_",
            width,
            fixed = TRUE,
            "@media (min-width: 768px) {
              body:not(.sidebar-mini-md) .content-wrapper, 
              body:not(.sidebar-mini-md) .main-footer, 
              body:not(.sidebar-mini-md) .main-header {
                transition: margin-left .3s ease-in-out;
                margin-left: _WIDTH_;
              }
            }
            
            @media (min-width: 992px) {
              .sidebar-mini.sidebar-collapse .main-sidebar.sidebar-focused, 
              .sidebar-mini.sidebar-collapse .main-sidebar:hover {
                width: _WIDTH_;
              }
            }

            @media (min-width: 992px) {
              .sidebar-mini.sidebar-collapse.layout-fixed .main-sidebar:hover .brand-link {
                width: _WIDTH_;
              }
            }
    
            .sidebar-collapse .main-sidebar, .sidebar-collapse .main-sidebar::before {
              margin-left: -_WIDTH_;
            }

            .main-sidebar, .main-sidebar::before {
              transition: margin-left .3s ease-in-out,width .3s ease-in-out;
              width: _WIDTH_;
            }

            .layout-fixed .brand-link {
              width: _WIDTH_;
            }
          "
          )
        )
      )
    )
  }

  sidebarTag <- shiny::tags$aside(
    customCSS,
    id = id,
    `data-fixed` = tolower(fixed),
    `data-minified` = if (minified) "true" else "false",
    `data-collapsed` = dataValueString,
    `data-disable` = if (disable) TRUE else FALSE,
    class = paste0(
      "main-sidebar sidebar-", skin, "-",
      status, " elevation-", elevation,
      if (expandOnHover) NULL else " sidebar-no-expand",
      if (!is.null(customArea)) " main-sidebar-custom"
    ),
    style = if (disable) "display: none;"
  )

  sidebarTag <- shiny::tagAppendChildren(sidebarTag, contentTag)

  # bottom sidebar
  if (!is.null(customArea)) {
    sidebarTag <- shiny::tagAppendChild(
      sidebarTag,
      shiny::tags$div(class = "sidebar-custom", customArea)
    )
  }

  sidebarTag
}




#' Toggle sidebar state
#'
#' \link{updateSidebar} toggles a \link{dashboardSidebar} on the client.
#'
#' @param id Sidebar id.
#' @param session Shiny session object.
#' @export
#'
#' @rdname dashboardSidebar
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bs4Dash)
#'
#'   shinyApp(
#'     ui = dashboardPage(
#'       header = dashboardHeader(),
#'       sidebar = dashboardSidebar(id = "sidebar"),
#'       body = dashboardBody(
#'         actionButton(inputId = "sidebarToggle", label = "Toggle Sidebar")
#'       )
#'     ),
#'     server = function(input, output, session) {
#'       observeEvent(input$sidebar, {
#'         if (input$sidebar) {
#'           showModal(modalDialog(
#'             title = "Alert",
#'             "The sidebar is opened.",
#'             easyClose = TRUE,
#'             footer = NULL
#'           ))
#'         }
#'       })
#'
#'       observeEvent(input$sidebarToggle, {
#'         updateSidebar(id = "sidebar", session = session)
#'       })
#'
#'       observe({
#'         print(input$sidebar)
#'       })
#'     }
#'   )
#' }
updatebs4Sidebar <- function(id, session = shiny::getDefaultReactiveDomain()) {
  session$sendInputMessage(id, NULL)
}




#' Dashboard main sidebar menu
#'
#' \link{sidebarMenu} creates a menu for \link{dashboardSidebar}.
#'
#' @param ... Slot for \link{menuItem} or \link{sidebarHeader}.
#' @param id For \link{sidebarMenu}, if \code{id} is present, this id will be
#'   used for a Shiny input value, and it will report which tab is selected. For
#'   example, if \code{id="tabs"}, then \code{input$tabs} will be the
#'   \code{tabName} of the currently-selected \link{menuItem}.
#' @param .list An optional list containing items to put in the menu Same as the
#' \code{...} arguments, but in list format. This can be useful when working
#' with programmatically generated items.
#' @param flat Whether sidebar items should have a flat design. FALSE by default.
#' @param compact Whether items should be compacted. FALSE by default.
#' @param childIndent Whether to indent children. TRUE by default.
#' @param legacy Whether to use the old adminLTE2 item selection display. Default
#' to FALSE.
#'
#' @rdname dashboardSidebar
#' @export
bs4SidebarMenu <- function(..., id = NULL, .list = NULL, flat = FALSE,
                           compact = FALSE, childIndent = TRUE, legacy = FALSE) {
  if (is.null(id)) id <- paste0("tabs_", round(stats::runif(1, min = 0, max = 1e9)))

  # make sure only 1 item is selected at start
  items <- c(list(...), .list)
  nav_items <- findSidebarItem(items, "nav-item")
  selectedItems <- dropNulls(lapply(seq_along(nav_items), function(i) {
    if (length(nav_items[[i]]$children[[1]]$attribs$`data-start-selected`) > 0) TRUE else NULL
  }))
  if (length(selectedItems) > 1) stop("Only 1 item may be selected at start!")

  menuCl <- "nav nav-pills nav-sidebar flex-column sidebar-menu"
  if (flat) menuCl <- paste0(menuCl, " nav-flat")
  if (compact) menuCl <- paste0(menuCl, " nav-compact")
  if (childIndent) menuCl <- paste0(menuCl, " nav-child-indent")
  if (legacy) menuCl <- paste0(menuCl, " nav-legacy")

  # menu Tag
  shiny::tags$ul(
    class = menuCl,
    `data-widget` = "treeview",
    role = "menu",
    `data-accordion` = "true",
    items,
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



#' Internally used by \link{sidebarMenu} to find treeview items
#' and normal items.
#' @param items List to search in.
#' @param regex Regex to apply.
#' @keywords internal
findSidebarItem <- function(items, regex) {
  dropNulls(lapply(seq_along(items), function(i) {
    isNavItem <- length(grep(regex, items[[i]]$attribs$class, perl = TRUE)) > 0
    if (isNavItem) items[[i]]
  }))
}




#' Dashboard sidebar menu item
#'
#' \link{menuItem} creates an item to put in \link{sidebarMenu}.
#'
#' @param text Item name.
#' @param ... \link{menuSubItem}.
#' @param icon An icon tag, created by \code{\link[shiny]{icon}}. If
#'   \code{NULL}, don't display an icon.
#' @param badgeLabel A label for an optional badge. Usually a number or a short
#'   word like "new".
#' @param badgeColor A color for the badge. Valid colors:
#' \itemize{
#'   \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#' }
#' @param tabName Should correspond exactly to the tabName given in \code{\link{tabItem}}.
#' @param href An link address. Not compatible with \code{tabName}.
#' @param newTab If \code{href} is supplied, should the link open in a new
#'   browser tab?
#' @param selected If \code{TRUE}, this \code{menuItem}
#'  will start selected. If no item have \code{selected=TRUE}, then the first
#'  \code{menuItem} will start selected.
#' @param expandedName A unique name given to each \code{menuItem} that serves
#'   to indicate which one (if any) is currently expanded. (This is only applicable
#'   to \code{menuItem}s that have children and it is mostly only useful for
#'   bookmarking state.)
#' @param startExpanded Whether to expand the \link{menuItem} at start.
#' @param condition When using \link{menuItem} with \link[shiny]{conditionalPanel},
#' write the condition here (see \url{https://github.com/RinteRface/bs4Dash/issues/35}).
#'
#' @rdname dashboardSidebar
#'
#' @note See examples for a use case of the condition parameter.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   # sidebarItem with conditional value
#'   library(shiny)
#'   library(bs4Dash)
#'
#'   ui <- dashboardPage(
#'     dashboardHeader(),
#'     dashboardSidebar(
#'       sidebarMenu(
#'         id = "sidebarMenu",
#'         menuItem(
#'           text = "Tab 1",
#'           tabName = "tab1"
#'         ),
#'         menuItem(
#'           condition = "input.show == true",
#'           text = "Tab 2",
#'           tabName = "tab2"
#'         )
#'       )
#'     ),
#'     dashboardBody(
#'       tabItems(
#'         tabItem(
#'           tabName = "tab1",
#'           h1("Welcome!"),
#'           checkboxInput("show", "Show Tab 2", FALSE)
#'         ),
#'         tabItem(
#'           tabName = "tab2",
#'           h1("Hey! You found me!")
#'         )
#'       )
#'     )
#'   )
#'   server <- function(input, output) {}
#'   shinyApp(ui = ui, server = server)
#' }
bs4SidebarMenuItem <- function(text, ..., icon = NULL, badgeLabel = NULL, badgeColor = "success",
                               tabName = NULL, href = NULL,
                               newTab = TRUE, selected = NULL,
                               expandedName = as.character(gsub("[[:space:]]", "", text)),
                               startExpanded = FALSE, condition = NULL) {
  subItems <- list(...)

  if (!is.null(icon)) {
    tagAssert(icon, type = "i")
    icon$attribs$class <- paste0(icon$attribs$class, " nav-icon")
  }

  if (!is.null(href) + !is.null(tabName) + (length(subItems) > 0) != 1) {
    stop("Must have either href, tabName, or sub-items (contained in ...).")
  }

  if (!is.null(badgeLabel) && length(subItems) != 0) {
    stop("Can't have both badge and subItems")
  }

  # Generate badge if needed
  if (!is.null(badgeLabel)) {
    validateStatus(badgeColor)
    badgeTag <- dashboardBadge(badgeLabel, color = badgeColor, position = "right")
  } else {
    badgeTag <- NULL
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
          href = if (!is.null(href)) href else "#",
          `data-target` = if (is.null(href)) {
            if (!is.null(tabName)) {
              paste0("#shiny-tab-", tabName)
            } 
          },
          target = if (!is.null(href)) {
            if (newTab) "_blank"
          },
          `data-toggle` = if (is.null(href)) "tab",
          `data-value` = if (!is.null(tabName)) tabName,
          # needed by leftSidebar.js
          `data-start-selected` = if (isTRUE(selected)) 1 else NULL,
          icon,
          shiny::tags$p(text, badgeTag)
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





#' Dashboard sidebar menu sub-item
#'
#' \link{menuSubItem} creates an item to put in \link{menuItem}.
#'
#' @param text Item name.
#' @param tabName Should correspond exactly to the tabName given in \code{\link{tabItem}}.
#' @param href An link address. Not compatible with \code{tabName}.
#' @param newTab If \code{href} is supplied, should the link open in a new
#'   browser tab?
#' @param icon An icon tag, created by \code{\link[shiny]{icon}}. If
#'   \code{NULL}, don't display an icon.
#' @param selected If \code{TRUE}, this \code{menuSubItem}
#'   will start selected. If no item have \code{selected=TRUE}.
#'
#' @rdname dashboardSidebar
#'
#' @export
bs4SidebarMenuSubItem <- function(text, tabName = NULL, href = NULL,
                                  newTab = NULL, icon = shiny::icon("angle-double-right"), selected = NULL) {
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
      href = if (!is.null(href)) href else "#",
      `data-target` = if (is.null(href)) {
        if (!is.null(tabName)) {
          paste0("#shiny-tab-", tabName)
        } 
      },
      target = if (!is.null(href)) {
        if (newTab) "_blank"
      },
      `data-toggle` = if (is.null(href)) "tab",
      `data-value` = tabName,
      # below this is needed by leftSidebar.js
      `data-start-selected` = if (isTRUE(selected)) 1 else NULL,
      icon,
      shiny::tags$p(text)
    )
  )
}


#' Dashboard sidebar menu header
#'
#' \link{sidebarHeader} creates a header to put in \link{dashboardSidebar}.
#'
#' @param title title.
#'
#' @rdname dashboardSidebar
#'
#' @export
bs4SidebarHeader <- function(title) {
  shiny::tags$li(class = "nav-header", title)
}



#' Dashboard sidebar user panel
#'
#' \link{sidebarUserPanel} creates a user Panel to put in \link{dashboardSidebar}.
#'
#' @param name Name of the user.
#' @param image A filename or URL to use for an image of the person. If it is a
#' local file, the image should be contained under the www/ subdirectory of
#' the application.
#'
#' @rdname dashboardSidebar
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
#' \link{updateTabItems} controls the active tab of \code{\link{tabItems}} from the
#' server. It behaves just like \code{\link[shiny]{updateTabsetPanel}}.
#'
#' @rdname dashboardSidebar
#'
#' @inheritParams shiny::updateTabsetPanel
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   library(shiny)
#'   library(bs4Dash)
#'
#'   shinyApp(
#'     ui = dashboardPage(
#'       header = dashboardHeader(skin = "dark"),
#'       body = dashboardBody(
#'         tabItems(
#'           tabItem(
#'             tabName = "tab1",
#'             sliderInput("obs", "Number of observations:",
#'               min = 0, max = 1000, value = 500
#'             ),
#'             plotOutput("distPlot")
#'           ),
#'           tabItem(
#'             tabName = "tab2",
#'             checkboxGroupInput(
#'               "variable", "Variables to show:",
#'               c(
#'                 "Cylinders" = "cyl",
#'                 "Transmission" = "am",
#'                 "Gears" = "gear"
#'               )
#'             ),
#'             tableOutput("data")
#'           ),
#'           tabItem(
#'             tabName = "tab3",
#'             checkboxInput("val", "Some value", FALSE),
#'             textOutput("value")
#'           ),
#'           tabItem(
#'             tabName = "tab4",
#'             "Nothing special here!"
#'           ),
#'           tabItem(
#'             tabName = "tab5",
#'             "Tab 5"
#'           ),
#'           tabItem(
#'             tabName = "tab6",
#'             "Tab 6"
#'           ),
#'           tabItem(
#'             tabName = "tab7",
#'             "Tab 7"
#'           )
#'         )
#'       ),
#'       sidebar = dashboardSidebar(
#'         skin = "light",
#'         inputId = "sidebarState",
#'         sidebarMenu(
#'           id = "sidebar",
#'           menuItem(
#'             text = "Tab 1",
#'             tabName = "tab1",
#'             icon = icon("shuttle-van")
#'           ),
#'           menuItem(
#'             text = "Tab 2",
#'             tabName = "tab2",
#'             icon = icon("space-shuttle"),
#'             selected = TRUE
#'           ),
#'           menuItem(
#'             text = "Item List 1",
#'             icon = icon("bars"),
#'             startExpanded = TRUE,
#'             menuSubItem(
#'               text = "Item 3",
#'               tabName = "tab3",
#'               icon = icon("circle-thin")
#'             ),
#'             menuSubItem(
#'               text = "Item 4",
#'               tabName = "tab4",
#'               icon = icon("circle-thin")
#'             )
#'           ),
#'           menuItem(
#'             text = "Item List 2",
#'             icon = icon("bars"),
#'             startExpanded = FALSE,
#'             menuSubItem(
#'               text = "Item 5",
#'               tabName = "tab5",
#'               icon = icon("circle-thin")
#'             ),
#'             menuSubItem(
#'               text = "Item 6",
#'               tabName = "tab6",
#'               icon = icon("circle-thin")
#'             )
#'           ),
#'           menuItem(
#'             text = "Tab 7",
#'             tabName = "tab7",
#'             icon = icon("home")
#'           )
#'         )
#'       ),
#'       controlbar = dashboardControlbar(
#'         skin = "light",
#'         sliderInput(
#'           inputId = "controller",
#'           label = "Update the first tabset",
#'           min = 1,
#'           max = 6,
#'           value = 2
#'         )
#'       ),
#'       footer = bs4DashFooter()
#'     ),
#'     server = function(input, output, session) {
#'       observe(print(input$sidebarItemExpanded))
#'       observe(print(input$sidebar))
#'
#'       # update tabset1
#'       observeEvent(input$controller,
#'         {
#'           updateTabItems(
#'             session,
#'             inputId = "sidebar",
#'             selected = paste0("tab", input$controller)
#'           )
#'         },
#'         ignoreInit = TRUE
#'       )
#'
#'       output$distPlot <- renderPlot({
#'         hist(rnorm(input$obs))
#'       })
#'
#'       output$data <- renderTable(
#'         {
#'           mtcars[, c("mpg", input$variable), drop = FALSE]
#'         },
#'         rownames = TRUE
#'       )
#'
#'       output$value <- renderText({
#'         input$val
#'       })
#'     }
#'   )
#' }
#' @export
updatebs4TabItems <- function (session = shiny::getDefaultReactiveDomain(), inputId, selected = NULL) {
  shiny::updateTabsetPanel(session = session, inputId, selected = selected)
}
