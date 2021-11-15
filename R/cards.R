#' Create a Boostrap 4 card
#'
#' Build an adminLTE3 card
#'
#' @param ... Contents of the box.
#' @param title Optional title.
#' @param footer Optional footer text.
#' @param status The status of the item. This determines the item's background
#'   color.  Valid statuses are defined as follows:
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
#' @param solidHeader Should the header be shown with a solid color background?
#' @param background If NULL (the default), the background of the box will be
#'   white. Otherwise, a color string. Valid colors are listed in
#'   \link{validColors}. See below:
#' \itemize{
#'  \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
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
#' @param width The width of the box, using the Bootstrap grid system. This is
#'   used for row-based layouts. The overall width of a region is 12, so the
#'   default card width of 4 occupies 1/3 of that width. For column-based
#'   layouts, use \code{NULL} for the width; the width is set by the column that
#'   contains the box.
#' @param height The height of a box, in pixels or other CSS unit. By default
#'   the height scales automatically with the content.
#' @param collapsible If TRUE, display a button in the upper right that allows
#'   the user to collapse the box.
#' @param collapsed If TRUE, start collapsed. This must be used with
#'   \code{collapsible=TRUE}.
#' @param closable If TRUE, display a button in the upper right that allows the user to close the box.
#' @param maximizable If TRUE, the card can be displayed in full screen mode.
#' @param icon Header icon. Displayed before title. Expect \code{\link[shiny]{icon}}.
#' @param gradient Whether to allow gradient effect for the background color. Default to FALSE.
#' @param boxToolSize Size of the toolbox: choose among "xs", "sm", "md", "lg".
#' @param elevation Card elevation.
#' @param headerBorder Whether to display a border between the header and body.
#' TRUE by default
#' @param label Slot for \link{boxLabel}.
#' @param dropdownMenu List of items in the boxtool dropdown menu. Use \link{boxDropdown}.
#' @param sidebar Slot for \link{boxSidebar}.
#' @param id Box unique id. \link{updateBox} target.
#'
#' @rdname box
#' @family cards
#'
#' @examples
#' # A box with label, sidebar, dropdown menu
#' if (interactive()) {
#'   library(shiny)
#'   library(bs4Dash)
#'
#'   shinyApp(
#'     ui = dashboardPage(
#'       dashboardHeader(),
#'       dashboardSidebar(),
#'       dashboardBody(
#'         box(
#'           title = "Closable Box with dropdown",
#'           closable = TRUE,
#'           width = 12,
#'           status = "warning",
#'           solidHeader = FALSE,
#'           collapsible = TRUE,
#'           label = boxLabel(
#'             text = 1,
#'             status = "danger"
#'           ),
#'           dropdownMenu = boxDropdown(
#'             boxDropdownItem("Link to google", href = "https://www.google.com"),
#'             boxDropdownItem("item 2", href = "#"),
#'             dropdownDivider(),
#'             boxDropdownItem("item 3", href = "#", icon = icon("th"))
#'           ),
#'           sidebar = boxSidebar(
#'             startOpen = TRUE,
#'             id = "mycardsidebar",
#'             sliderInput(
#'               "obs",
#'               "Number of observations:",
#'               min = 0,
#'               max = 1000,
#'               value = 500
#'             )
#'           ),
#'           plotOutput("distPlot")
#'         )
#'       )
#'     ),
#'     server = function(input, output) {
#'       output$distPlot <- renderPlot({
#'         hist(rnorm(input$obs))
#'       })
#'     }
#'   )
#' }
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4Card <- function(..., title = NULL, footer = NULL, status = NULL,
                    solidHeader = FALSE, background = NULL, width = 6, height = NULL,
                    collapsible = TRUE, collapsed = FALSE, closable = FALSE, maximizable = FALSE, icon = NULL,
                    gradient = FALSE, boxToolSize = "sm", elevation = NULL, headerBorder = TRUE, label = NULL, dropdownMenu = NULL,
                    sidebar = NULL, id = NULL) {

  if (is.null(status)) solidHeader <- TRUE
  
  # multiple validation
  validateBoxProps(
    title = title,
    label = label,
    sidebar = sidebar,
    dropdownMenu = dropdownMenu,
    status = status,
    gradient = gradient,
    collapsible = collapsible,
    collapsed = collapsed,
    solidHeader = solidHeader,
    background = background,
    elevation = elevation,
    width = width
  )

  props <- dropNulls(
    list(
      title = if (!is.null(title)) {
        if (inherits(title, "list")) {
          unlist(
            dropNulls(
              lapply(title, function(e) {
                if (inherits(e, "shiny.tag.list") ||
                  inherits(e, "shiny.tag")) {
                  as.character(e)
                }
              })
            )
          )
        } else {
          as.character(title)
        }
      } else {
        title
      },
      status = status,
      solidHeader = solidHeader,
      background = background,
      width = width,
      height = height,
      collapsible = collapsible,
      closable = closable,
      maximizable = maximizable,
      gradient = gradient
    )
  )

  # set card class
  cardCl <- setBoxClass(
    status, 
    solidHeader, 
    collapsible, 
    collapsed,
    elevation, 
    gradient, 
    background, 
    sidebar
  )

  # set style
  style <- setBoxStyle(height, sidebar)


  cardToolTag <- NULL
  # create card tools whenever necessary
  if (collapsible || closable || maximizable || 
      !is.null(dropdownMenu) || !is.null(sidebar) || !is.null(label)) {
    cardToolTag <- shiny::tags$div(class = "card-tools float-right")
  }

  # update boxToolTag
  cardToolTag <- shiny::tagAppendChildren(
    cardToolTag,
    label,
    createBoxTools(
      collapsible, 
      collapsed, 
      closable, 
      maximizable, 
      sidebar, 
      dropdownMenu,
      boxToolSize,
      status,
      background,
      solidHeader
    )
  )



  # header
  if (is.null(title) && 
    (maximizable || closable || collapsible || 
    !is.null(dropdownMenu) || !is.null(sidebar) || !is.null(label)
  )) title <- "\u200C"

  headerTag <- shiny::tags$div(
    class = if (headerBorder) "card-header" else "card-header border-0",
    shiny::tags$h3(class = "card-title", icon, title)
  )
  headerTag <- shiny::tagAppendChild(headerTag, cardToolTag)


  # body
  bodyTag <- shiny::tags$div(
    class = "card-body",
    style = style,
    ...,
    sidebar[[2]]
  )

  # footer
  footerTag <- if (!is.null(footer)) {
    shiny::tags$div(
      class = "card-footer",
      footer
    )
  }

  cardTag <- shiny::tags$div(class = cardCl, id = id)
  cardTag <- shiny::tagAppendChildren(cardTag, headerTag, bodyTag, footerTag)
  
  # wrapper
  shiny::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    cardTag,
    # config script to be used by card input binding
    shiny::tags$script(
      type = "application/json",
      `data-for` = id,
      jsonlite::toJSON(
        x = props,
        auto_unbox = TRUE,
        json_verbatim = TRUE
      )
    )
  )
}




#' Create a label for Boostrap 4 card
#'
#' @param text Label text. In practice only few letters or a number.
#' @param status label color status. See \link{getAdminLTEColors}.
#' @param tooltip Label tooltip text on hover.
#' @family boxWidgets
#' @rdname boxLabel
#' @export
bs4CardLabel <- function(text, status, tooltip = NULL) {
  if (nchar(text) > 10) warning("Avoid long texts in avaCardLabel.")

  shiny::tags$span(
    class = paste0("badge bg-", status),
    title = if (!is.null(tooltip)) tooltip,
    `data-toggle` = if (!is.null(tooltip)) "tooltip",
    text
  )
}





#' Create a sidebar for Boostrap 4 card
#'
#' To insert in the sidebar slot of \link{box}.
#'
#' @param ... Sidebar content.
#' @param id Unique sidebar id. Useful if you want to use \link{updateBoxSidebar}.
#' @param width Sidebar opening width in percentage. 50\% by default, 
#' means the card sidebar will take 50% of the card width, when opened. 
#' A numeric value between 25 and 100.
#' @param background Sidebar background color. Dark by default.
#' @param startOpen Whether the sidebar is open at start. FALSE by default.
#' @param icon Sidebar icon. Expect \code{\link[shiny]{icon}}.
#' @param easyClose Whether to close sidebar on click outside. Default to TRUE. 
#'
#' @rdname boxSidebar
#' @family boxWidgets
#' @export
bs4CardSidebar <- function(..., id = NULL, width = 50, background = "#333a40",
                           startOpen = FALSE, icon = shiny::icon("cogs"), 
                           easyClose = TRUE) {

  stopifnot(width >= 25 && width <= 100)
  
  # Toggle to insert in bs4Card
  toolbarTag <- shiny::tags$button(
    id = id,
    `data-background`= background, 
    `data-width` = width,
    `data-widget` = "chat-pane-toggle",
    `data-toggle` = "tooltip",
    `data-original-title` = "More",
    `data-start-open` = tolower(startOpen),
    `data-easy-close` = tolower(easyClose),
    type = "button",
    icon
  )

  # sidebar content
  contentTag <- shiny::tags$div(
    style = "z-index: 1; height: inherit;",
    class = "direct-chat-contacts",
    shiny::tags$ul(
      class = "contacts-list",
      shiny::tags$li(
        ...
      )
    )
  )

  shiny::tagList(toolbarTag, contentTag)
}




#' Update an AdminLTE3 card from the server side
#'
#' To update \link{box} on the server side.
#'
#' @param id Card id.
#' @param action Action to trigger: \code{c("remove", "toggle", "toggleMaximize", "restore", "update")}.
#' @param options If action is update, a list of new options to configure the box, such as
#' \code{list(title = "new title", status = NULL, solidHeader = FALSE,
#' background = "red", width = 6, height = "200px", collapsible = FALSE, closable = FALSE)}.
#' @param session Shiny session.
#'
#' @export
#'
#' @rdname box
#'
#' @examples
#' # Toggle a box on the client
#' if (interactive()) {
#'   library(shiny)
#'   library(bs4Dash)
#'
#'   ui <- dashboardPage(
#'     dashboardHeader(),
#'     dashboardSidebar(),
#'     dashboardBody(
#'       tags$style("body { background-color: ghostwhite}"),
#'       fluidRow(
#'         actionButton("toggle_box", "Toggle Box"),
#'         actionButton("remove_box", "Remove Box", class = "bg-danger"),
#'         actionButton("restore_box", "Restore Box", class = "bg-success")
#'       ),
#'       actionButton("update_box", "Update Box", class = "bg-info"),
#'       actionButton("update_box2", "Update Box 2", class = "bg-info"),
#'       br(),
#'       br(),
#'       box(
#'         title = textOutput("box_state"),
#'         id = "mybox",
#'         status = "danger",
#'         background = "maroon",
#'         solidHeader = TRUE,
#'         gradient = TRUE,
#'         collapsible = TRUE,
#'         closable = TRUE,
#'         plotOutput("plot")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$plot <- renderPlot({
#'       req(!input$mybox$collapsed)
#'       plot(rnorm(200))
#'     })
#'
#'     output$box_state <- renderText({
#'       state <- if (input$mybox$collapsed) "collapsed" else "uncollapsed"
#'       paste("My box is", state)
#'     })
#'
#'     observeEvent(input$toggle_box, {
#'       updateBox("mybox", action = "toggle")
#'     })
#'
#'     observeEvent(input$remove_box, {
#'       updateBox("mybox", action = "remove")
#'     })
#'
#'     observeEvent(input$restore_box, {
#'       updateBox("mybox", action = "restore")
#'     })
#'
#'     observeEvent(input$mybox$visible, {
#'       collapsed <- if (input$mybox$collapsed) "collapsed" else "uncollapsed"
#'       visible <- if (input$mybox$visible) "visible" else "hidden"
#'       message <- paste("My box is", collapsed, "and", visible)
#'       showNotification(message, type = "warning", duration = 1)
#'     })
#'
#'     observeEvent(input$update_box, {
#'       updateBox(
#'         "mybox",
#'         action = "update",
#'         options = list(
#'           title = h2("hello", dashboardBadge(1, color = "primary")),
#'           status = "warning",
#'           solidHeader = TRUE,
#'           width = 12,
#'           background = NULL,
#'           height = "900px",
#'           closable = FALSE
#'         )
#'       )
#'     })
#'
#'     observeEvent(input$update_box2, {
#'       updateBox(
#'         "mybox",
#'         action = "update",
#'         options = list(
#'           status = NULL,
#'           solidHeader = FALSE,
#'           width = 4,
#'           background = "green",
#'           height = "500px",
#'           closable = TRUE
#'         )
#'       )
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
updatebs4Card <- function(id, action = c("remove", "toggle", "toggleMaximize", "restore", "update"),
                          options = NULL, session = shiny::getDefaultReactiveDomain()) {
  action <- match.arg(action)
  # for update, we take a list of options
  if (action == "update") {
    # handle case whare options are shiny tag or a list of tags ...
    options <- lapply(options, function(o) {
      if (inherits(o, "shiny.tag") || inherits(o, "shiny.tag.list")) {
        o <- as.character(o)
        return(o)
      }
      if (inherits(o, "list")) {
        o <- unlist(
          dropNulls(
            lapply(o, function(e) {
              if (inherits(e, "shiny.tag.list") ||
                inherits(e, "shiny.tag")) {
                as.character(e)
              } else {
                e
              }
            })
          )
        )
      }
      o
    })
    message <- dropNulls(c(action = action, options = list(options)))
    session$sendInputMessage(id, message)
  } else {
    session$sendInputMessage(id, message = match.arg(action))
  }
  session$sendInputMessage(id, action)
}




#' Programmatically toggle a bs4Card sidebar
#'
#' @param id Card sidebar id.
#' @param session Shiny session object.
#'
#' @rdname boxSidebar
#'
#' @export
#' @examples
#' # Toggle a box sidebar
#' if (interactive()) {
#'   library(shiny)
#'   library(bs4Dash)
#'
#'   shinyApp(
#'     ui = dashboardPage(
#'       header = dashboardHeader(),
#'       body = dashboardBody(
#'         box(
#'           title = "Update box sidebar",
#'           closable = TRUE,
#'           width = 12,
#'           height = "500px",
#'           solidHeader = FALSE,
#'           collapsible = TRUE,
#'           actionButton("update", "Toggle card sidebar"),
#'           sidebar = boxSidebar(
#'             id = "mycardsidebar",
#'             p("Sidebar Content")
#'           )
#'         )
#'       ),
#'       sidebar = dashboardSidebar()
#'     ),
#'     server = function(input, output, session) {
#'       observe(print(input$mycardsidebar))
#'
#'       observeEvent(input$update, {
#'         updateBoxSidebar("mycardsidebar")
#'       })
#'     }
#'   )
#' }
updatebs4CardSidebar <- function(id, session = shiny::getDefaultReactiveDomain()) {
  session$sendInputMessage(id, NULL)
}




#' Create a box dropdown item list
#'
#' Can be used to add dropdown items to a cardtool.
#'
#' @param ... Slot for \link{cardDropdownItem}.
#' @param icon Dropdown menu icon.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @rdname boxDropdown
#' @family boxWidgets
#' @export
cardDropdown <- function(..., icon = shiny::icon("wrench")) {
  contentTag <- shiny::tags$div(
    class = "dropdown-menu dropdown-menu-right",
    role = "menu",
    ...
  )

  # for bs4Card toolbar
  toolbarTag <- shiny::tags$div(
    class = "btn-group",
    shiny::tags$button(
      type = "button",
      `data-toggle` = "dropdown",
      icon
    ),
    contentTag
  )

  toolbarTag
}




#' Create a box dropdown item
#'
#' To insert in \link{boxDropdown}.
#'
#' @param ... Item content.
#' @param id If passed, the item will behave like an action button.
#' @param href Target url or page.
#' @param icon Optional icon. Expect \link[shiny]{icon}.
#'
#' @rdname boxDropdown
#'
#' @export
cardDropdownItem <- function(..., id = NULL, href = NULL, icon = NULL) {
  shiny::tags$a(
    id = id,
    class = paste0("dropdown-item", if (!is.null(id)) " action-button"),
    href = if (!is.null(href)) href else "#",
    target = if (!is.null(href)) {
      "_blank"
    },
    icon,
    ...
  )
}



#' Create a box dropdown divider
#'
#' @note Useful to separate 2 sections of dropdown items.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
dropdownDivider <- function() {
  shiny::tags$a(class = "divider")
}





#' Boostrap 4 value box
#'
#' A beautiful AdminLTE3 value box.
#'
#' @param value The value to display in the box. Usually a number or short text.
#' @param subtitle Subtitle text.
#' @param icon An icon tag, created by \code{\link[shiny]{icon}}.
#' @param color The color of the item. This determines the item's background
#'   color.  Valid colors are defined as follows:
#' \itemize{
#'
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
#' @param width The width of the box, using the Bootstrap grid system. This is
#'   used for row-based layouts. The overall width of a region is 12, so the
#'   default width of 4 occupies 1/3 of that width. For column-based
#'   layouts, use \code{NULL} for the width; the width is set by the column that
#'   contains the box.
#' @param href An optional URL to link to in the footer. Should both `footer`
#'   and this parameter be set, `footer` will take precedence.
#' @param footer Optional html content for the footer of the box.
#' @param gradient Whether to use gradient style for background color. Default to FALSE.
#' @param elevation Value box elevation.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @rdname valueBox
#'
#' @family cards
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bs4Dash)
#'
#'   shinyApp(
#'     ui = dashboardPage(
#'       header = dashboardHeader(),
#'       sidebar = dashboardSidebar(),
#'       controlbar = dashboardControlbar(),
#'       footer = dashboardFooter(),
#'       title = "test",
#'       body = bs4DashBody(
#'         fluidRow(
#'           valueBox(
#'             value = 150,
#'             subtitle = "New orders",
#'             color = "primary",
#'             icon = icon("shopping-cart")
#'           ),
#'           valueBox(
#'             value = "53%",
#'             subtitle = "New orders",
#'             color = "indigo",
#'             icon = icon("cogs"),
#'             footer = div("Hello World")
#'           ),
#'           valueBox(
#'             value = "44",
#'             subtitle = "User Registrations",
#'             color = "teal",
#'             icon = icon("sliders")
#'           )
#'         )
#'       )
#'     ),
#'     server = function(input, output) {}
#'   )
#' }
#' @export
bs4ValueBox <- function(value, subtitle, icon = NULL, color = NULL, width = 3,
                        href = NULL, footer = NULL, gradient = FALSE, elevation = NULL) {
  if (!is.null(icon)) {
    tagAssert(icon, type = "i")
  }

  if (is.null(color) && gradient) {
    stop("color cannot be NULL when gradient is TRUE. 
         fill cannot be TRUE when color is NULL.")
  }

  # check conditions
  if (!is.null(width)) {
    stopifnot(is.numeric(width))
    # respect the bootstrap grid
    stopifnot(width <= 12)
    stopifnot(width >= 0)
  }

  if (!is.null(elevation)) {
    stopifnot(is.numeric(elevation))
    stopifnot(elevation < 6)
    stopifnot(elevation >= 0)
  }

  if (!is.null(footer) & !is.null(href)) {
    stop("Choose either href or footer.")
  }


  valueBoxCl <- "small-box"
  if (!is.null(color)) {
    validateStatusPlus(color)
    if (gradient) {
      valueBoxCl <- paste0(valueBoxCl, " bg-gradient-", color)
    } else {
      valueBoxCl <- paste0(valueBoxCl, " bg-", color)
    }
  }
  if (!is.null(elevation)) valueBoxCl <- paste0(valueBoxCl, " elevation-", elevation)

  innerTag <- shiny::tags$div(
    class = "inner",
    value,
    shiny::tags$p(class = "small-box-subtitle", subtitle)
  )

  iconTag <- if (!is.null(icon)) {
    shiny::tags$div(
      class = "icon",
      icon
    )
  } else {
    NULL
  }

  footerTag <- if (!is.null(footer)) {
    shiny::tags$div(
      class = "small-box-footer",
      footer
    )
  } else {
    if (!is.null(href)) {
      shiny::tags$a(
        href = href,
        target = "_blank",
        class = "small-box-footer",
        "More info",
        shiny::icon("arrow-circle-right")
      )
    } else {
      shiny::tags$div(class = "small-box-footer", style = "height: 30px;")
    }
  }

  valueBoxTag <- shiny::tags$div(class = valueBoxCl)
  valueBoxTag <- shiny::tagAppendChildren(valueBoxTag, innerTag, iconTag, footerTag)

  shiny::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    valueBoxTag
  )
}



#' Boostrap 4 info box
#'
#' A beautiful AdminLTE3 info box.
#'
#' @param title Info box title.
#' @param value The value to display in the box. Usually a number or short text.
#' @param subtitle Any extra UI element.
#' @param icon An icon tag, created by \code{\link[shiny]{icon}}.
#' @param color A color for the box. Valid colors are defined as follows:
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
#' @param width The width of the box, using the Bootstrap grid system. This is
#'   used for row-based layouts. The overall width of a region is 12, so the
#'   default width of 4 occupies 1/3 of that width. For column-based
#'   layouts, use \code{NULL} for the width; the width is set by the column that
#'   contains the box.
#' @param href An optional URL to link to.
#' @param fill If FALSE (the default), use a white background for the content, and
#' the color argument for the background of the icon. If TRUE, use the color argument
#' for the background of the content; the icon will use the same color with a slightly
#' darkened background.
#' @param gradient Whether to use gradient style for background color. Default to FALSE.
#' @param elevation Box elevation.
#' @param iconElevation Icon elevation compared to the main content (relief). 3 by default.
#' @param tabName Optional: \link{infoBox} behaves like \link{menuItem} and
#' may be used to navigate between multiple \link{tabItem}.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' @rdname infoBox
#'
#' @family cards
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bs4Dash)
#'
#'   shinyApp(
#'     ui = dashboardPage(
#'       header = dashboardHeader(),
#'       sidebar = dashboardSidebar(
#'         sidebarMenu(
#'           menuItem(
#'             text = "Item 1",
#'             tabName = "tab1"
#'           ),
#'           menuItem(
#'             text = "Item 2",
#'             tabName = "tab2"
#'           )
#'         )
#'       ),
#'       controlbar = dashboardControlbar(),
#'       footer = dashboardFooter(),
#'       title = "test",
#'       body = dashboardBody(
#'         tabItems(
#'           tabItem(
#'             tabName = "tab1",
#'             fluidRow(
#'               infoBox(
#'                 title = "Messages",
#'                 value = 1410,
#'                 icon = icon("envelope"),
#'                 color = "orange",
#'                 fill = TRUE,
#'               ),
#'               infoBox(
#'                 title = "Bookmarks",
#'                 color = "info",
#'                 value = 240,
#'                 icon = icon("bookmark"),
#'                 tabName = "tab2"
#'               )
#'             )
#'           ),
#'           tabItem(
#'             tabName = "tab2",
#'             infoBox(
#'               title = "Comments",
#'               color = "indigo",
#'               gradient = TRUE,
#'               value = 41410,
#'               subtitle = "A subtitle",
#'               icon = icon("comments"),
#'               tabName = "tab1"
#'             )
#'           )
#'         )
#'       )
#'     ),
#'     server = function(input, output) {}
#'   )
#' }
#' @export
bs4InfoBox <- function(title, value = NULL, subtitle = NULL, icon = shiny::icon("bar-chart"),
                       color = NULL, width = 4, href = NULL, fill = FALSE, gradient = FALSE,
                       elevation = NULL, iconElevation = NULL, tabName = NULL) {

  # check conditions
  tagAssert(icon, "i")
  if (!is.null(color)) validateStatusPlus(color)

  if (is.null(color) && (fill || gradient)) {
    stop("color cannot be NULL when gradient is TRUE. 
         fill cannot be TRUE when color is NULL.")
  }

  if (gradient && !fill) {
    stop("gradient cannot be TRUE when fill is FALSE.")
  }

  if (!is.null(width)) {
    stopifnot(is.numeric(width))
    # respect the bootstrap grid
    stopifnot(width <= 12)
    stopifnot(width >= 0)
  }

  if (!is.null(elevation)) {
    stopifnot(is.numeric(elevation))
    stopifnot(elevation < 6)
    stopifnot(elevation >= 0)
  }

  if (!is.null(iconElevation)) {
    stopifnot(is.numeric(iconElevation))
    stopifnot(iconElevation < 6)
    stopifnot(iconElevation >= 0)
  }

  infoBoxCl <- "info-box"
  if (!is.null(color)) {
    if (fill) {
      if (gradient) {
        infoBoxCl <- paste0(infoBoxCl, " bg-gradient-", color)
      } else {
        infoBoxCl <- paste0(infoBoxCl, " bg-", color)
      }
    }
  }

  if (!is.null(elevation)) infoBoxCl <- paste0(infoBoxCl, " elevation-", elevation)

  # icon is mandatory
  infoBoxIconCl <- "info-box-icon"
  if (!is.null(color)) {
    if (!fill) infoBoxIconCl <- paste0(infoBoxIconCl, " bg-", color)
  }
  if (!is.null(iconElevation)) infoBoxIconCl <- paste0(infoBoxIconCl, " elevation-", iconElevation)

  iconTag <- shiny::tags$span(
    class = infoBoxIconCl,
    id = if (!is.null(tabName)) paste0("icon-", tabName),
    # icon
    icon
  )


  contentTag <- shiny::tags$div(
    class = "info-box-content",
    shiny::tags$span(
      class = "info-box-text",
      title
    ),
    if (!is.null(value)) {
      shiny::tags$span(
        class = "info-box-number",
        value
      )
    },
    if (!is.null(subtitle)) shiny::p(subtitle)
  )

  if (!is.null(href)) {
    contentTag <- shiny::a(
      href = href,
      contentTag,
      target = "_blank",
      style = "color: inherit;"
    )
  }


  infoBoxTag <- shiny::tags$div(class = infoBoxCl)
  infoBoxTag <- shiny::tagAppendChildren(infoBoxTag, iconTag, contentTag)

  # handle tabName 
  infoBoxTag <- shiny::tagList(
    if (!is.null(tabName)) {
      shiny::singleton(
        shiny::tags$head(
          shiny::tags$script(
            shiny::HTML(
              paste0(
                "$(function() {
                    $('#icon-", tabName, "').on('click', function() {
                    $('#tab-", tabName, "').click();
                  });
                });
                "
              )
            )
          )
        )
      )
    },
    infoBoxTag
  )


  shiny::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    infoBoxTag
  )
}




#' Create a Boostrap 4 tabCard
#'
#' Build an adminLTE3 card with tabs
#'
#' @inheritParams bs4Card
#' @inheritParams tabsetPanel
#' @param side \link[shiny]{tabPanel} side. Either left or right.
#'
#' @family cards
#' @rdname tabBox
#'
#' @note User will access the \link{tabBox} input with input$<id>_box. This allows
#' to get the state of the box and update it on the server with \link{updateBox}.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bs4Dash)
#'
#'   shinyApp(
#'     ui = dashboardPage(
#'       header = dashboardHeader(),
#'       sidebar = dashboardSidebar(),
#'       controlbar = dashboardControlbar(),
#'       footer = dashboardFooter(),
#'       title = "tabBox",
#'       body = dashboardBody(
#'         tabBox(
#'           id = "tabcard",
#'           title = "A card with tabs",
#'           selected = "Tab 2",
#'           status = "primary",
#'           solidHeader = FALSE,
#'           type = "tabs",
#'           tabPanel(
#'             title = "Tab 1",
#'             "Content 1"
#'           ),
#'           tabPanel(
#'             title = "Tab 2",
#'             "Content 2"
#'           ),
#'           tabPanel(
#'             title = "Tab 3",
#'             "Content 3"
#'           )
#'         )
#'       )
#'     ),
#'     server = function(input, output) {}
#'   )
#' }
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4TabCard <- function(..., id = NULL, selected = NULL, title = NULL, width = 6,
                       height = NULL, side = c("left", "right"), type = NULL,
                       footer = NULL, status = NULL, solidHeader = FALSE, background = NULL,
                       collapsible = TRUE, collapsed = FALSE, closable = FALSE, maximizable = FALSE,
                       icon = NULL, gradient = FALSE, boxToolSize = "sm", elevation = NULL,
                       headerBorder = TRUE, label = NULL, dropdownMenu = NULL,
                       sidebar = NULL) {
  side <- match.arg(side)
  if (is.null(type)) type <- "pills"

  # Build tabs
  content <- tabsetPanel(
    ...,
    id = id,
    selected = selected,
    type = type,
    position = NULL
  )

  # Re-use box function
  boxTag <- box(
    content$children[[2]],
    title = title,
    footer = footer,
    status = status,
    solidHeader = solidHeader,
    background = background,
    width = width,
    height = height,
    collapsible = collapsible,
    collapsed = collapsed,
    closable = closable,
    maximizable = maximizable,
    icon = icon,
    gradient = gradient,
    boxToolSize = boxToolSize,
    elevation = elevation,
    headerBorder = headerBorder,
    label = label,
    dropdownMenu = dropdownMenu,
    sidebar = sidebar,
    id = NULL
  )

  # add card-tabs class
  boxTag$children[[1]]$attribs$class <- paste0(
    boxTag$children[[1]]$attribs$class,
    if (solidHeader) {
      " card-tabs"
    } else {
      " card-outline-tabs"
    }
  )

  # change header class
  boxTag$children[[1]]$children[[1]]$attribs$class <- paste0(
    boxTag$children[[1]]$children[[1]]$attribs$class,
    if (solidHeader) {
      " p-0 pt-1"
    } else {
      " p-0 border-bottom-0"
    }
  )


  # Remove title and add it to tab list
  titleTag <- boxTag$children[[1]]$children[[1]]$children[[1]]
  boxTag$children[[1]]$children[[1]]$children[[1]] <- NULL
  titleNavTag <- shiny::tags$li(
    class = "pt-2 px-3",
    titleTag
  )
  
  boxToolTag <- boxTag$children[[1]]$children[[1]]$children[[1]]
  boxTag$children[[1]]$children[[1]]$children[[1]] <- NULL
  
  if (side == "right") {
    content$children[[1]] <- tagInsertChild(
      content$children[[1]],
      titleNavTag,
      1
    )
  } else {
    content$children[[1]] <- tagInsertChild(
      content$children[[1]],
      titleNavTag,
      length(content$children[[1]])
    )
  }
  
  # Insert box tools at the end of the list
  content$children[[1]] <- tagInsertChild(
    content$children[[1]],
    shiny::tags$li(class = "ml-auto", boxToolTag),
    length(content$children[[1]])
  )

  # Insert tabs at different position in the header tag
  if (side == "right") {
    boxTag$children[[1]]$children[[1]] <- tagInsertChild(
      boxTag$children[[1]]$children[[1]],
      content$children[[1]],
      length(boxTag$children[[1]]$children[[1]])
    )
  } else {
    boxTag$children[[1]]$children[[1]] <- tagInsertChild(
      boxTag$children[[1]]$children[[1]],
      content$children[[1]],
      1
    )
  }

  # add custom input id. User will access the tabBox input with input$<tabset_id>_box
  boxTag$children[[1]]$attribs$id <- if (!is.null(id)) paste0(id, "_box")
  boxTag$children[[2]]$attribs$`data-for` <- if (!is.null(id)) paste0(id, "_box")
  boxTag
}




#' @title AdminLTE3 widget user card
#'
#' @description \link{userBox} creates a user card.
#'
#' @inheritParams bs4Card
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @rdname userBox
#' @family cards
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bs4Dash)
#'
#'   shinyApp(
#'     ui = dashboardPage(
#'       header = dashboardHeader(),
#'       sidebar = dashboardSidebar(),
#'       controlbar = dashboardControlbar(),
#'       footer = dashboardFooter(),
#'       title = "test",
#'       body = dashboardBody(
#'         userBox(
#'           title = userDescription(
#'             title = "Nadia Carmichael",
#'             subtitle = "lead Developer",
#'             type = 2,
#'             image = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",
#'           ),
#'           status = "primary",
#'           gradient = TRUE,
#'           background = "primary",
#'           boxToolSize = "xl",
#'           "Some text here!",
#'           footer = "The footer here!"
#'         ),
#'         userBox(
#'           title = userDescription(
#'             title = "Alexander Pierce",
#'             subtitle = "Founder & CEO",
#'             type = 1,
#'             image = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
#'           ),
#'           status = "indigo",
#'           closable = TRUE,
#'           "Some text here!",
#'           footer = "The footer here!"
#'         ),
#'         userBox(
#'           title = userDescription(
#'             title = "Elizabeth Pierce",
#'             subtitle = "Web Designer",
#'             image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
#'             backgroundImage = "https://cdn.statically.io/img/wallpaperaccess.com/full/1119564.jpg",
#'           ),
#'           status = "olive",
#'           closable = TRUE,
#'           maximizable = TRUE,
#'           "Some text here!",
#'           footer = "The footer here!"
#'         )
#'       )
#'     ),
#'     server = function(input, output) {}
#'   )
#' }
#' @export
bs4UserCard <- function(..., title = NULL, footer = NULL, status = NULL,
                        background = NULL, width = 6, height = NULL,
                        collapsible = TRUE, collapsed = FALSE, closable = FALSE, maximizable = FALSE,
                        gradient = FALSE, boxToolSize = "sm", elevation = NULL, headerBorder = TRUE,
                        label = NULL, dropdownMenu = NULL, sidebar = NULL, id = NULL) {

  # userBox is built on top of the box function. The difference is the title tag
  # that is replaced by userDescription ...
  boxTag <- box(
    ...,
    title = title,
    footer = footer,
    status = status,
    solidHeader = TRUE,
    background = background,
    width = width,
    height = height,
    collapsible = collapsible,
    collapsed = collapsed,
    closable = closable,
    maximizable = maximizable,
    icon = NULL,
    gradient = gradient,
    boxToolSize = boxToolSize,
    elevation = elevation,
    headerBorder = headerBorder,
    label = label,
    dropdownMenu = dropdownMenu,
    sidebar = sidebar,
    id = id
  )
  
  # remove status class from box that is not necessary for userBox
  if (!is.null(status)) {
    temp_pattern <- paste0("card-", status)
    boxTag$children[[1]]$attribs$class <- gsub(temp_pattern, "", boxTag$children[[1]]$attribs$class) 
  }

  # find the selected type
  type <- title[[2]]

  # specific class for userDescription
  boxTag$children[[1]]$attribs$class <- paste0(boxTag$children[[1]]$attribs$class, " card-widget user-card")
  if (!is.null(type)) {
    boxTag$children[[1]]$attribs$class <- paste0(boxTag$children[[1]]$attribs$class, " widget-user-", type)
  } else {
    boxTag$children[[1]]$attribs$class <- paste0(boxTag$children[[1]]$attribs$class, " widget-user")
  }


  # Change color
  if (!is.null(status)) {
    if (gradient) {
      if (inherits(title[[1]], "shiny.tag.list")) {
        title[[1]][[1]]$attribs$class <- paste0(title[[1]][[1]]$attribs$class, " bg-gradient-", status)
      } else {
        title[[1]]$attribs$class <- paste0(title[[1]]$attribs$class, " bg-gradient-", status)
      }
    } else {
      if (inherits(title[[1]], "shiny.tag.list")) {
        title[[1]][[1]]$attribs$class <- paste0(title[[1]][[1]]$attribs$class, " bg-", status)
      } else {
        title[[1]]$attribs$class <- paste0(title[[1]]$attribs$class, " bg-", status)
      }
    }
  }


  # recover box tools
  boxTools <- boxTag$children[[1]]$children[[1]]$children[[2]]

  # replace title tag by the user widget
  boxTag$children[[1]]$children[[1]] <- title[[1]]

  # inject box tools
  if (inherits(boxTag$children[[1]]$children[[1]], "shiny.tag.list")) {
    boxTag$children[[1]]$children[[1]][[1]] <- tagInsertChild(
      boxTag$children[[1]]$children[[1]][[1]],
      boxTools,
      1
    )
  } else {
    boxTag$children[[1]]$children[[1]] <- tagInsertChild(
      boxTag$children[[1]]$children[[1]],
      boxTools,
      1
    )
  }

  boxTag
}

#' User Description
#'
#' \link{userDescription} creates a customized title tag for \link{userBox}.
#'
#' @param title User card title.
#' @param subtitle User card subtitle.
#' @param image User image url or path.
#' @param backgroundImage image url, if any. Background needs to be TRUE.
#' @param type User card type. Either 1 or 2. 1 corresponds to a centered user image,
#' while 2 is a left aligned user image.
#' @param imageElevation User card image elevation (numeric). NULL by default.
#'
#' @rdname userBox
#' @export
bs4UserDescription <- function(title, subtitle = NULL, image, backgroundImage = NULL,
                               type = c(1, 2), imageElevation = NULL) {
  headerCl <- "widget-user-header"

  # if type is not explicitly provided, it will use the default value, c(1, 2).
  # Below we ensure that whenever it is the case, we only select the first element
  # by default. We also need to convert to character for match.arg
  if (length(type) == 2) {
    type <- as.character(type[1])
    type <- match.arg(type)
  }

  # once type is assigned, if it is "1" we actually put it back to NULL since
  # the class widget-user-1 does not exist (only widget-user-2).
  if (!is.null(type)) {
    type <- as.character(type)
    type <- match.arg(type)
    if (type == "1") type <- NULL
  }

  headerImageTag <- shiny::tags$div(
    class = "widget-user-image",
    shiny::tags$img(
      class = if (!is.null(imageElevation)) {
        paste0("img-circle elevation-", imageElevation)
      } else {
        "img-circle"
      },
      src = image,
      alt = "User Avatar"
    )
  )

  if (!is.null(backgroundImage)) headerCl <- paste0(headerCl, " bg-black")

  userDescriptionTag <- if (is.null(type)) {
    shiny::tagList(
      shiny::tags$div(
        class = headerCl,
        style = if (!is.null(backgroundImage)) {
          paste0("background: url('", backgroundImage, "') center center;")
        },
        # title and subtitle
        shiny::tags$h3(class = "widget-user-username", title),
        if (!is.null(subtitle)) shiny::tags$h5(class = "widget-user-desc", subtitle)
      ),
      headerImageTag
    )
  } else {
    shiny::tags$div(
      class = headerCl,
      style = if (!is.null(backgroundImage)) {
        paste0("background: url('", backgroundImage, "') center center;")
      },
      headerImageTag,
      # title and subtitle
      shiny::tags$h3(class = "widget-user-username", title),
      if (!is.null(subtitle)) shiny::tags$h5(class = "widget-user-desc", subtitle)
    )
  }

  list(userDescriptionTag, type)
}





#' AdminLTE3 card profile
#'
#' \link{boxProfile} goes inside a \link{box}. Displays user informations in an elegant
#' container.
#'
#' @param ... Any element such as \link{boxProfileItem}.
#' @param image Profile image, if any.
#' @param title Title.
#' @param subtitle Subtitle.
#' @param bordered Whether the container should have a border or not. FALSE by default.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bs4Dash)
#'
#'   shinyApp(
#'     ui = dashboardPage(
#'       header = dashboardHeader(),
#'       sidebar = dashboardSidebar(
#'         sidebarMenu(
#'           sidebarHeader("Main content"),
#'           menuItem(
#'             "Profile Card",
#'             tabName = "profile_card",
#'             icon = icon("desktop")
#'           )
#'         )
#'       ),
#'       controlbar = dashboardControlbar(),
#'       footer = dashboardFooter(),
#'       title = "boxProfile",
#'       body = dashboardBody(
#'         tabItems(
#'           tabItem(
#'             tabName = "profile_card",
#'             bs4Card(
#'               status = "primary",
#'               solidHeader = TRUE,
#'               boxProfile(
#'                 image = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
#'                 title = "Nina Mcintire",
#'                 subtitle = "Software Engineer",
#'                 bordered = TRUE,
#'                 boxProfileItem(
#'                   title = "Followers",
#'                   description = 1322
#'                 ),
#'                 boxProfileItem(
#'                   title = "Following",
#'                   description = 543
#'                 ),
#'                 boxProfileItem(
#'                   title = "Friends",
#'                   description = 13287
#'                 )
#'               )
#'             )
#'           )
#'         )
#'       )
#'     ),
#'     server = function(input, output) {}
#'   )
#' }
#' @rdname boxProfile
#' @family boxWidgets
#' @export
cardProfile <- function(..., image = NULL, title, subtitle = NULL, bordered = FALSE) {
  cl <- if (bordered) "list-group" else "list-group list-group-unbordered"

  shiny::tags$div(
    class = "card-profile",
    if (!is.null(image)) {
      shiny::tags$div(
        class = "text-center",
        shiny::img(class = "profile-user-img img-fluid img-circle", src = image)
      )
    },
    shiny::h3(class = "profile-username text-center", title),
    if (!is.null(subtitle)) shiny::p(class = "text-muted text-center", subtitle),
    shiny::tags$ul(
      class = cl,
      ...
    )
  )
}


#' @title AdminLTE3 card profile item
#'
#' @description Create card profile item
#'
#' @param title Item title.
#' @param description Item info.
#'
#' @rdname boxProfile
#'
#' @export
cardProfileItem <- function(title, description) {
  shiny::tags$li(
    class = "list-group-item",
    shiny::strong(title),
    shiny::a(class = "float-right", description)
  )
}




#' @title AdminLTE3 social card
#'
#' @description \link{socialBox} Creates social card
#'
#' @inheritParams bs4Card
#'
#' @rdname socialBox
#' @family cards
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bs4Dash)
#'
#'   shinyApp(
#'     ui = dashboardPage(
#'       dashboardHeader(),
#'       dashboardSidebar(),
#'       dashboardBody(
#'         socialBox(
#'           title = userBlock(
#'             image = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
#'             title = "Social Box",
#'             subtitle = "example-01.05.2018"
#'           ),
#'           "Some text here!",
#'           attachmentBlock(
#'             image = "https://adminlte.io/themes/AdminLTE/dist/img/photo1.png",
#'             title = "Test",
#'             href = "https://google.com",
#'             "This is the content"
#'           ),
#'           lapply(X = 1:10, FUN = function(i) {
#'             boxComment(
#'               image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
#'               title = paste("Comment", i),
#'               date = "01.05.2018",
#'               paste0("The ", i, "-th comment")
#'             )
#'           }),
#'           footer = "The footer here!"
#'         )
#'       ),
#'       controlbar = dashboardControlbar(),
#'       title = "socialBox"
#'     ),
#'     server = function(input, output) { }
#'   )
#' }
#' @export
bs4SocialCard <- function(..., title = NULL, footer = NULL, width = 6, height = NULL,
                          collapsible = TRUE, collapsed = FALSE, closable = FALSE, maximizable = FALSE,
                          boxToolSize = "sm", elevation = NULL, headerBorder = TRUE, label = NULL, dropdownMenu = NULL,
                          sidebar = NULL, id = NULL) {
  items <- list(...)
  # recover comments

  comments <- extractSocialItem(items)
  otherItems <- extractSocialItem(items, FALSE)

  # userBox is built on top of the box function. The difference is the title tag
  # that is replaced by userDescription ...
  boxTag <- box(
    ...,
    title = title,
    footer = footer,
    width = width,
    height = height,
    collapsible = collapsible,
    collapsed = collapsed,
    closable = closable,
    maximizable = maximizable,
    icon = NULL,
    gradient = FALSE,
    boxToolSize = boxToolSize,
    elevation = elevation,
    headerBorder = headerBorder,
    label = label,
    dropdownMenu = dropdownMenu,
    sidebar = sidebar,
    id = id
  )

  # specific class
  boxTag$children[[1]]$attribs$class <- paste0(boxTag$children[[1]]$attribs$class, " card-widget social-card")

  # replace title tag by the user widget
  boxTag$children[[1]]$children[[1]]$children[[1]] <- title


  # inject any comments
  if (length(comments) > 0) {
    commentsTag <- shiny::tags$div(
      class = "card-footer card-comments",
      style = "overflow-y: auto; max-height: 150px; display: block;",
      comments
    )

    # insert in boxTag structure
    boxTag$children[[1]]$children[[2]]$children <- otherItems
    boxTag$children[[1]] <- tagInsertChild(
      boxTag$children[[1]],
      commentsTag,
      3
    )
  }

  boxTag
}



#' User block
#'
#' \link{userBlock} goes in the title of \link{socialBox}.
#'
#' @param image User image.
#' @param title A title, user name,...
#' @param subtitle Any subtitle.
#'
#' @rdname socialBox
#' @family boxWidgets
#'
#' @export
userBlock <- function(image, title, subtitle = NULL) {
  shiny::tags$div(
    class = "user-block",
    shiny::img(class = "img-circle", src = image),
    shiny::tags$span(
      class = "username",
      shiny::a(href = "javascript:void(0)", title)
    ),
    if (!is.null(subtitle)) shiny::tags$span(class = "description", subtitle)
  )
}



#' @title BS4 card comment container
#'
#' @description Create a card comment to insert in \link{socialBox}
#'
#' @param ... Comment content.
#' @param image Author image, if any.
#' @param title Comment title.
#' @param date Date of publication.
#'
#' @rdname socialBox
#' @family boxWidgets
#'
#' @export
cardComment <- function(..., image, title = NULL, date = NULL) {
  comment <- list(...)
  if (length(comment) == 0) stop("You must enter a comment.")

  cardCommentTag <- shiny::tags$div(
    class = "card-comment",
    shiny::img(class = "img-circle img-sm", src = image),
    shiny::tags$div(
      class = "comment-text",
      shiny::tags$span(
        class = "username",
        title,
        if (!is.null(date)) shiny::tags$span(class = "text-muted float-right", date)
      ),
      ...
    )
  )

  class(cardCommentTag) <- c(class(cardCommentTag), "card-comment")
  cardCommentTag
}




#' Bootstrap 4 container for cards
#'
#' @param ... Slot for bs4Dash cards.
#' @param type Container type. See \url{https://getbootstrap.com/docs/4.0/components/card/#card-layout}
#' for more details.
#' @export
#'
#' @family cards
#'
#' @note Cards must have width argument set to NULL.
#' @rdname boxLayout
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bs4Dash)
#'
#'   # with group
#'   shinyApp(
#'     ui = dashboardPage(
#'       header = dashboardHeader(),
#'       sidebar = dashboardSidebar(),
#'       body = dashboardBody(
#'         boxLayout(
#'           type = "group",
#'           lapply(1:4, function(i) {
#'             box(
#'               width = NULL,
#'               title = paste("Card", i),
#'               closable = FALSE,
#'               collapsible = FALSE,
#'               "Lorem ipsum is so fun!"
#'             )
#'           })
#'         )
#'       ),
#'       controlbar = dashboardControlbar(),
#'       title = "Box layout group"
#'     ),
#'     server = function(input, output) {}
#'   )
#'
#'   # with deck
#'   shinyApp(
#'     ui = dashboardPage(
#'       header = dashboardHeader(),
#'       sidebar = dashboardSidebar(),
#'       body = dashboardBody(
#'         boxLayout(
#'           type = "deck",
#'           lapply(1:4, function(i) {
#'             box(
#'               width = NULL,
#'               title = paste("Card", i),
#'               closable = FALSE,
#'               collapsible = FALSE,
#'               "Lorem ipsum is so fun!"
#'             )
#'           })
#'         )
#'       ),
#'       controlbar = dashboardControlbar(),
#'       title = "Box layout deck"
#'     ),
#'     server = function(input, output) {}
#'   )
#'
#'   # with columns
#'   shinyApp(
#'     ui = dashboardPage(
#'       header = dashboardHeader(),
#'       sidebar = dashboardSidebar(),
#'       body = dashboardBody(
#'         boxLayout(
#'           type = "columns",
#'           lapply(1:12, function(i) {
#'             box(
#'               width = NULL,
#'               title = paste("Card", i),
#'               closable = FALSE,
#'               collapsible = FALSE,
#'               height = if (i %% 2 == 1) "200px",
#'               status = if (i %% 2 == 0) "primary",
#'               if (i %% 2 == 0) "Lorem ipsum is so fun!",
#'               if (i == 1 | i == 7 | i == 12) img(src = "https://via.placeholder.com/290x160")
#'             )
#'           })
#'         )
#'       ),
#'       controlbar = dashboardControlbar(),
#'       title = "Box layout columns"
#'     ),
#'     server = function(input, output) {}
#'   )
#' }
bs4CardLayout <- function(..., type = c("group", "deck", "columns")) {
  cards <- list(...)
  if (inherits(cards[[1]], "list")) cards <- cards[[1]]
  # stop if width is accidentally passed
  cards <- lapply(seq_along(cards), function(i) {
    if (length(grep("col-sm", cards[[i]]$attribs$class)) > 0) {
      stop("The card width parameter must be NULL")
    } else {
      cards[[i]]
    }
  })
  type <- match.arg(type)
  shiny::tags$div(class = paste0("card-", type), cards)
}