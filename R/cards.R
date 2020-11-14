#' Create a Boostrap 4 card
#'
#' Build an adminLTE3 card
#'
#' @param ... Contents of the box.
#' @param title Optional title.
#' @param footer Optional footer text.
#' @param status The status of the item This determines the item's background
#'   color.  Valid statuses are defined as follows:
#' \itemize{
#' 
#'   \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item \code{black}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#000000")}.
#'   \item \code{gray-dark}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item \code{gray}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item \code{light}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#1f2d3d")}.
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
#' Only primary, success, info, warning and danger are compatible with solidHeader!
#' @param solidHeader Should the header be shown with a solid color background?
#' @param background If NULL (the default), the background of the box will be
#'   white. Otherwise, a color string. Valid colors are listed in
#'   \link{validColors}. See below:
#' \itemize{
#'  \item \code{indigo}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6610f2")}.
#'  \item \code{lightblue}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3c8dbc")}.
#'  \item \code{navy}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#001f3f")}.
#'  \item \code{purple}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#605ca8")}.
#'  \item \code{fuchsia}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#f012be")}.
#'  \item \code{pink}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#e83e8c")}.
#'  \item \code{maroon}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#d81b60")}.
#'  \item \code{orange}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ff851b")}.
#'  \item \code{lime}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#01ff70")}.
#'  \item \code{teal}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#39cccc")}.
#'  \item \code{olive}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3d9970")}.
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
#' @param icon Header icon. Displayed before title. Expect \code{\link[shiny]{icon}}.
#' @param gradient Whether to allow gradient effect for the background color. Default to FALSE.
#' @param cardToolSize Size of the toolbox: choose among "xs", "sm", "md", "lg".
#' @param maximizable If TRUE, the card can be displayed in full screen mode.
#' @param elevation Card elevation. 
#' @param headerBorder Whether to display a border between the header and body.
#' TRUE by default
#' @param label Slot for \link{cardLabel}.
#' @param dropdownMenu List of items in the boxtool dropdown menu. Use \link{boxDropdown}.
#' @param sidebar Slot for \link{cardSidebar}.
#' @param id Box unique id. \link{updateCard} target.
#'
#' @rdname card
#'
#' @examples
#' # A box with label, sidebar, dropdown menu
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'
#'  shinyApp(
#'    ui = dashboardPage(
#'      dashboardHeader(),
#'      dashboardSidebar(),
#'      dashboardBody(
#'       box(
#'         title = "Closable Box with dropdown", 
#'         closable = TRUE, 
#'         width = 12,
#'         status = "warning", 
#'         solidHeader = FALSE, 
#'         collapsible = TRUE,
#'         label = boxLabel(
#'          text = 1,
#'          status = "danger"
#'         ),
#'         dropdownMenu = boxDropdown(
#'          boxDropdownItem("Link to google", href = "http://www.google.com"),
#'          boxDropdownItem("item 2", href = "#"),
#'          dropdownDivider(),
#'          boxDropdownItem("item 3", href = "#", icon = icon("th"))
#'         ),
#'         sidebar = boxSidebar(
#'          startOpen = TRUE,
#'          id = "mycardsidebar",
#'          sliderInput(
#'           "obs", 
#'           "Number of observations:",
#'           min = 0, 
#'           max = 1000, 
#'           value = 500
#'          )
#'         ),
#'         plotOutput("distPlot")
#'        )
#'      )
#'    ),
#'    server = function(input, output) {
#'     output$distPlot <- renderPlot({
#'      hist(rnorm(input$obs))
#'     })
#'    }
#'  )
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4Card <- function(..., title = NULL, footer = NULL, status = NULL, 
                    solidHeader = FALSE, background = NULL, width = 6, height = NULL, 
                    collapsible = TRUE, collapsed = FALSE, closable = FALSE, icon = NULL, 
                    gradient = FALSE, cardToolSize = "sm", maximizable = FALSE, 
                    elevation = NULL, headerBorder = TRUE, label = NULL, dropdownMenu = NULL, 
                    sidebar = NULL, id = NULL) {
  
  props <- dropNulls(
    list(
      title = as.character(title),
      status = status,
      solidHeader = solidHeader,
      background = background,
      width = width,
      height = height,
      collapsible = collapsible,
      closable = closable,
      gradient = gradient
    )
  )
  
  if (is.null(title) && 
      (!is.null(label) || !is.null(sidebar) || !is.null(dropdownMenu))) {
    stop("Cannot use box tools without a title")
  }
  
  if (!collapsible & collapsed) {
    stop("Cannot collapse a card that is not collapsible.")
  }
  
  if (is.null(status) & solidHeader) stop("solidHeader cannot be used when status is NULL.")
  if (gradient && is.null(background)) stop("gradient cannot be used when background is NULL.")
  
  
  if (!is.null(elevation)) {
    stopifnot(is.numeric(elevation))
    stopifnot(elevation < 6)
    stopifnot(elevation >= 0)
  }
  
  if (!is.null(width)) {
    stopifnot(is.numeric(width))
    # respect the bootstrap grid
    stopifnot(width <= 12)
    stopifnot(width >= 0)
  }
  
  
  cardCl <- "card"
  
  if (!is.null(status)) {
    validateStatusPlus(status)
    cardCl <- paste0(cardCl, " card-", status)
  }
  
  if (!solidHeader) cardCl <- paste0(cardCl, " card-outline")
  
  if (collapsible && collapsed) cardCl <- paste0(cardCl, " collapsed-card")
  if (!is.null(elevation)) cardCl <- paste0(cardCl, " elevation-", elevation)
  
  if (!is.null(background)) {
    validateColor(background)
    cardCl <- paste0(cardCl, " bg-", if (gradient) "gradient-", background)
  }
  
  
  if (!is.null(sidebar)) {
    sidebarToggle <- sidebar[[2]]
    startOpen <- sidebar[[2]]$attribs$`data-start-open`
    if (startOpen == "true") {
      cardCl <- paste0(cardCl, " direct-chat direct-chat-contacts-open")
    } else {
      cardCl <- paste0(cardCl, " direct-chat")
    }
  }
  
  
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", shiny::validateCssUnit(height))
  }
  # add padding if box sidebar
  if (!is.null(sidebar)) {
    style <- paste(style, "padding: 10px;")
  }
  
  
  cardToolTag <- NULL
  
  if (collapsible || closable || !is.null(dropdownMenu) || 
      !is.null(label) || !is.null(sidebar)) {
    btnToolClass <- "btn"
    btnToolClass <- if (
      !is.null(status) || 
      (is.null(status) && is.null(background))
    ) {
      paste(btnToolClass, "btn-tool")
    } else {
      paste0(
        btnToolClass, 
        if (!is.null(background)) paste0(" bg-", background),
        " btn-", cardToolSize
      )
    }
    
    cardToolTag <- shiny::tags$div(class = "card-tools pull-right")
  }
  
  
  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- status %OR% "default"
    collapseIcon <- if (collapsed) 
      "plus"
    else "minus"
    collapseTag <- shiny::tags$button(
      class = btnToolClass, 
      `data-card-widget` = "collapse", 
      shiny::icon(collapseIcon)
    )
  }
  
  closableTag <- NULL
  if (closable) {
    closableTag <- shiny::tags$button(
      class = btnToolClass, 
      `data-card-widget` = "remove", 
      type = "button",
      shiny::icon("times")
    )
  } 
  
  maximizableTag <- NULL
  if (maximizable) {
    maximizableTag <- shiny::tags$button(
      type = "button",
      class = "btn btn-tool",
      `data-card-widget` = "maximize",
      shiny::icon("expand")
    )
  }
  
  # Modify sidebar trigger class if background ...
  if (!is.null(sidebar)) {
    if (is.null(status) && !is.null(background)) {
      sidebar[[2]]$attribs$class <- paste0(
        "btn",
        if (!is.null(background)) {
          paste0(" bg-", background)
        },
        " btn-", cardToolSize
      ) 
    }
  }
  
  # modify dropdown trigger if background
  if (!is.null(dropdownMenu)) {
    if (is.null(status) && !is.null(background)) {
      dropdownMenu$children[[1]]$attribs$class <- paste0(
        "btn", 
        paste0(
          if (!is.null(background)) {
            paste0(" bg-", background)
          },
          " btn-", cardToolSize
        ), 
        " dropdown-toggle"
      )
    }
    
  }
  
  # update boxToolTag
  cardToolTag <- shiny::tagAppendChildren(
    cardToolTag, 
    label, 
    dropdownMenu, 
    collapseTag, 
    closableTag,
    maximizableTag,
    sidebar[[2]]
  )
  
  
  
  # header
  if (is.null(title) & (maximizable | closable | collapsible)) title <- "\u200C"
  
  headerTag <- shiny::tags$div(
    class = if (headerBorder) "card-header" else "card-header no-border",
    icon, 
    if (!is.null(title)) shiny::tags$h3(class = "card-title", title) else NULL
  )
  headerTag <- if (!is.null(title)) shiny::tagAppendChild(headerTag, cardToolTag)
  
  
  # body
  bodyTag <- shiny::tags$div(
    class = "card-body",
    style = style,
    ...,
    sidebar[c(1, 3)]
  )
  
  footerTag <- if (!is.null(footer)) {
    shiny::tags$div(
      class = "card-footer",
      footer
    ) 
  }
  
  cardTag <- shiny::tags$div(class = cardCl, id = id)
  cardTag <- shiny::tagAppendChildren(cardTag, headerTag, bodyTag, footerTag)
  
  shiny::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    cardTag,
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
#' To insert in the sidebar slot of \link{bs4Card}.
#'
#' @param ... Sidebar content.
#' @param id Unique sidebar id. Useful if you want to use \link{updatebs4CardSidebar}.
#' @param width Sidebar width in percentage. 25\% by default. A character value of any width CSS understands (e.g. "100px").
#' @param background Sidebar background color. Dark by default.
#' @param startOpen Whether the sidebar is open at start. FALSE by default.
#' @param icon Sidebar icon. Expect \code{\link[shiny]{icon}}.
#' 
#' 
#' @rdname cardSidebar
#' @export
bs4CardSidebar <- function(..., id = NULL, width = "25%", background = "#333a40", 
                           startOpen = FALSE, icon = shiny::icon("cogs")) {
  
  # Toggle to insert in bs4Card
  toolbarTag <- shiny::tags$button(
    class = "btn btn-tool",
    id = id,
    `data-widget` = "chat-pane-toggle",
    `data-toggle` = "tooltip",
    `data-original-title` = "More",
    `data-start-open` = tolower(startOpen),
    type = "button",
    icon
  )
  
  # sidebar content
  contentTag <- shiny::tags$div(
    style = "z-index: 1;",
    class = "direct-chat-contacts",
    shiny::tags$ul(
      class = "contacts-list", 
      shiny::tags$li(
        style = paste0("width: ", width, ";"), 
        ...
      )
    )
  )
  
  # custom CSS
  translation_rate <- paste0("calc(100% - ", width, ")")
  sidebarCSS <- shiny::singleton(
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          paste0(
            ".direct-chat-contacts {
                -webkit-transform: translate(100%, 0);
                -ms-transform: translate(100%, 0);
                -o-transform: translate(100%, 0);
                transform: translate(100%, 0);
                position: absolute;
                top: 0;
                bottom: 0;
                height: 100%;
                width: 100%;
                background: ", background, ";
                color: #fff;
                overflow: auto;
              }
              .direct-chat-contacts-open .direct-chat-contacts {
                -webkit-transform: translate(", translation_rate, ", 0);
                -ms-transform: translate(", translation_rate, ", 0);
                -o-transform: translate(", translation_rate, ", 0);
                transform: translate(", translation_rate, ", 0);
              }
              "
          )
        )
      )
    )
  )
  
  shiny::tagList(sidebarCSS, toolbarTag, contentTag)
}




#' Update an AdminLTE3 card from the server side
#'
#' @param id Card inputId.
#' @param action Action to trigger: \code{c("remove", "toggle", "toggleMaximize", "restore", "update")}.
#' @param options If action is update, a list of new options to configure the box, such as
#' \code{list(title = "new title", status = NULL, solidHeader = FALSE, 
#' background = "red", width = 6, height = "200px", collapsible = FALSE, closable = FALSE)}.
#' @param session Shiny session.
#' 
#' @export
#' 
#' @rdname card
#'
#' @examples
#' # Toggle a box on the client
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  ui <- dashboardPage(
#'    dashboardHeader(),
#'    dashboardSidebar(),
#'    dashboardBody(
#'      tags$style("body { background-color: ghostwhite}"),
#'      fluidRow(
#'        actionButton("toggle_box", "Toggle Box"),
#'        actionButton("remove_box", "Remove Box", class = "bg-danger"),
#'        actionButton("restore_box", "Restore Box", class = "bg-success")
#'      ),
#'      actionButton("update_box", "Update Box", class = "bg-info"), 
#'      actionButton("update_box2", "Update Box 2", class = "bg-info"),
#'      br(),
#'      br(),
#'      box(
#'        title = textOutput("box_state"),
#'        id = "mybox",
#'        status = "danger", 
#'        background = "maroon", 
#'        gradient = TRUE,
#'        collapsible = TRUE,
#'        closable = TRUE,
#'        plotOutput("plot")
#'      )
#'    )
#'  )
#'  
#'  server <- function(input, output, session) {
#'    output$plot <- renderPlot({
#'      req(!input$mybox$collapsed)
#'      plot(rnorm(200))
#'    })
#'    
#'    output$box_state <- renderText({
#'      state <- if (input$mybox$collapsed) "collapsed" else "uncollapsed"
#'      paste("My box is", state)
#'    })
#'    
#'    observeEvent(input$toggle_box, {
#'      updateBox("mybox", action = "toggle")
#'    })
#'    
#'    observeEvent(input$remove_box, {
#'      updateBox("mybox", action = "remove")
#'    })
#'    
#'    observeEvent(input$restore_box, {
#'      updateBox("mybox", action = "restore")
#'    })
#'    
#'    observeEvent(input$mybox$visible, {
#'      collapsed <- if (input$mybox$collapsed) "collapsed" else "uncollapsed"
#'      visible <- if (input$mybox$visible) "visible" else "hidden"
#'      message <- paste("My box is", collapsed, "and", visible)
#'      showNotification(message, type = "warning", duration = 1)
#'    })
#'    
#'    observeEvent(input$update_box, {
#'      updateBox(
#'        "mybox", 
#'        action = "update", 
#'        options = list(
#'          title = tagList(h2("hello"), dashboardLabel(1, status = "primary")),
#'          status = "warning", 
#'          solidHeader = TRUE, 
#'          width = 12, 
#'          background = NULL, 
#'          height = "900px", 
#'          closable = FALSE
#'        )
#'      )
#'    })
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
#'    
#'  }
#'  
#'  shinyApp(ui, server)
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
#' @rdname cardSidebar
#' 
#' @export
#' @examples
#' # Toggle a box sidebar
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     header = dashboardHeader(),
#'     body = dashboardBody(
#'       box(
#'         title = "Update box sidebar", 
#'         closable = TRUE, 
#'         width = 12,
#'         height = "500px",
#'         solidHeader = FALSE, 
#'         collapsible = TRUE,
#'         actionButton("update", "Toggle card sidebar"),
#'         sidebar = boxSidebar(
#'           id = "mycardsidebar",
#'           p("Sidebar Content")
#'         )
#'       )
#'     ),
#'     sidebar = dashboardSidebar()
#'   ),
#'   server = function(input, output, session) {
#'     observe(print(input$mycardsidebar))
#'     
#'     observeEvent(input$update, {
#'       updateBoxSidebar("mycardsidebar")
#'     })
#'     
#'   }
#'  )
#' }
updatebs4CardSidebar <- function(id, session = shiny::getDefaultReactiveDomain()) {
  session$sendInputMessage(id, NULL)
}




#' Create a box dropdown item list
#'
#' Can be used to add dropdown items to a cardtool.
#'
#' @param ... Slot for \link{dropdownItem}.
#' @param icon Dropdown menu icon.
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
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
      class = "btn btn-tool dropdown-toggle",
      `data-toggle` = "dropdown",
      icon
    ),
    contentTag
  )
  
  toolbarTag
  
}




#' Create a box dropdown item 
#'
#' @param ... Item content.
#' @param id If passed, the item will behave like an action button.
#' @param href Target url or page.
#' @param icon Optional icon. Expect \link[shiny]{icon}.
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
cardDropdownItem <- function(..., id = NULL, href = NULL, icon = NULL) {
  shiny::tags$a(
    id = id,
    class = paste0("dropdown-item", if (!is.null(id)) " action-button"),
    href = href,
    target = "_blank",
    target = if (!is.null(href)) {
      "_blank"
    } else {
      "#"
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
#' @param elevation Value box elevation. 
#' @param status A color for the box. "primary", "info", "success", "warning", "danger" or NULL.
#' @param width The width of the box, using the Bootstrap grid system. This is
#'   used for row-based layouts. The overall width of a region is 12, so the
#'   default width of 4 occupies 1/3 of that width. For column-based
#'   layouts, use \code{NULL} for the width; the width is set by the column that
#'   contains the box.
#' @param footer Optional html content for the footer of the box.
#' @param href An optional URL to link to in the footer. Should both `footer`
#'   and this parameter be set, `footer` will take precedence. 
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @family cards
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'      navbar = bs4DashNavbar(),
#'      sidebar = bs4DashSidebar(),
#'      controlbar = bs4DashControlbar(),
#'      footer = bs4DashFooter(),
#'      title = "test",
#'      body = bs4DashBody(
#'       fluidRow(
#'        bs4ValueBox(
#'         value = 150,
#'         subtitle = "New orders",
#'         status = "primary",
#'         icon = "shopping-cart",
#'         href = "#"
#'        ),
#'        bs4ValueBox(
#'         value = "53%",
#'         subtitle = "New orders",
#'         status = "danger",
#'         icon = "cogs",
#'         footer = shiny::div("Hello World")
#'        ),
#'        bs4ValueBox(
#'         value = "44",
#'         subtitle = "User Registrations",
#'         status = "warning",
#'         icon = "sliders"
#'        )
#'       )
#'      )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#'
#' @export
bs4ValueBox <- function(value, subtitle, icon = NULL, elevation = NULL,
                        status = NULL, width = 3, footer = NULL, href = NULL) {
  
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
  if (!is.null(status)) valueBoxCl <- paste0(valueBoxCl, " bg-", status)
  if (!is.null(elevation)) valueBoxCl <- paste0(valueBoxCl, " elevation-", elevation)
  
  innerTag <- shiny::tags$div(
    class = "inner",
    value,
    shiny::tags$p(class = "small-box-subtitle", subtitle)
  )
  
  iconTag <- if(!is.null(icon)) {
    shiny::tags$div(
      class = "icon",
      shiny::icon(icon)
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
      NULL
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
#' @param ... Any extra UI element.
#' @param tabName Optional: \link{bs4InfoBox} may be used to navigate between tabs.
#' @param title Info box title.
#' @param value The value to display in the box. Usually a number or short text.
#' @param icon An icon tag, created by \code{\link[shiny]{icon}}.
#' @param iconStatus Icon color. Only if status is NULL.
#' @param iconElevation Icon elevation compared to the main content (relief). 3 by default.
#' @param status A color for the box. "primary", "info", "success", "warning", "danger" or NULL.
#' @param gradientColor If NULL (the default), the background of the box will be
#'   white. Otherwise, a color string. "primary", "success", "warning" or "danger".
#' @param width The width of the box, using the Bootstrap grid system. This is
#'   used for row-based layouts. The overall width of a region is 12, so the
#'   default width of 4 occupies 1/3 of that width. For column-based
#'   layouts, use \code{NULL} for the width; the width is set by the column that
#'   contains the box.
#' @param elevation Infobox elevation.
#'   
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @family cards
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'      navbar = bs4DashNavbar(),
#'      sidebar = bs4DashSidebar(),
#'      controlbar = bs4DashControlbar(),
#'      footer = bs4DashFooter(),
#'      title = "test",
#'      body = bs4DashBody(
#'       fluidRow(
#'        bs4InfoBox(
#'         title = "Messages",
#'         value = 1410,
#'         icon = "envelope"
#'        ),
#'        bs4InfoBox(
#'         title = "Bookmarks",
#'         status = "info",
#'         value = 240,
#'         icon = "bookmark"
#'        ),
#'        bs4InfoBox(
#'         title = "Comments",
#'         gradientColor = "danger",
#'         value = 41410,
#'         icon = "comments"
#'        )
#'       )
#'      )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#'
#' @export
bs4InfoBox <- function(..., tabName = NULL, title, value, icon = NULL,
                       iconStatus = NULL, iconElevation = NULL, status = NULL, 
                       gradientColor = NULL, width = 4,
                       elevation = NULL) {
  
  # checks
  if (!is.null(gradientColor) & !is.null(status)) {
    stop(
      "gradientColor is not compatible with status. Please choose only one property."
    )
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
  
  if (!is.null(iconElevation)) {
    stopifnot(is.numeric(iconElevation))
    stopifnot(iconElevation < 6)
    stopifnot(iconElevation >= 0)
  }
  
  if (is.null(icon) & (!is.null(iconStatus) | !is.null(iconElevation) | !is.null(tabName))) {
    stop("iconStatus/iconElevation/tabName must be set only if icon is not NULL.")
  }
  
  infoBoxCl <- if (!is.null(gradientColor)) {
    paste0("info-box bg-gradient-", gradientColor)
  } else {
    if (is.null(status)) {
      "info-box"
    } else {
      paste0("info-box bg-", status)
    }
  }
  
  if (!is.null(elevation)) infoBoxCl <- paste0(infoBoxCl, " elevation-", elevation)
  
  # Only do if icon is specified
  if(!is.null(icon)) {
    infoBoxIconCl <- "info-box-icon"
    if (!is.null(iconStatus)) infoBoxIconCl <- paste0(infoBoxIconCl, " bg-", iconStatus)
    if (!is.null(iconElevation)) infoBoxIconCl <- paste0(infoBoxIconCl, " elevation-", iconElevation)
    
    iconTag <- shiny::tags$span(
      class = infoBoxIconCl,
      id = if(!is.null(tabName)) paste0("icon-", tabName),
      # icon
      shiny::icon(icon)
    )
  } else {
    iconTag <- NULL
  }
  
  
  contentTag <- shiny::tags$div(
    class = "info-box-content",
    shiny::tags$span(
      class = "info-box-text",
      title
    ),
    shiny::tags$span(
      class = "info-box-number",
      value
    ),
    ...
  )
  
  
  infoBoxTag <- shiny::tags$div(class = infoBoxCl)
  infoBoxTag <- shiny::tagAppendChildren(infoBoxTag, iconTag, contentTag)
  
  # handle icon color (white or black depending on the box background)
  infoBoxTag <- shiny::tagList(
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
    ),
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
#' @param ... Contents of the box: should be \link{bs4TabPanel}.
#' @param id Unique \link{bs4TabSetPanel} id.
#' @param title TabCard title.
#' @param width The width of the box, using the Bootstrap grid system. This is
#'   used for row-based layouts. The overall width of a region is 12, so the
#'   default width of 4 occupies 1/3 of that width. For column-based
#'   layouts, use \code{NULL} for the width; the width is set by the column that
#'   contains the box.
#' @param height The height of a box, in pixels or other CSS unit. By default
#'   the height scales automatically with the content.
#' @param elevation tabCard elevation. 
#' @param side Side of the box the tabs should be on (\code{"left"} or
#'   \code{"right"}).
#' @param type TabPanel type: "tabs" or "pills". "pills" is the default if type is NULL.
#'   
#' @inheritParams bs4Card
#' @inheritParams bs4TabSetPanel
#' 
#' @family cards
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
#'      bs4TabCard(
#'       id = "tabcard",
#'       title = "A card with tabs",
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
#'      )
#'     )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4TabCard <- function(..., id, title = NULL, status = NULL, elevation = NULL, 
                       solidHeader = FALSE, headerBorder = TRUE, gradientColor = NULL,
                       tabStatus = NULL, width = 6, height = NULL,  
                       collapsible = TRUE, collapsed = FALSE, closable = TRUE,
                       maximizable = FALSE, overflow = FALSE, side = c("left", "right"),
                       type = NULL) {
  
  found_active <- FALSE
  side <- match.arg(side)
  if (is.null(type)) type <- "pills"
  
  tabCardCl <- "card"
  tabCardCl <- if (!is.null(gradientColor)) {
    paste0(tabCardCl, " bg-gradient-", gradientColor)
  } else {
    if (is.null(status)) {
      paste0(tabCardCl, " card-default card-tabs")
    } else {
      if (isTRUE(solidHeader)) {
        paste0(tabCardCl, " card-", status, " card-tabs")
      } else {
        paste0(tabCardCl, " card-outline card-", status, " card-outline-tabs")
      }
    }
  }
  if (isTRUE(collapsible) & isTRUE(collapsed)) tabCardCl <- paste0(tabCardCl, " collapsed-card")
  if (!is.null(elevation)) tabCardCl <- paste0(tabCardCl, " elevation-", elevation)
  
  # tools collapse/closable
  if (isTRUE(closable) | isTRUE(collapsible) | isTRUE(maximizable)) {
    cardToolTag <- shiny::tags$div(
      class = "tools pt-3 pb-3 pr-2 mr-2",
      
      # collapse
      if (isTRUE(collapsible)) {
        collapseIcon <- if (collapsed) 
          "plus"
        else "minus"
        shiny::tags$button(
          type = "button",
          class = "btn btn-tool pb-0 pt-0",
          `data-card-widget` = "collapse",
          shiny::icon(collapseIcon)
        )
      },
      
      # close
      if (isTRUE(closable)) {
        shiny::tags$button(
          type = "button",
          class = "btn btn-tool pb-0 pt-0",
          `data-card-widget` = "remove",
          shiny::tags$i(class = "fa fa-times")
        )
      },
      
      # maximize
      if (maximizable) {
        shiny::tags$button(
          type = "button",
          class = "btn btn-tool",
          `data-card-widget` = "maximize",
          shiny::tags$i(class = "fa fa-expand")
        )
      }
    )
  } else {
    cardToolTag <- shiny::tags$div()
  }
  
  # header
  tabMenu <- bs4TabSetPanel(..., id = id, side = side, tabStatus = tabStatus, type = type)[[1]]
  if (is.null(title) & (isTRUE(maximizable) | isTRUE(closable) | isTRUE(collapsible))) title <- "\u200C"
  
  headerTag <- shiny::tags$div(
    class = if (isTRUE(headerBorder)) "card-header d-flex p-0" else "card-header d-flex p-0 no-border",
    if (side == "right") {
      shiny::tagList(
        if (!is.null(title)) shiny::tags$h3(class = "card-title p-3", title) else NULL,
        # tab menu
        tabMenu
      )
    } else {
      shiny::tagList(
        # tab menu
        tabMenu,
        if (!is.null(title)) shiny::tags$h3(class = "card-title p-3 ml-auto", title) else NULL
      )
    }
  )
  headerTag <- if (!is.null(title)) shiny::tagAppendChild(headerTag, cardToolTag)
  
  
  # body
  panelContent <- bs4TabSetPanel(..., id = id, side = side, tabStatus = tabStatus)[[2]]
  bodyTag <- shiny::tags$div(
    class = "card-body",
    style = if (overflow) "overflow-y: auto; max-height: 500px;" else NULL,
    panelContent
  )
  
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", shiny::validateCssUnit(height))
  }
  
  tabCardTag <- shiny::tags$div(
    class = tabCardCl,
    style = if (!is.null(style)) style
  )
  
  tabCardTag <- shiny::tagAppendChildren(tabCardTag, headerTag, bodyTag)
  
  shiny::tags$div(
    class = paste0("col-sm-", width),
    tabCardTag
  )
}




#' @title AdminLTE3 widget user card
#'
#' @description Create widget user card
#'
#' @param ... Footer content.
#' @param type User card type. Either NULL or 2.
#' @param status User card color. "primary", "warning", "danger", "info" or "success".
#' @param src User image url or path.
#' @param elevation User card elevation (numeric). NULL by default.
#' @param imageElevation User card image elevation (numeric). NULL by default.
#' @param title User card title.
#' @param subtitle User card subtitle.
#' @param width The width of the card, using the Bootstrap grid system.
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'      navbar = bs4DashNavbar(),
#'      sidebar = bs4DashSidebar(),
#'      controlbar = bs4DashControlbar(),
#'      footer = bs4DashFooter(),
#'      title = "test",
#'      body = bs4DashBody(
#'       fluidRow(
#'       bs4UserCard(
#'         src = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
#'         status = "info",
#'         title = "User card type 1",
#'         subtitle = "a subtitle here",
#'         elevation = 4,
#'         "Any content here"
#'        ),
#'        bs4UserCard(
#'         type = 2,
#'         src = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",
#'         status = "success",
#'         imageElevation = 4,
#'         title = "User card type 2",
#'         subtitle = "a subtitle here",
#'         bs4ProgressBar(
#'          value = 5,
#'          striped = FALSE,
#'          status = "info"
#'          ),
#'         bs4ProgressBar(
#'           value = 5,
#'           striped = TRUE,
#'           status = "warning",
#'           width = "20%"
#'         )
#'        )
#'       )
#'      )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#'
#' @export
bs4UserCard <- function(..., type = NULL, src = NULL, elevation = NULL, imageElevation = NULL,
                        status = c("primary", "warning", "danger", "info", "success"),
                        title = NULL, subtitle = NULL, width = 6) {
  
  status <- match.arg(status)
  
  userCardCl <- "card card-widget"
  if (!is.null(type)) {
    userCardCl <- paste0(userCardCl, " widget-user-", type)
  } else {
    userCardCl <- paste0(userCardCl, " widget-user") 
  } 
  
  if (!is.null(elevation)) userCardCl <- paste0(userCardCl, " elevation-", elevation)
  
  
  headerCl <- "widget-user-header"
  if (!is.null(status)) headerCl <- paste0(headerCl, " bg-", status)
  
  
  headerImageTag <- shiny::tags$div(
    class = "widget-user-image",
    shiny::tags$img(
      class = if (!is.null(imageElevation)) {
        paste0("img-circle elevation-", imageElevation)
      } else {
        "img-circle"
      },
      src = src
    )
  )
  
  headerTag <- if (is.null(type)) {
    shiny::tagList(
      shiny::tags$div(
        class = headerCl,
        # title and subtitle
        shiny::tags$h3(class = "widget-user-username", title),
        shiny::tags$h5(class = "widget-user-desc", subtitle)
      ),
      headerImageTag
    )
  } else {
    shiny::tags$div(
      class = headerCl,
      headerImageTag,
      # title and subtitle
      shiny::tags$h3(class = "widget-user-username", title),
      shiny::tags$h5(class = "widget-user-desc", subtitle)
    )
  }
  
  
  footerTag <- shiny::tags$div(
    class = "card-footer",
    style = "overflow-y: auto; max-height: 500px;",
    ...
  )
  
  userCardTag <- shiny::tags$div(class = userCardCl)
  userCardTag <- shiny::tagAppendChildren(userCardTag, headerTag, footerTag)
  
  shiny::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    userCardTag
  )
}





#' @title AdminLTE3 simple box
#'
#' @description Create nice and epurated box
#'
#' @param ... Body content.
#' @param title Box title.
#' @param width The width of the box, using the Bootstrap grid system.
#' @param height The height of a box, in pixels or other CSS unit. By default
#'   the height scales automatically with the content.
#' 
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'      navbar = bs4DashNavbar(),
#'      sidebar = bs4DashSidebar(
#'       bs4SidebarMenu(
#'        bs4SidebarHeader("Main content"),
#'        bs4SidebarMenuItem(
#'          "Basic boxes",
#'          tabName = "boxes",
#'          icon = "desktop"
#'        )
#'       )
#'      ),
#'      controlbar = bs4DashControlbar(),
#'      footer = bs4DashFooter(),
#'      title = "test",
#'      body = bs4DashBody(
#'       bs4TabItems(
#'        bs4TabItem(
#'          tabName = "boxes",
#'          fluidRow(
#'           bs4Box(
#'            height = "600px",
#'            title = "Box 1",
#'            plotOutput("plot"),
#'            column(
#'             width = 12,
#'             align = "center",
#'             sliderInput(
#'               "obs",
#'               "Number of observations:",
#'               min = 0, max = 1000,
#'               value = 500
#'             )
#'            )
#'           ),
#'           bs4Box(
#'            height = "600px",
#'            title = "Box 2",
#'            plotOutput("distPlot"),
#'            column(
#'             width = 12,
#'             align = "center",
#'             radioButtons(
#'               "dist", 
#'               inline = TRUE,
#'               "Distribution type:",
#'               c("Norm" = "norm",
#'                 "Unif" = "unif",
#'                 "LogNorm" = "lnorm",
#'                 "Exp" = "exp")
#'             )
#'            )
#'           )
#'          )
#'        )
#'       )
#'      )
#'    ),
#'    server = function(input, output) {
#'     output$plot <- renderPlot({
#'      hist(rnorm(input$obs))
#'      })
#'      
#'      output$distPlot <- renderPlot({
#'        dist <- switch(
#'        input$dist,
#'        norm = rnorm,
#'        unif = runif,
#'        lnorm = rlnorm,
#'        exp = rexp,
#'        rnorm
#'      )
#'        
#'        hist(dist(500))
#'      })
#'    }
#'  )
#' }
#'
#' @export
bs4Box <- function(..., title = NULL, width = 6, height = NULL) {
  
  
  boxHeader <- shiny::tags$div(
    class = "card-header no-border",
    shiny::tags$div(
      class = "d-flex justify-content-between",
      shiny::tags$h3(class = "card-title", title)
    )
  )
  
  boxBody <- shiny::tags$div(
    class = "card-body",
    style = "overflow-y: auto; max-height: 800px;",
    ...
  )
  
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", shiny::validateCssUnit(height))
  }
  
  boxTag <- shiny::tags$div(class = "card card-box", style = if (!is.null(style)) style)
  boxTag <- shiny::tagAppendChildren(boxTag, boxHeader, boxBody)
  
  boxWrapper <- shiny::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    boxTag
  )
  
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            ".card-box {
              box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
              transition: 0.3s;
              border-radius: 5px; /* 5px rounded corners */
            }
            
            /* On mouse-over, add a deeper shadow */
            .card-box:hover {
             box-shadow: 0 16px 32px 0 rgba(0,0,0,0.2);
            }
            "
          )
        )
      )
    ),
    boxWrapper
  )
}



#' @title AdminLTE3 card profile
#'
#' @description Create card profile
#'
#' @param ... Any element such as \link{cardProfileItemList}.
#' @param src Profile image, if any.
#' @param title Title.
#' @param subtitle Subtitle.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'      navbar = bs4DashNavbar(),
#'      sidebar = bs4DashSidebar(
#'       bs4SidebarMenu(
#'        bs4SidebarHeader("Main content"),
#'        bs4SidebarMenuItem(
#'          "Profile Card",
#'          tabName = "profile_card",
#'          icon = "desktop"
#'        )
#'       )
#'      ),
#'      controlbar = bs4DashControlbar(),
#'      footer = bs4DashFooter(),
#'      title = "test",
#'      body = bs4DashBody(
#'       bs4TabItems(
#'        bs4TabItem(
#'          tabName = "profile_card",
#'          bs4Card(
#'           status = "primary",
#'           solidHeader = TRUE,
#'           cardProfile(
#'            src = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
#'            title = "Nina Mcintire",
#'            subtitle = "Software Engineer",
#'            cardProfileItemList(
#'             bordered = TRUE,
#'             cardProfileItem(
#'              title = "Followers",
#'              description = 1322
#'             ),
#'             cardProfileItem(
#'              title = "Following",
#'              description = 543
#'             ),
#'             cardProfileItem(
#'              title = "Friends",
#'              description = 13287
#'             )
#'            )
#'           )
#'         )
#'        )
#'       )
#'      )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#' 
#' @export
cardProfile <- function(..., src = NULL, title = NULL, subtitle = NULL) {
  
  shiny::tags$div(
    class = "card-body card-profile",
    shiny::tags$div(
      class = "text-center",
      shiny::img(class = "profile-user-img img-fluid img-circle", src = src)
    ),
    shiny::h3(class = "profile-username text-center", title),
    shiny::p(class = "text-muted text-center", subtitle),
    ...
  ) 
}

#' @title AdminLTE3 card profile item container
#'
#' @description Create card profile item list
#'
#' @param ... Slot for \link{cardProfileItem}.
#' @param bordered Whether the container should have a border or not. FALSE by default.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @export
cardProfileItemList <- function(..., bordered = FALSE) {
  
  cl <- if (isTRUE(bordered)) "list-group mb-3" else "list-group list-group-unbordered mb-3"
  
  shiny::tags$ul(
    class = cl,
    ...
  )
}

#' @title AdminLTE3 card profile item 
#'
#' @description Create card profile item 
#'
#' @param title Item title.
#' @param description Item info.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @export
cardProfileItem <- function(title = NULL, description = NULL) {
  shiny::tags$li(
    class = "list-group-item",
    shiny::strong(title),
    shiny::a(class = "float-right", description)
  )
}




#' @title AdminLTE3 social card
#'
#' @description Create social card
#'
#' @param ... Body content. May include \link{attachmentBlock} for instance.
#' @param src Header image, if any.
#' @param title Card title.
#' @param subtitle card subtitle.
#' @param width Card width (between 1 and 12). 
#' @param height Card height.
#' @param collapsible If TRUE, display a button in the upper right that allows the user to collapse the card. 
#' @param closable If TRUE, display a button in the upper right that allows the user to close the card.
#' @param comments Slot for \link{cardComment}.
#' @param footer Card footer, if any.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  shinyApp(
#'   ui = bs4DashPage(
#'     navbar = bs4DashNavbar(),
#'     sidebar = bs4DashSidebar(
#'      bs4SidebarMenu(
#'        bs4SidebarHeader("Main content"),
#'        bs4SidebarMenuItem(
#'          "Social Card",
#'          tabName = "social_card",
#'          icon = "desktop"
#'        )
#'       )
#'     ),
#'     footer = bs4DashFooter(),
#'     body = bs4DashBody(
#'      bs4TabItems(
#'        bs4TabItem(
#'          tabName = "profile_card",
#'          bs4SocialCard(
#'           title = "Social Card",
#'           subtitle = "example-01.05.2018",
#'           src = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
#'           "Some text here!",
#'           comments = tagList(
#'            lapply(X = 1:10, FUN = function(i) {
#'              cardComment(
#'               src = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
#'               title = paste("Comment", i),
#'               date = "01.05.2018",
#'               paste0("The ", i, "-th comment")
#'              )
#'             })
#'           ),
#'           footer = "The footer here!"
#'          )
#'        )
#'      )
#'    ),
#'    title = "socialCard"
#'   ),
#'   server = function(input, output) { }
#'  )
#' }
#'
#' @export
bs4SocialCard <- function(..., src = NULL, title = NULL, subtitle = NULL, 
                          width = 6, height = NULL, collapsible = TRUE,
                          closable = TRUE, comments = NULL, footer = NULL) {
  
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", shiny::validateCssUnit(height), ";")
  }
  
  shiny::column(
    width = width,
    shiny::tags$div(
      class = "card card-widget",
      style = paste0(style, " display: block;"),
      
      # header
      shiny::tags$div(
        class = "card-header",
        
        # userblock
        shiny::tags$div(
          class = "user-block",
          shiny::img(class = "img-circle", src = src),
          shiny::tags$span(
            class = "username",
            shiny::a(href = "javascript:void(0)", title)
          ),
          shiny::tags$span(class = "description", subtitle)
        ),
        
        # cardTool
        shiny::tags$div(
          class = "card-tools",
          if (collapsible) {
            shiny::tags$button(
              class = "btn btn-tool",
              `data-card-widget` = "collapse",
              type = "button",
              shiny::tags$i(class = "fa fa-minus")
            )
          },
          if (closable) {
            shiny::tags$button(
              class = "btn btn-tool",
              `data-card-widget` = "remove",
              type = "button",
              shiny::tags$i(class = "fa fa-times")
            )
          }
        )
      ),
      
      # card body
      shiny::tags$div(
        class = "card-body",
        ...
      ),
      
      # card comments
      if (!is.null(comments)) {
        shiny::tags$div(
          class = "card-footer card-comments",
          style = "overflow-y: auto; max-height: 150px; display: block;",
          comments
        ) 
      },
      
      # footer
      if (!is.null(footer)) {
        shiny::tags$div(
          class = "card-footer", 
          style = "display: block;",
          footer
        ) 
      }
    )
  )
}



#' @title BS4 card comment container
#'
#' @description Create a card comment to insert in \link{bs4SocialCard}
#'
#' @param ... Comment content.
#' @param src Author image, if any.
#' @param title Comment title.
#' @param date Date of publication.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @export
cardComment <- function(..., src = NULL, title = NULL, date = NULL) {
  shiny::tags$div(
    class = "card-comment",
    shiny::img(class = "img-circle img-sm", src = src),
    shiny::tags$div(
      class = "comment-text",
      shiny::tags$span(
        class = "username", 
        title,
        shiny::tags$span(class = "text-muted float-right", date)
      ),
      ...
    )
  )
}




#' Bootstrap 4 container for cards
#'
#' @param ... Slot for bs4Dash cards.
#' @param type Container type. See \url{https://getbootstrap.com/docs/4.0/components/card/#card-layout}
#' for more details.
#' @export
#' 
#' @note Cards must have width argument set to NULL.
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  # with group
#'  shiny::shinyApp(
#'    ui = dashboardPage(
#'      navbar = dashboardHeader(),
#'      sidebar = dashboardSidebar(),
#'      body = dashboardBody(
#'        bs4CardLayout(
#'          type = "group",
#'          lapply(1:4, function(i) {
#'            bs4Card(
#'              width = NULL,
#'              title = paste("Card", i), 
#'              closable = FALSE,
#'              collapsible = FALSE,
#'              "Lorem ipsum is so fun!"
#'            )
#'          })
#'        )
#'      ),
#'      controlbar = dashboardControlbar()
#'    ),
#'    server = function(input, output) {}
#'  )
#'  
#'  # with deck
#'  shiny::shinyApp(
#'    ui = dashboardPage(
#'      navbar = dashboardHeader(),
#'      sidebar = dashboardSidebar(),
#'      body = dashboardBody(
#'        bs4CardLayout(
#'          type = "deck",
#'          lapply(1:4, function(i) {
#'            bs4Card(
#'              width = NULL,
#'              title = paste("Card", i), 
#'              closable = FALSE,
#'              collapsible = FALSE,
#'              "Lorem ipsum is so fun!"
#'            )
#'          })
#'        )
#'      ),
#'      controlbar = dashboardControlbar()
#'    ),
#'    server = function(input, output) {}
#'  )
#'  
#'  # with columns
#'  shiny::shinyApp(
#'    ui = dashboardPage(
#'      navbar = dashboardHeader(),
#'      sidebar = dashboardSidebar(),
#'      body = dashboardBody(
#'        bs4CardLayout(
#'          type = "columns",
#'          lapply(1:12, function(i) {
#'            bs4Card(
#'              width = NULL,
#'              title = paste("Card", i), 
#'              closable = FALSE,
#'              collapsible = FALSE,
#'              height = if (i %% 2 == 1) "200px",
#'              status = if (i %% 2 == 0) "primary",
#'              if (i %% 2 == 0) "Lorem ipsum is so fun!",
#'              if (i == 1 | i == 7 | i == 12) img(src = "https://via.placeholder.com/290x160")
#'            )
#'          })
#'        )
#'      ),
#'      controlbar = dashboardControlbar()
#'    ),
#'    server = function(input, output) {}
#'  )
#'  
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