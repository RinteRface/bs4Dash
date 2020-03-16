#' Create a Boostrap 4 card
#'
#' Build an adminLTE3 card
#'
#' @param ... Contents of the box.
#' @param inputId Get the state of the card. Optional.
#' @param title Optional title.
#' @param footer Optional footer text.
#' @param status The status of the card header. "primary", "secondary", "success", "warning", "danger", "white", "light", "dark", "transparent". NULL by default.
#' @param elevation Card elevation. 
#' @param solidHeader Should the header be shown with a solid color background?
#' @param headerBorder Whether to display a border between the header and body.
#' TRUE by default
#' @param gradientColor If NULL (the default), the background of the box will be
#'   white. Otherwise, a color string. "primary", "success", "warning" or "danger".
#' @param width The width of the box, using the Bootstrap grid system. This is
#'   used for row-based layouts. The overall width of a region is 12, so the
#'   default width of 4 occupies 1/3 of that width. For column-based
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
#' @param label Slot for \link{bs4CardLabel}.
#' @param dropdownMenu List of items in the the boxtool dropdown menu. Use \link{dropdownItemList}.
#' @param overflow Whether to enable overflow in the card body and footer. FALSE by default.
#' @param sidebar Slot for \link{bs4CardSidebar}.
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
#'     fluidRow(
#'      column(
#'       width = 6,
#'       bs4Card(
#'        title = "Closable Box with dropdown", 
#'        closable = TRUE, 
#'        width = 12,
#'        status = "warning", 
#'        solidHeader = FALSE, 
#'        collapsible = TRUE,
#'        label = bs4CardLabel(
#'         text = 1,
#'         status = "danger",
#'         tooltip = "Hello!"
#'        ),
#'        dropdownMenu = dropdownItemList(
#'          dropdownItem(url = "http://www.google.com", name = "Link to google"),
#'          dropdownItem(url = "#", name = "item 2"),
#'          dropdownDivider(),
#'          dropdownItem(url = "#", name = "item 3")
#'        ),
#'        p("Box Content")
#'       )
#'      ),
#'      column(
#'       width = 6, 
#'       bs4Card(
#'        title = "Closable Box with gradient", 
#'        closable = TRUE, 
#'        width = 12,
#'        solidHeader = FALSE, 
#'        gradientColor = "success",
#'        collapsible = TRUE,
#'        p("Box Content")
#'       )
#'      )
#'      ),
#'      fluidRow(
#'       bs4Card(
#'        title = "Closable Box with solidHeader", 
#'        closable = TRUE, 
#'        width = 6,
#'        solidHeader = TRUE, 
#'        status = "primary",
#'        collapsible = TRUE,
#'        p("Box Content")
#'       ),
#'       bs4Card(
#'        inputId = "card4",
#'        title = "Maximizable Card", 
#'        width = 6,
#'        status = "warning", 
#'        closable = FALSE,
#'        maximizable = TRUE, 
#'        collapsible = TRUE,
#'        sliderInput("obs", "Number of observations:",
#'                    min = 0, max = 1000, value = 500
#'        ),
#'        plotOutput("distPlot")
#'       )
#'      )
#'     )
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
bs4Card <- function(..., inputId = NULL, title = NULL, footer = NULL, status = NULL, elevation = NULL,
                    solidHeader = FALSE, headerBorder = TRUE, gradientColor = NULL, 
                    width = 6, height = NULL, collapsible = TRUE, collapsed = FALSE, 
                    closable = FALSE, maximizable = FALSE, label = NULL, dropdownMenu = NULL, overflow = FALSE, sidebar = NULL) {
  
  if (!is.null(height) & overflow) {
    stop(
      "overlow and height are not compatible. Please choose only one property. 
      When overflow is TRUE, the maximum height is 500px"
    )
  }
  
  if (is.null(status) & solidHeader) stop("solidHeader cannot be used when status is NULL.")
  
  if (!is.null(gradientColor) & solidHeader) {
    stop(
      "gradientColor is not compatible with solideHeader. Please choose only one property."
    )
  }
  
  if (!is.null(gradientColor) & !is.null(status)) {
    stop(
      "gradientColor is not compatible with status. Please choose only one property."
    )
  }
  
  if (!collapsible & collapsed) {
    stop("Cannot collapse a card that is not collapsible.")
  }
  
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
  
  cardCl <- if (!is.null(gradientColor)) {
    paste0("card bg-gradient-", gradientColor)
  } else {
    if (is.null(status)) {
      "card card-default"
    } else {
      if (isTRUE(solidHeader)) {
        paste0("card card-outline card-", status)
      } else {
        paste0("card card-", status)
      }
    }
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
  
  if (isTRUE(collapsible) & isTRUE(collapsed)) cardCl <- paste0(cardCl, " collapsed-card")
  if (!is.null(elevation)) cardCl <- paste0(cardCl, " elevation-", elevation)
  
  cardToolTag <- shiny::tags$div(
    class = "card-tools",
    
    # labels
    if (!is.null(label)) label,
    # dropdown
    if (!is.null(dropdownMenu)) dropdownMenu,
    
    # collapse
    if (isTRUE(collapsible)) {
      collapseIcon <- if (collapsed) 
        "plus"
      else "minus"
      shiny::tags$button(
        type = "button",
        class = "btn btn-tool",
        `data-card-widget` = "collapse",
        shiny::icon(collapseIcon)
      )
    },
    
    # close
    if (isTRUE(closable)) {
      shiny::tags$button(
        type = "button",
        class = "btn btn-tool",
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
    },
    
    # sidebar
    if (!is.null(sidebar)) sidebar[[2]]
  )
  
  # header
  if (is.null(title) & (isTRUE(maximizable) | isTRUE(closable) | isTRUE(collapsible))) title <- "\u200C"
  
  headerTag <- shiny::tags$div(
    class = if (isTRUE(headerBorder)) "card-header" else "card-header no-border",
    if (!is.null(title)) shiny::tags$h3(class = "card-title", title) else NULL
  )
  headerTag <- if (!is.null(title)) shiny::tagAppendChild(headerTag, cardToolTag)
  

  # body
  bodyTag <- shiny::tags$div(
    class = "card-body",
    style = if (!is.null(height)) {
      paste0("height: ", shiny::validateCssUnit(height))
    } else {
      if (overflow) "overflow-y: auto; max-height: 500px;" else NULL
    },
    ...,
    if (!is.null(sidebar)) sidebar[c(1, 3)]
  )
  
  footerTag <- if (!is.null(footer)) {
    shiny::tags$div(
      class = "card-footer",
      style = if (overflow) "overflow-y: auto; max-height: 500px;" else NULL,
      footer
    ) 
  }
  
  cardTag <- shiny::tags$div(class = cardCl, id = inputId)
  cardTag <- shiny::tagAppendChildren(cardTag, headerTag, bodyTag, footerTag)
  
  cardWrapper <- shiny::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    cardTag
  )
  cardWrapper
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
#' @param inputId Unique sidebar id. Useful if you want to use \link{updatebs4CardSidebar}.
#' @param width Sidebar width in percentage. 25\% by default. A character value of any width CSS understands (e.g. "100px").
#' @param background Sidebar background color. Dark by default.
#' @param startOpen Whether the sidebar is open at start. FALSE by default.
#' @param icon Sidebar icon.
#' 
#' @export
bs4CardSidebar <- function(..., inputId = NULL, width = "25%", background = "#333a40", 
                           startOpen = FALSE, icon = "cogs") {
  
  # Toggle to insert in bs4Card
  toolbarTag <- shiny::tags$button(
    class = "btn btn-tool",
    id = inputId,
    `data-widget` = "chat-pane-toggle",
    `data-toggle` = "tooltip",
    `data-original-title` = "More",
    `data-start-open` = tolower(startOpen),
    type = "button",
    shiny::icon(icon)
  )
  
  # sidebar content
  contentTag <- shiny::tags$div(
    style = "z-index: 10000;",
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




#' update an AdminLTE3 card from the server side
#'
#' @param inputId Card inputId
#' @param session Shiny session
#' @param action Action to trigger: \code{c("remove", "toggle", "toggleMaximize", "restore")}.
#' 
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shiny::shinyApp(
#'    ui = dashboardPage(
#'      sidebar_collapsed = TRUE,
#'      navbar = dashboardHeader(),
#'      sidebar = dashboardSidebar(),
#'      body = dashboardBody(
#'        actionButton(inputId = "triggerCard", label = "Trigger Card Action"),
#'        selectInput(
#'          inputId = "cardAction", 
#'          label = "Card action", 
#'          choices = c(
#'            "remove",
#'            "toggle",
#'            "toggleMaximize",
#'            "restore"
#'          )
#'        ),
#'        
#'        bs4Card(
#'          inputId = "mycard",
#'          title = "The plot is visible when you maximize the card", 
#'          closable = TRUE, 
#'          maximizable = TRUE,
#'          width = 12,
#'          status = "warning", 
#'          solidHeader = FALSE, 
#'          collapsible = TRUE,
#'          sliderInput("obs", "Number of observations:",
#'                      min = 0, max = 1000, value = 500
#'          ),
#'          plotOutput("distPlot")
#'        )
#'      )
#'    ),
#'    server = function(input, output, session) {
#'      
#'      output$distPlot <- renderPlot({
#'        if (input$mycard$maximized) {
#'          hist(rnorm(input$obs)) 
#'        }
#'      })
#'      
#'      observeEvent(input$triggerCard, {
#'        updatebs4Card(inputId = "mycard", session = session, action = input$cardAction)
#'      })
#'      
#'      observe({
#'        print(
#'          list(
#'            collapsed = input$mycard$collapsed,
#'            maximized = input$mycard$maximized,
#'            visible = input$mycard$visible
#'          )
#'        )
#'      })
#'    }
#'  )
#' }
updatebs4Card <- function(inputId, session, action = c("remove", "toggle", "toggleMaximize", "restore")) {
  action <- match.arg(action)
  session$sendInputMessage(inputId, action)
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
dropdownItemList <- function(..., icon = "wrench") {
  
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
      shiny::icon(icon)
    ),
    contentTag
  )
  
  return(toolbarTag)
  
}




#' Create a box dropdown item 
#'
#' @param url Target url or page.
#' @param name Item name.
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
dropdownItem <- function(url = NULL, name = NULL) {
  shiny::tags$a(
    class = "dropdown-item",
    href = url,
    target = "_blank",
    name 
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
        paste0(tabCardCl, " card-outline card-", status, " card-outline-tabs")
      } else {
        paste0(tabCardCl, " card-", status, " card-tabs")
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