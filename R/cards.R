#' Create a Boostrap 4 card
#'
#' Build an adminLTE3 card
#'
#' @param ... Contents of the box.
#' @param title Optional title.
#' @param footer Optional footer text.
#' @param status The status of the item This determines the item's background
#'   color. "primary", "success", "warning", "danger". NULL by default.
#' @param elevation Card elevation. 
#' @param solidHeader Should the header be shown with a solid color background?
#' @param headerBorder Whether to display a border between the header and body.
#' TRUE by default
#' @param gradientColor If NULL (the default), the background of the box will be
#'   white. Otherwise, a color string. "primary", "success", "warning" or "danger".
#' @param width The width of the box, using the Bootstrap grid system. This is
#'   used for row-based layouts. The overall width of a region is 12, so the
#'   default valueBox width of 4 occupies 1/3 of that width. For column-based
#'   layouts, use \code{NULL} for the width; the width is set by the column that
#'   contains the box.
#' @param height The height of a box, in pixels or other CSS unit. By default
#'   the height scales automatically with the content.
#' @param collapsible If TRUE, display a button in the upper right that allows
#'   the user to collapse the box.
#' @param collapsed If TRUE, start collapsed. This must be used with
#'   \code{collapsible=TRUE}.
#' @param closable If TRUE, display a button in the upper right that allows the user to close the box.
#' @param labelStatus status of the box label: "danger", "success", "primary", "warning".
#' @param labelText Label text.
#' @param labelTooltip Label tooltip displayed on hover.
#' @param dropdownMenu List of items in the the boxtool dropdown menu. Use dropdownItemList().
#' @param dropdownIcon Dropdown icon. "wrench" by default.
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
#'        labelText = 1,
#'        labelStatus = "danger",
#'        labelTooltip = "Hi Bro!",
#'        dropdownIcon = "wrench",
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
#'        status = "warning", 
#'        solidHeader = FALSE, 
#'        gradientColor = "success",
#'        collapsible = TRUE,
#'        p("Box Content")
#'       )
#'      )
#'      ),
#'      bs4Card(
#'        title = "Closable Box with solidHeader", 
#'        closable = TRUE, 
#'        width = 6,
#'        solidHeader = TRUE, 
#'        status = "primary",
#'        collapsible = TRUE,
#'        p("Box Content")
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
bs4Card <- function(..., title = NULL, footer = NULL, status = NULL, elevation = NULL,
                    solidHeader = FALSE, headerBorder = TRUE, gradientColor = NULL, 
                    width = 6, height = NULL, collapsible = TRUE, collapsed = FALSE, 
                    closable = TRUE, labelStatus = NULL, labelText = NULL, 
                    labelTooltip = NULL, dropdownMenu = NULL, dropdownIcon = "wrench") {
  
  cardCl <- if (!is.null(gradientColor)) {
    paste0("card bg-", gradientColor, "-gradient")
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
    
  if (isTRUE(collapsed)) cardCl <- paste0(cardCl, " collapsed-card")
  if (!is.null(elevation)) cardCl <- paste0(cardCl, " elevation-", elevation)
  
  cardToolTag <- shiny::tags$div(
    class = "card-tools",
    
    # labels
    if (!is.null(labelText) || !is.null(labelStatus) || !is.null(labelTooltip)) {
      shiny::tags$span(
        class = paste0("badge bg-", labelStatus),
        title = if (!is.null(labelTooltip)) labelTooltip,
        `data-toggle` = "tooltip",
        labelText
      )
    },
    
    if (!is.null(dropdownMenu)) {
      shiny::tags$div(
        class = "btn-group",
        shiny::tags$button(
          type = "button",
          class = "btn btn-tool dropdown-toggle",
          `data-toggle` = "dropdown",
          shiny::icon(dropdownIcon)
        ),
        dropdownMenu
      )
    },
    
    # collapse
    if (isTRUE(collapsible)) {
      collapseIcon <- if (collapsed) 
        "plus"
      else "minus"
      shiny::tags$button(
        type = "button",
        class = "btn btn-tool",
        `data-widget` = "collapse",
        shiny::icon(collapseIcon)
      )
    },
    
    # close
    if (isTRUE(closable)) {
      shiny::tags$button(
        type = "button",
        class = "btn btn-tool",
        `data-widget` = "remove",
        shiny::tags$i(class = "fa fa-times")
      )
    }
  )
  
  # header
  headerTag <- shiny::tags$div(
    class = if (isTRUE(headerBorder)) "card-header" else "card-header no-border",
    shiny::tags$h3(class = "card-title", title)
  )
  headerTag <- shiny::tagAppendChild(headerTag, cardToolTag)
  
  # body
  bodyTag <- shiny::tags$div(
    class = "card-body",
    style = "overflow-y: auto; max-height: 500px;",
    ...
  )
  
  footerTag <- shiny::tags$div(
    class = "card-footer",
    style = "overflow-y: auto; max-height: 500px;",
    footer
  )
  
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", shiny::validateCssUnit(height))
  }
  
  cardTag <- shiny::tags$div(
    class = cardCl,
    style = if (!is.null(style)) style
  )
  cardTag <- shiny::tagAppendChildren(cardTag, headerTag, bodyTag, footerTag)
  
  shiny::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    cardTag
  )
    
}




#' Create a box dropdown item list
#'
#' Can be used to add dropdown items to a cardtool.
#'
#' @param ... Slot for dropdownItem.
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
dropdownItemList <- function(...) {
  shiny::tags$div(
    class = "dropdown-menu dropdown-menu-right",
    role = "menu",
    ...
  )
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
#'   default valueBox width of 4 occupies 1/3 of that width. For column-based
#'   layouts, use \code{NULL} for the width; the width is set by the column that
#'   contains the box.
#' @param href An optional URL to link to. 
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
#'         icon = "cogs"
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
                        status = NULL, width = 3, href = "#") {
 
  valueBoxCl <- "small-box"
  if (!is.null(status)) valueBoxCl <- paste0(valueBoxCl, " bg-", status)
  if (!is.null(elevation)) valueBoxCl <- paste0(valueBoxCl, " elevation-", elevation)
  
  innerTag <- shiny::tags$div(
    class = "inner",
    value,
    shiny::tags$p(subtitle)
  )
  
  iconTag <- shiny::tags$div(
    class = "icon",
    shiny::icon(icon)
  )
    
  footerTag <- shiny::tags$a(
      href = href,
      class = "small-box-footer",
      "More info",
      shiny::icon("arrow-circle-right")
    )
    
  valueBoxTag <- shiny::tags$div(
    class = valueBoxCl
  )
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
#' @param title Info box title.
#' @param value The value to display in the box. Usually a number or short text.
#' @param icon An icon tag, created by \code{\link[shiny]{icon}}.
#' @param iconElevation Icon elevation compared to the main content (relief). 3 by default.
#' @param status A color for the box. "primary", "info", "success", "warning", "danger" or NULL.
#' @param gradientColor If NULL (the default), the background of the box will be
#'   white. Otherwise, a color string. "primary", "success", "warning" or "danger".
#' @param width The width of the box, using the Bootstrap grid system. This is
#'   used for row-based layouts. The overall width of a region is 12, so the
#'   default valueBox width of 4 occupies 1/3 of that width. For column-based
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
bs4InfoBox <- function(..., title, value = NULL,
                       icon = NULL, 
                       iconElevation = 3, status = NULL, 
                       gradientColor = NULL, width = 4,
                       elevation = NULL) {
  

  infoBoxCl <- if (!is.null(gradientColor)) {
    paste0("info-box bg-", gradientColor, "-gradient")
  } else {
    if (is.null(status)) {
      "info-box"
    } else {
      paste0("info-box bg-", status)
    }
  }
  
  if (!is.null(elevation)) infoBoxCl <- paste0(infoBoxCl, " elevation-", elevation)
  
  iconTag <- shiny::tags$span(
    class = "info-box-icon",
    class = if (!is.null(iconElevation)) paste0("elevation-", iconElevation),
    # icon
    shiny::icon(icon)
  )
  
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
        shiny::tags$style(
          shiny::HTML(
              if (is.null(status)) {
                if (is.null(gradientColor)) {
                  paste0(
                    ".fa-", icon, "{
                      color: #000;
                     }
                    "
                  )
                } else {
                  paste0(
                    ".fa-", icon, "{
                      color: #fff;
                     }
                    "
                  )
                }
              } else {
                if (status == "white") {
                  paste0(
                    ".fa-", icon, "{
                      color: #000;
                     }
                    "
                  )
                } else {
                  paste0(
                    ".fa-", icon, "{
                      color: #fff;
                     }
                    "
                  )
                }
              }
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
#' @param ... Contents of the box: should be bs4TabPanel.
#' @param title TabCard title.
#' @param width The width of the box, using the Bootstrap grid system. This is
#'   used for row-based layouts. The overall width of a region is 12, so the
#'   default valueBox width of 4 occupies 1/3 of that width. For column-based
#'   layouts, use \code{NULL} for the width; the width is set by the column that
#'   contains the box.
#' @param height The height of a box, in pixels or other CSS unit. By default
#'   the height scales automatically with the content.
#' @param elevation tabCard elevation. 
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
#'       title = "A card with tabs",
#'       bs4TabPanel(
#'        tabName = "Tab1", 
#'        active = FALSE,
#'        "Content 1"
#'       ),
#'       bs4TabPanel(
#'        tabName = "Tab2", 
#'        active = TRUE,
#'        "Content 2"
#'       ),
#'       bs4TabPanel(
#'        tabName = "Tab3", 
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
bs4TabCard <- function(..., title = NULL, width = 6, 
                       height = NULL, elevation = NULL) {
  
  tabCardCl <- "card"
  if (!is.null(elevation)) tabCardCl <- paste0(tabCardCl, " elevation-", elevation)
  
  # header
  headerTag <- shiny::tags$div(
    class = "card-header d-flex p-0",
    shiny::tags$h3(class = "card-title p-3", title),
    
    # tab menu
    bs4TabSetPanel(...)
  )
  
  # body
  panels <- list(...)
  bodyTag <- shiny::tags$div(
    class = "card-body",
    style = "overflow-y: auto; max-height: 500px;",
    shiny::tags$div(
      class = "tab-content",
      lapply(1:length(panels), FUN = function(i) {
        panels[[i]][[2]]
      })
    )
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
#' Imported by bs4TabCard. Do not use outside!
#'
#' @param ... Slot for bs4TabPanel.
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4TabSetPanel <- function(...) {
  
  tabs <- list(...)
  
  # handle tabs
  tabSetPanelItem <- lapply(1:length(tabs), FUN = function(i) {
    
    tabName <- tabs[[i]][[1]]
    tabsTag <- tabs[[i]][[2]]
    
    id <- tabsTag$attribs$id
    active <- sum(grep(x = tabsTag$attribs$class, pattern = "active")) == 1
    
    shiny::tags$li(
      class = "nav-item",
      shiny::tags$a(
        class = if (active == 1) "nav-link active" else "nav-link",
        href = paste0("#", id),
        `data-toggle` = "tab",
        tabName
      )
    )
  })

  tabsetTag <- shiny::tags$ul(class = "nav nav-pills ml-auto p-2")
  tabsetTag <- shiny::tagAppendChildren(tabsetTag, tabSetPanelItem)
  tabsetTag
}



#' Create a tabPanel
#' 
#' To be included in a bs4TabCard
#'
#' @param ... Tab content
#' @param tabName Tab name: it will be also passed as the id argument. Should be unique.
#' @param active Whether the tab is active or not. FALSE bu default.
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4TabPanel <- function(..., tabName, active = FALSE) {
  
  tabPanelTag <- shiny::tags$div(
    class = if (isTRUE(active)) "tab-pane active" else "tab-pane",
    id = tabName,
    ...
  )
  return(list(tabName, tabPanelTag))
}



#' @title AdminLTE3 widget user card
#'
#' @description Create widget user card
#'
#' @param ... footer content.
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
    style = "overflow-y: auto; max-height: 600px;",
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