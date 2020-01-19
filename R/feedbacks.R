#' Create a Bootstrap 4 Tooltip from the UI side
#' 
#' This replaces the shinyBS tooltip feature that is not compatible
#' with Bootstrap 4
#'
#' @param tag Tooltip target.
#' @param title Tooltip title.
#' @param placement Tooltipe placement: "top", "bottom", "left" or "right". 
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shiny::shinyApp(
#'   ui = bs4DashPage(
#'     enable_preloader = TRUE,
#'     navbar = bs4DashNavbar(),
#'     sidebar = bs4DashSidebar(),
#'     controlbar = bs4DashControlbar(),
#'     footer = bs4DashFooter(),
#'     title = "test",
#'     body = bs4DashBody(
#'      bs4TooltipUI(
#'       actionButton("goButton", "Hover to see the tooltip"),
#'       title = "My tooltip",
#'       placement = "top"
#'      )
#'     )
#'   ),
#'   server = function(input, output) {}
#'  )
#' }
bs4TooltipUI <- function(tag, title, placement = c("top", "bottom", "left", "right")) {
  
  placement <- match.arg(placement)
  
  tag <- shiny::tagAppendAttributes(
    tag, 
    `data-toggle` = "tooltip",
    `data-placement` = placement, 
    title = title
  )
  
  tagId <- tag$attribs$id
  
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$script(
          "$(function () {
           // enable all tooltips
           var target = '#' + ", tagId, ";
           $('target').tooltip();
          });
          "
        )
      )
    ),
    tag
  )
}



#' Create a Bootstrap 4 Tooltip from the server side
#' 
#' This replaces the shinyBS tooltip feature that is not compatible
#' with Bootstrap 4
#'
#' @param target Tooltip target.
#' @param title Tooltip title.
#' @param placement Tooltipe placement: "top", "bottom", "left" or "right". 
#' @param session Shiny session object.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shiny::shinyApp(
#'   ui = bs4DashPage(
#'     enable_preloader = TRUE,
#'     navbar = bs4DashNavbar(),
#'     sidebar = bs4DashSidebar(),
#'     controlbar = bs4DashControlbar(),
#'     footer = bs4DashFooter(),
#'     title = "test",
#'     body = bs4DashBody(
#'      actionButton("goButton", "Click on me to add tooltip"),
#'      actionButton("goButton2", "You can't see me first!")
#'     )
#'   ),
#'   server = function(input, output, session) {
#'    observeEvent(input$goButton, {
#'      bs4TooltipServer(
#'       session = session, 
#'       target = "goButton2", 
#'       title = "Server tooltip", 
#'       placement = "bottom"
#'      )
#'    })
#'   }
#'  )
#' }
bs4TooltipServer <- function(target, title, placement = c("top", "bottom", "left", "right"), 
                             session) {
  
  placement <- match.arg(placement)
  
  message <- dropNulls(
    list(
      target = target,
      title = title,
      placement = placement
    )
  )
  session$sendCustomMessage("tooltip", message)
}





#' Create a Bootstrap 4 Popover from the UI side
#' 
#' This replaces the shinyBS popover feature that is not compatible
#' with Bootstrap 4
#'
#' @param tag Popover target.
#' @param content Popover content.
#' @param title Popover title.
#' @param placement Popover placement: "top", "bottom", "left" or "right". 
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shiny::shinyApp(
#'   ui = bs4DashPage(
#'     enable_preloader = TRUE,
#'     navbar = bs4DashNavbar(),
#'     sidebar = bs4DashSidebar(),
#'     controlbar = bs4DashControlbar(),
#'     footer = bs4DashFooter(),
#'     title = "test",
#'     body = bs4DashBody(
#'      bs4PopoverUI(
#'       actionButton("goButton", "Click me to see the popover!"),
#'        title = "My popover",
#'        placement = "right",
#'        content = "Vivamus sagittis lacus vel augue laoreet rutrum faucibus."
#'      )
#'     )
#'   ),
#'   server = function(input, output) {}
#'  )
#' }
bs4PopoverUI <- function(tag, content, title, placement = c("top", "bottom", "left", "right")) {
  
  placement <- match.arg(placement)
  
  tag <- shiny::tagAppendAttributes(
    tag, 
    `data-container` = "body",
    `data-toggle` = "popover",
    `data-placement` = placement, 
    `data-content` = content,
    title = title
  )
  
  tagId <- tag$attribs$id
  
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$script(
          "$(function () {
           // enable all popovers
           $('[data-toggle=\"popover\"]').popover();
          });
          "
        )
      )
    ),
    tag
  )
}





#' Create a Bootstrap 4 Popover from the server side
#' 
#' This replaces the shinyBS popover feature that is not compatible
#' with Bootstrap 4
#'
#' @param target Popover target.
#' @param content Popover content.
#' @param title Popover title.
#' @param placement Popover placement: "top", "bottom", "left" or "right". 
#' @param session Shiny session object.
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shiny::shinyApp(
#'   ui = bs4DashPage(
#'     enable_preloader = TRUE,
#'     navbar = bs4DashNavbar(),
#'     sidebar = bs4DashSidebar(),
#'     controlbar = bs4DashControlbar(),
#'     footer = bs4DashFooter(),
#'     title = "test",
#'     body = bs4DashBody(
#'      actionButton("goButton", "Show popover!"),
#'      actionButton("goButton2", "You can't see me first!")
#'     )
#'   ),
#'   server = function(input, output, session) {
#'    observeEvent(input$goButton, {
#'       bs4PopoverServer(
#'         session = session, 
#'         target = "goButton2", 
#'         content = "Vivamus sagittis lacus vel augue laoreet rutrum faucibus.",
#'         title = "Server popover", 
#'         placement = "bottom"
#'       )
#'     })
#'   }
#'  )
#' }
bs4PopoverServer <- function(target, content, title, placement = c("top", "bottom", "left", "right"), session) {
  
  placement <- match.arg(placement)
  
  message <- dropNulls(
    list(
      target = target,
      content = content,
      title = title,
      placement = placement
    )
  )
  session$sendCustomMessage("popover", message)
}