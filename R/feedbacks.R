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
#' @param id Tooltip target id.
#' @param selector jQuery selector. Allow more customization for the target (nested tags).
#' @param options List of options to pass to the tooltip. See \url{https://getbootstrap.com/docs/4.0/components/tooltips/}. 
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
#'       id = "goButton2", 
#'       options = list(
#'        title = "Server tooltip", 
#'        placement = "bottom"
#'       )
#'      )
#'    })
#'   }
#'  )
#' }
bs4TooltipServer <- function(id = NULL, selector = NULL, options, session = shiny::getDefaultReactiveDomain()) {
  
  if (!is.null(id) & !is.null(selector)) {
    stop("Please choose either target or selector!")
  }
  if (is.null(options$title)) stop("Please provide a tooltip title!")
  
  options <- jsonlite::toJSON(options, auto_unbox = TRUE, pretty = TRUE)
  
  message <- dropNulls(
    list(
      id = id,
      selector = selector,
      options = options
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
#' @param id Popover target id.
#' @param selector jQuery selector. Allow more customization for the target (nested tags).
#' @param options List of options to pass to the popover. See \url{https://getbootstrap.com/docs/4.0/components/popovers/}.
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
#'         id = "goButton2", 
#'         options = list(
#'          content = "Vivamus sagittis lacus vel augue laoreet rutrum faucibus.",
#'          title = "Server popover", 
#'          placement = "bottom",
#'          trigger = "hover"
#'         )
#'       )
#'     })
#'   }
#'  )
#' }
bs4PopoverServer <- function(id = NULL, selector = NULL, options, session = shiny::getDefaultReactiveDomain()) {
  
  if (!is.null(id) & !is.null(selector)) {
    stop("Please choose either target or selector!")
  }
  if (is.null(options$content)) stop("Please provide a popover content!")
  options <- jsonlite::toJSON(options, auto_unbox = TRUE, pretty = TRUE)
  
  message <- dropNulls(
    list(
      id = id,
      selector = selector,
      options = options
    )
  )
  session$sendCustomMessage("popover", message)
}





#' Create an adminLTE toast
#'
#' Builtin AdminLTE3 toasts
#'
#' @param title Toast title.
#' @param body Body content.
#' @param subtitle Toast subtitle.
#' @param options Toasts options: a list. See \url{https://adminlte.io/docs/3.0/javascript/toasts.html}.
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
#'   ui = dashboardPage(
#'     navbar = dashboardHeader(),
#'     sidebar = dashboardSidebar(),
#'     body = dashboardBody(
#'       actionButton("sendToast", "Send Toast")
#'     ),
#'     controlbar = dashboardControlbar()
#'   ),
#'   server = function(input, output) {
#'     observeEvent(input$sendToast, {
#'       bs4Toast(
#'         title = "My Toast", 
#'         body = h4("I am a toast!"),
#'         options = list(
#'           autohide = TRUE,
#'           icon = "fas fa-home",
#'           close = FALSE
#'         )
#'       )
#'     })
#'   }
#'  )
#'  
#' }
#' @importFrom jsonlite toJSON
bs4Toast <- function(title, body = NULL, subtitle = NULL, options = NULL, 
                     session = shiny::getDefaultReactiveDomain()) {
  
  props <- dropNulls(
    list(
      title = title,
      body = body,
      subtitle = subtitle
    )
  )
  
  message <- c(props, options)
  
  # make sure that shiny tags are evaluated and converted
  # to strings since the toast api only accept strings
  message2 <- lapply(seq_along(message), function(i) {
    if (inherits(message[[i]], "shiny.tag")) {
      as.character(force(message[[i]]))
    } else {
      message[[i]]
    }
  })
  names(message2) <- names(message)
  
  # convert to json
  message <- jsonlite::toJSON(
    message2,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  
  session$sendCustomMessage("toast", message2)
  
}




#' Create a Bootstrap 4 alert
#' 
#' AdminLTE3 alert
#'
#' @param ... Alert content.
#' @param id Alert id. Needed by \link{bs4CloseAlert}.
#' @param title Alert title.
#' @param closable Whether to allow the user to close the alert. FALSE by default.
#' @param width Alert width. Between 1 and 12.
#' @param elevation Alert elevation.
#' @param status Alert status. "primary", "success", "warning", "danger" or "info".
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
#'        title = "Alerts",
#'        bs4Alert(
#'         title = "Be Careful!",
#'         status = "danger",
#'         closable = FALSE,
#'         "Danger alert preview. This alert is not dismissable. 
#'         A wonderful serenity has taken possession of my entire soul, 
#'         like these sweet mornings of spring which 
#'         I enjoy with my whole heart."
#'        ),
#'        bs4Alert(
#'         title = "Congratulation!",
#'         status = "success",
#'         closable = TRUE,
#'         elevation = 4,
#'         "Warning alert preview. This alert is dismissable. 
#'         A wonderful serenity has taken possession of my entire soul, 
#'         like these sweet mornings of spring which 
#'         I enjoy with my whole heart."
#'        )
#'      )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }

#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4Alert <- function(..., id = NULL, title, closable = TRUE, width = 6, elevation = NULL,
                     status = c("primary", "warning", "danger", "info", "success")) {
  
  status <- match.arg(status)
  
  type <- switch(
    status,
    primary = "info",
    danger = "ban",
    info = "info",
    warning = "warning",
    success = "check"
  )
  
  alertCl <- "alert alert-dismissible"
  if (!is.null(status)) alertCl <- paste0(alertCl, " alert-", status)
  if (!is.null(elevation)) alertCl <- paste0(alertCl, " elevation-", elevation)
  
  alertTag <- shiny::tags$div(
    id = id, 
    class = alertCl,
    if (closable) shiny::tags$button(
      type = "button",
      class = "close",
      `data-dismiss` = "alert",
      `aria-hidden` = "true",
      "x"
    ),
    shiny::tags$h5(
      shiny::tags$i(class = paste0("icon fa fa-", type)),
      title
    ),
    ...
  )
  
  shiny::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    alertTag
  )
}




#' Close AdminLTE3 alert
#' 
#' Server side function
#'
#' @param id \link{bs4Alert} id.
#' @param session Shiny session object.
#' @export
#'
#' @note One may use input$<id>, where id is the alert unique id, to trigger
#' more actions on the server side after the alert closed.
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shiny::shinyApp(
#'   ui = dashboardPage(
#'     navbar = dashboardHeader(),
#'     sidebar = dashboardSidebar(),
#'     body = dashboardBody(
#'       actionButton("close", "Close Alert"),
#'       bs4Alert(id = "myalert", title = "Hello", status = "success")
#'     ),
#'     controlbar = dashboardControlbar()
#'   ),
#'   server = function(input, output, session) {
#'     observeEvent(input$close, {
#'       bs4CloseAlert(id = "myalert")
#'     })
#'     
#'     observe(print(input$myalert))
#'     
#'     observeEvent(input$myalert, {
#'       bs4Toast(title = "Alert succesfully closed!")
#'     })
#'   }
#'  )
#' }
bs4CloseAlert <- function(id, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage("alert", session$ns(id))
}