#' Create a Bootstrap 4 Tooltip from the UI side
#' 
#' This replaces the shinyBS tooltip feature that is not compatible
#' with Bootstrap 4
#'
#' @param tag Tooltip target.
#' @param title Tooltip title.
#' @param placement Tooltip placement: "top", "bottom", "left" or "right". 
#'
#' @export
#' @rdname bs4Tooltip
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     header = dashboardHeader(),
#'     sidebar = dashboardSidebar(),
#'     controlbar = dashboardControlbar(),
#'     footer = dashboardFooter(),
#'     title = "Tooltip UI",
#'     body = dashboardBody(
#'      tooltip(
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
          sprintf(
            "$(function () {
              // enable tooltip
              $('#%s').tooltip();
            });
            ",
            tagId
          )
        )
      )
    ),
    tag
  )
}



#' Create a Bootstrap 4 tooltip from the server side
#' 
#' \link{addTooltip} adds a tooltip to the given target.
#' 
#' @note This replaces the shinyBS tooltip feature that is not compatible
#' with Bootstrap 4
#'
#' @param id Tooltip target id.
#' @param selector jQuery selector. Allow more customization for the target (nested tags).
#' @param options List of options to pass to the tooltip. See \url{https://getbootstrap.com/docs/4.0/components/tooltips/}. 
#' @param session Shiny session object.
#'
#' @export
#' @rdname tooltip
#'
#' @examples
#' if (interactive()) {
#'  library(shiny) 
#'  library(bs4Dash) 
#'     
#'  shinyApp( 
#'    ui = dashboardPage( 
#'      header = dashboardHeader(), 
#'      sidebar = dashboardSidebar(), 
#'      controlbar = dashboardControlbar(), 
#'      footer = dashboardFooter(), 
#'      title = "Tooltip server", 
#'      body = dashboardBody( 
#'       sliderInput("obs", "Number of observations:", 
#'         min = 0, max = 1000, value = 500 
#'       ), 
#'       plotOutput("distPlot") 
#'      ) 
#'    ), 
#'    server = function(input, output, session) { 
#'      output$distPlot <- renderPlot({ 
#'        hist(rnorm(input$obs)) 
#'      }) 
#'          
#'      observeEvent(input$obs, { 
#'        if (input$obs > 500) { 
#'         addTooltip( 
#'           id = "distPlot",  
#'           options = list(
#'            title = "Server tooltip",  
#'            placement = "bottom"
#'           ) 
#'         ) 
#'        } else { 
#'          removeTooltip(id = "distPlot") 
#'        } 
#'      }) 
#'    } 
#'   )      
#' }
addTooltip <- function(id = NULL, selector = NULL, options, session = shiny::getDefaultReactiveDomain()) {
  
  if (!is.null(id) && !is.null(selector)) {
    stop("Please choose either target or selector!")
  }
  if (is.null(options$title)) stop("Please provide a tooltip title!")
  
  message <- dropNulls(
    list(
      id = session$ns(id),
      selector = selector,
      options = options
    )
  )
  session$sendCustomMessage("create-tooltip", message)
}




#' Remove a Bootstrap 4 tooltip from the server side
#' 
#' \link{removeTooltip} destroys the current targeted tooltip.
#'
#' @param id Tooltip target id.
#' @param session Shiny session object.
#' @export
#' @rdname tooltip
removeTooltip <- function(id, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage("remove-tooltip", message = session$ns(id))
}





#' Create a Bootstrap 4 popover from the UI side
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
#' @rdname bs4Popover
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     header = dashboardHeader(),
#'     sidebar = dashboardSidebar(),
#'     controlbar = dashboardControlbar(),
#'     footer = dashboardFooter(),
#'     title = "Popover UI",
#'     body = dashboardBody(
#'      popover(
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
          sprintf(
            "$(function () {
              // enable popover
              $('#%s').popover();
            });
            ",
            tagId
          )
        )
      )
    ),
    tag
  )
}





#' Create a Bootstrap 4 popover from the server side
#' 
#' \link{addPopover} adds a popover to the given target.
#' 
#' @note This replaces the shinyBS popover feature that is not compatible
#' with Bootstrap 4
#'
#' @param id Popover target id.
#' @param selector jQuery selector. Allow more customization for the target (nested tags).
#' @param options List of options to pass to the popover. See \url{https://getbootstrap.com/docs/4.0/components/popovers/}.
#' @param session Shiny session object.
#' @export
#' @rdname popover
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     header = dashboardHeader(),
#'     sidebar = dashboardSidebar(),
#'     controlbar = dashboardControlbar(),
#'     footer = dashboardFooter(),
#'     title = "Popover server",
#'     body = dashboardBody(
#'      sliderInput("obs", "Number of observations:",
#'        min = 0, max = 1000, value = 500
#'      ),
#'      plotOutput("distPlot")
#'     )
#'   ),
#'   server = function(input, output, session) {
#'    output$distPlot <- renderPlot({
#'      hist(rnorm(input$obs))
#'    })
#' 
#'    observeEvent(input$obs, {
#'      if (input$obs > 500) {
#'       addPopover(
#'         id = "distPlot", 
#'         options = list(
#'          content = "Vivamus sagittis lacus vel augue laoreet rutrum faucibus.",
#'          title = "Server popover", 
#'          placement = "bottom",
#'          trigger = "hover"
#'         )
#'       )
#'      } else {
#'        removePopover(id = "distPlot")
#'      }
#'    })
#'   }
#'  )
#' }
addPopover <- function(id = NULL, selector = NULL, options, session = shiny::getDefaultReactiveDomain()) {
  
  if (!is.null(id) && !is.null(selector)) {
    stop("Please choose either target or selector!")
  }
  if (is.null(options$content)) stop("Please provide a popover content!")
  
  message <- dropNulls(
    list(
      id = session$ns(id),
      selector = selector,
      options = options
    )
  )
  session$sendCustomMessage("create-popover", message)
}



#' Remove a Bootstrap 4 popover from the server side
#' 
#' \link{removePopover} destroys the current targeted popover.
#'
#' @param id Popover target id.
#' @param session Shiny session object.
#' @export
#' @rdname popover
removePopover <- function(id, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage("remove-popover", message = session$ns(id))
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
#' @rdname toast
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     header = dashboardHeader(),
#'     sidebar = dashboardSidebar(),
#'     body = dashboardBody(
#'       actionButton("sendToast", "Send Toast")
#'     ),
#'     controlbar = dashboardControlbar(),
#'     title = "Toasts"
#'   ),
#'   server = function(input, output) {
#'     observeEvent(input$sendToast, {
#'       toast(
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
  
  session$sendCustomMessage("toast", message2)
  
}





#' Create a Bootstrap 4 alert on the server side
#' 
#' \link{createAlert} creates an alert and inserts it in the DOM.
#'
#' @param id Anchor id. Where to insert the alert. See example. 
#' @param selector jQuery selector. Allow more customization for the anchor (nested tags).
#' @param options List of options to pass to the alert. See below:
#' \itemize{
#'  \item content: Alert content.
#'  \item title: Alert title.
#'  \item closable: Whether to allow the user to close the alert. FALSE by default.
#'  \item width: Alert width. Between 1 and 12.
#'  \item elevation: Alert elevation.
#'  \item status: Alert status. "primary", "success", "warning", "danger" or "info".
#' }
#' @param session Shiny session object.
#' @export
#' 
#' @note Unlike shinyBS, there is no need to specify an anchorId and an alertId. id refers to the anchorId,
#' and the alertId is simply "anchorId-alert". On the server side, one can access the alert status by
#' input$<id>. If TRUE, the alert has been created and is visible, if FALSE the alert has just been closed.
#' 
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     header = dashboardHeader(),
#'     sidebar = dashboardSidebar(),
#'     body = dashboardBody(
#'       tooltip(
#'        sliderInput("obs", "Observations:", 10, min = 1, max = 100),
#'        placement = "right",
#'        title = "Set me higher than 50!"
#'       ),
#'       div(id = "myalert", style = "position: absolute; bottom: 0; right: 0;")
#'     ),
#'     controlbar = dashboardControlbar()
#'   ),
#'   server = function(input, output, session) {
#'     observeEvent(input$obs, {
#'       if (input$obs > 50) {
#'        createAlert(
#'          id = "myalert",
#'          options = list(
#'           title = "Alert",
#'           closable = TRUE,
#'           width = 12,
#'           elevations = 4,
#'           status = "primary",
#'           content = "Alert content ..."
#'          )
#'        )
#'       } else {
#'        closeAlert(id = "myalert")
#'       }
#'       
#'     })
#' 
#'     observe(print(input$myalert))
#'     
#'     observeEvent(input$myalert, {
#'       status <- if (input$myalert) "opened" else "closed"
#'       toast(title = sprintf("Alert succesfully %s!", status))
#'     })
#'   }
#'  )
#' }
#' 
#' @rdname alert
bs4CreateAlert <- function(id = NULL, selector = NULL, options, session = shiny::getDefaultReactiveDomain()) {

  if (!is.null(id) && !is.null(selector)) {
    stop("Please choose either target or selector!")
  }

  message <- dropNulls(
    list(
      id = session$ns(id),
      selector = selector,
      options = options
    )
  )

  session$sendCustomMessage("create-alert", message)  
}





#' Close AdminLTE3 alert
#' 
#' \link{closeAlert} closes an alert created via \link{createAlert}.
#'
#' @param id Anchor id.
#' @param session Shiny session object.
#' @export
#' 
#' @rdname alert
bs4CloseAlert <- function(id, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage("close-alert", session$ns(id))
}