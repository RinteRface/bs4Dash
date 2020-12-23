#' Create a Bootstrap 4 dashboard badge item
#' 
#' \link{dashboardBadge} creates a badge. It may be inserted in any element like inside 
#' a \link[shiny]{actionButton} or a \link{dashboardSidebar}.
#'
#' @param ... Badge content.
#' @param color Badge color. Valid colors:
#' \itemize{
#'   \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#' }
#' @param position Badge position: "left" or "right".
#' @param rounded Whether the badge is rounded instead of square. FALSE by default.
#' 
#' @rdname badge
#'  
#' @examples 
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     dashboardHeader(),
#'     dashboardSidebar(),
#'     dashboardBody(
#'      dashboardBadge("Badge 1", color = "danger"),
#'      actionButton(
#'       inputId = "badge", 
#'       label = "Hello", 
#'       icon = NULL, 
#'       width = NULL, 
#'       dashboardBadge(1, color = "primary")
#'      )
#'     )
#'   ),
#'   server = function(input, output) { }
#'  )
#' }
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4Badge <- function(..., color, position = c("left", "right"),
                     rounded = FALSE) {
  
  validateStatus(color)
  position <- match.arg(position)
  
  shiny::tags$span(
    class = paste0(position, " badge", " badge-", color, if (rounded) " badge-pill"),
    ...
  )
}




#' Bootstrap 4 accordion container
#'
#' \link{accordion} creates an accordion container. 
#' Accordions are part of collapsible elements.
#'
#' @param ... slot for \link{accordionItem}.
#' @param id Unique accordion id.
#' @param width The width of the accordion.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' @rdname accordion
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     dashboardHeader(),
#'     dashboardSidebar(),
#'     dashboardBody(
#'       accordion(
#'        id = "accordion1",
#'         accordionItem(
#'           title = "Accordion 1 Item 1",
#'           status = "danger",
#'           collapsed = TRUE,
#'           "This is some text!"
#'         ),
#'         accordionItem(
#'           title = "Accordion 1 Item 2",
#'           status = "indigo",
#'           collapsed = FALSE,
#'           "This is some text!"
#'         )
#'       ),
#'       accordion(
#'        id = "accordion2",
#'         accordionItem(
#'           title = "Accordion 2 Item 1",
#'           status = "info",
#'           collapsed = TRUE,
#'           "This is some text!"
#'         ),
#'         accordionItem(
#'           title = "Accordion 2 Item 2",
#'           status = "success",
#'           collapsed = FALSE,
#'           "This is some text!"
#'         )
#'       )
#'     ),
#'     title = "Accordion"
#'   ),
#'   server = function(input, output) { }
#'  )
#' }
#'
#' @export
bs4Accordion <- function(..., id, width = 12) {
  
  items <- list(...)
  
  # patch that enables a proper accordion behavior
  # we add the data-parent non standard attribute to each
  # item. Each accordion must have a unique id.
  lapply(seq_along(items), FUN = function(i) {
    items[[i]]$children[[2]]$attribs[["data-parent"]] <<- paste0("#", id) 
    items[[i]]$children[[1]]$children[[1]]$children[[1]]$attribs[["href"]] <<- paste0("#collapse_", id, "_", i)
    items[[i]]$children[[2]]$attribs[["id"]] <<- paste0("collapse_", id, "_", i)
  })
  
  shiny::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    shiny::tags$div(
      class = "accordion",
      id = id,
      items
    )
  )
}


#' Bootstrap 4 accordion item
#' 
#' \link{accordionItem} is to be inserted in a \link{accordion}.
#'
#' @inheritParams bs4Card
#' 
#' @rdname accordion
#'
#' @export
bs4AccordionItem <- function(..., title, status = NULL, 
                             collapsed = TRUE, solidHeader = TRUE) {
  
  cl <- "card"
  if (!is.null(status)) {
    validateStatusPlus(status)
    cl <- paste0(cl, " card-", status)
  }
  
  if (!solidHeader) cl <- paste0(cl, " card-outline")
  
  shiny::tags$div(
    class = cl,
    
    # box header
    shiny::tags$div(
      class = "card-header",
      shiny::tags$h4(
        class = "card-title w-100",
        shiny::tags$a(
          class = "d-block w-100",
          href = NULL,
          `data-toggle` = "collapse",
          `aria-expanded` = if (collapsed) "false" else "true",
          class = if (collapsed) "collapsed",
          title
        )
      )
    ),
    
    shiny::tags$div(
      id = NULL,  
      `data-parent` = NULL,
      class = if (collapsed) {
        "collapse"
      } else {
        "collapse show"
      },
      #`aria-expanded` = if (isTRUE(collapsed)) "false" else "true",
      #style = if (isTRUE(collapsed)) "height: 0px;" else NULL,
      shiny::tags$div(class = "card-body", ...)
    )
  )
}





#' Update an accordion on the client
#' 
#' \link{updateAccordion} toggles an \link{accordion} on the client.
#'
#' @param id Accordion to target.
#' @param selected Index of the newly selected \link{accordionItem}.
#' @param session Shiny session object.
#'
#' @export
#' @rdname accordion
#' @examples
#' 
#' # Update accordion
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     dashboardHeader(),
#'     dashboardSidebar(),
#'     dashboardBody(
#'       radioButtons("controller", "Controller", choices = c(1, 2)),
#'       br(),
#'       accordion(
#'         id = "accordion1",
#'         accordionItem(
#'           title = "Accordion 1 Item 1",
#'           status = "danger",
#'           collapsed = TRUE,
#'           "This is some text!"
#'         ),
#'         accordionItem(
#'           title = "Accordion 1 Item 2",
#'           status = "warning",
#'           collapsed = TRUE,
#'           "This is some text!"
#'         )
#'       )
#'     ),
#'     title = "Update Accordion"
#'   ),
#'   server = function(input, output, session) {
#'     observeEvent(input$controller, {
#'       updateAccordion(id = "accordion1", selected = input$controller)
#'     })
#'     observe(print(input$accordion1))
#'     observeEvent(input$accordion1, {
#'       showNotification(sprintf("You selected accordion N° %s", input$accordion1), type = "message")
#'     })
#'   }
#'  )
#' }
updateAccordion <- function(id, selected, session = shiny::getDefaultReactiveDomain()) {
  session$sendInputMessage(id, selected)
}





#' Bootstrap 4 carousel
#' 
#' \link{carousel} creates a carousel container to display media content.
#'
#' @param ... Slot for \link{carouselItem}.
#' @param id Unique carousel id.
#' @param indicators Whether to display left and right indicators.
#' @param width Carousel width. Between 1 and 12.
#' @param .list Should you need to pass \link{carouselItem} via \link{lapply} or similar,
#' put these item here instead of passing them in ...
#' 
#' @examples 
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'    ui = dashboardPage(
#'      header = dashboardHeader(),
#'      sidebar = dashboardSidebar(),
#'      body = dashboardBody(
#'       carousel(
#'        id = "mycarousel",
#'        carouselItem(
#'         caption = "Item 1",
#'         tags$img(src = "https://placehold.it/900x500/3c8dbc/ffffff&text=I+Love+Bootstrap")
#'        ),
#'        carouselItem(
#'         caption = "Item 2",
#'         tags$img(src = "https://placehold.it/900x500/39CCCC/ffffff&text=I+Love+Bootstrap")
#'        )
#'       )
#'      ),
#'      title = "Carousel"
#'    ),
#'    server = function(input, output) { }
#'  )
#' }
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @rdname carousel
#' @family boxWidgets
#'
#' @export
bs4Carousel <- function(..., id, indicators = TRUE, width = 12, .list = NULL) {
  
  items <- c(list(...), .list)
  
  generateCarouselNav <- function(items) {
    found_active <- FALSE
    navs <- lapply(seq_along(items), FUN = function(i) {
      # if we found an active item, all other active items are ignored.
      active <- if (found_active) {
         FALSE
      } else {
        sum(grep(x = items[[i]]$attribs$class, pattern = "active")) == 1
      }
      # if the item has active class and no item was found before, we found the active item
      if (active && !found_active) found_active <- TRUE
      
      shiny::tags$li(
        `data-target` = paste0("#", id),
        `data-slide-to` = i - 1,
        class = if (active) "active"
      )
    })
    
    actives <- dropNulls(lapply(navs, function(nav) {
      nav$attribs$class
    }))
    
    # Make sure at least the first item is active
    if (length(actives) == 0) {
      navs[[1]]$attribs$class <- "active"
      items[[1]]$attribs$class <<- paste0(
        items[[1]]$attribs$class,
        " active"
      )
    }
    
    navs
    
  }
  
  indicatorsTag <- shiny::tags$ol(
    class = "carousel-indicators",
    generateCarouselNav(items)
  )
  
  bodyTag <- shiny::tags$div(
    class = "carousel-inner",
    items
  )
  
  controlButtons <- if (indicators) {
    shiny::tagList(
      # previous
      shiny::tags$a(
        class = "carousel-control-prev",
        href = paste0("#", id),
        role = "button",
        `data-slide` = "prev",
        shiny::tags$span(
          class = "carousel-control-prev-icon",
          `aria-hidden` = "true"
        ),
        shiny::tags$span(class = "sr-only", "Previous")
      ),
      # next
      shiny::tags$a(
        class = "carousel-control-next",
        href = paste0("#", id),
        role = "button",
        `data-slide` = "next",
        shiny::tags$span(
          class = "carousel-control-next-icon",
          `aria-hidden` = "true"
        ),
        shiny::tags$span(class = "sr-only", "Next")
      )
    )
  } else {
    NULL
  }
  
  carouselTag <- shiny::tags$div(
    class = "carousel slide",
    `data-ride` = "carousel",
    id = id
  )
  
  carouselTag <- shiny::tagAppendChildren(carouselTag, indicatorsTag, bodyTag, controlButtons)
  
  shiny::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    carouselTag
  )
  
}



#' Bootstrap 4 carousel item
#' 
#' \link{carouselItem} creates a carousel item to insert in a \link{carousel}
#' 
#' @param ... Element such as images, iframe, ...
#' @param caption Item caption.
#' @param active Whether the item is active or not at start.
#' 
#' @rdname carousel
#'
#' @export
bs4CarouselItem <- function(..., caption = NULL, active = FALSE) {
  shiny::tags$div(
    class = if (active) "carousel-item active" else "carousel-item",
    ..., 
    if (!is.null(caption)) {
      shiny::tags$div(class = "carousel-caption", caption)
    }
  )
}




#' AdminLTE3 progress bar
#' 
#' Create a Bootstrap 4 progress bar.
#'
#' @param value Progress bar value.
#' @param min Progress bar minimum value.
#' @param max Progress bar maximum value.
#' @param vertical Whether to display the progress bar in vertical mode. FALSE by default.
#' @param striped Whether the progress bar is striped or not. FALSE by default.
#' @param animated Whether to animate the progress bar. Default to FALSE.
#' @param status Progress bar status. Valid colors are defined as follows:
#' \itemize{
#'   \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item \code{gray-dark}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item \code{gray}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item \code{white}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#1f2d3d")}.
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
#' 
#' @param size Progress bar size. NULL, "sm", "xs" or "xxs".
#' @param label Progress label. NULL by default.
#' 
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'    ui = dashboardPage(
#'      header = dashboardHeader(),
#'      sidebar = dashboardSidebar(),
#'      body = dashboardBody(
#'       box(
#'        title = "Horizontal",
#'        progressBar(
#'         value = 10,
#'         striped = TRUE,
#'         animated = TRUE
#'        ),
#'        progressBar(
#'         value = 50,
#'         status = "warning",
#'         size = "xs"
#'        ),
#'        progressBar(
#'         value = 20,
#'         status = "danger",
#'         size = "sm"
#'        )
#'       ),
#'       box(
#'        title = "Vertical",
#'        progressBar(
#'         value = 10,
#'         striped = TRUE,
#'         animated = TRUE,
#'         vertical = TRUE
#'        ),
#'        progressBar(
#'         value = 50,
#'         status = "warning",
#'         size = "xs",
#'         vertical = TRUE
#'        ),
#'        progressBar(
#'         value = 20,
#'         status = "danger",
#'         size = "sm",
#'         vertical = TRUE
#'        )
#'       )
#'      ),
#'      title = "Progress bars"
#'    ),
#'    server = function(input, output) { }
#'  )
#' }

#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' @rdname progress
#'
#' @export
bs4ProgressBar <- function (value, min = 0, max = 100, vertical = FALSE, striped = FALSE, 
                            animated = FALSE, status = "primary", size = NULL, 
                            label = NULL) {
  
  if (!is.null(status)) validateStatusPlus(status)
  stopifnot(value >= min)
  stopifnot(value <= max)
  
  # wrapper class
  progressCl <- if (isTRUE(vertical)) "progress vertical" else "progress mb-3"
  if (!is.null(size)) progressCl <- paste0(progressCl, " progress-", size)
  
  # bar class
  barCl <- "progress-bar"
  if (!is.null(status)) barCl <- paste0(barCl, " bg-", status)
  if (striped) barCl <- paste0(barCl, " progress-bar-striped")
  if (animated) barCl <- paste0(barCl, " progress-bar-animated")
  
  # wrapper
  barTag <- shiny::tags$div(
    class = barCl, 
    role = "progressbar", 
    `aria-valuenow` = value, 
    `aria-valuemin` = min, 
    `aria-valuemax` = max, 
    style = if (vertical) {
      paste0("height: ", paste0(value, "%"))
    }
    else {
      paste0("width: ", paste0(value, "%"))
    }, 
    if(!is.null(label)) label
  )
  
  progressTag <- shiny::tags$div(class = progressCl)
  progressTag <- shiny::tagAppendChild(progressTag, barTag)
  progressTag
}






#' Create a Bootstrap 4 callout
#' 
#' AdminLTE3 callout
#'
#' @param ... Callout content.
#' @param title Callout title.
#' @param status Callout status. Valid statuses:
#' \itemize{
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#' }
#' @param width Callout width. Between 1 and 12.
#' @param elevation Callout elevation.
#' 
#' @rdname callout
#' 
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'    ui = dashboardPage(
#'      header = dashboardHeader(),
#'      sidebar = dashboardSidebar(),
#'      controlbar = dashboardControlbar(),
#'      footer = dashboardFooter(),
#'      title = "Callout",
#'      body = bs4DashBody(
#'        title = "Callouts",
#'        callout(
#'         title = "I am a danger callout!",
#'         elevation = 4,
#'         status = "danger",
#'         "There is a problem that we need to fix. 
#'         A wonderful serenity has taken possession of 
#'         my entire soul, like these sweet mornings of 
#'         spring which I enjoy with my whole heart."
#'        ),
#'        callout(
#'         title = "I am a danger callout!",
#'         status = "warning",
#'         "This is a yellow callout."
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
bs4Callout <- function(..., title, status = c("warning", "danger", "info", "success"),
                       width = 6, elevation = NULL) {
  
  validateStatus(status)
  status <- match.arg(status)
  
  calloutCl <- "callout"
  if (!is.null(status)) calloutCl <- paste0(calloutCl, " callout-", status)
  if (!is.null(elevation)) calloutCl <- paste0(calloutCl, " elevation-", elevation)
  
  calloutTag <- shiny::tags$div(
    class = calloutCl,
    shiny::tags$h5(title),
    ...
  )
  
  shiny::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    calloutTag
  )
}



#' @title AdminLTE3 loading state element
#'
#' @description When a section is still work in progress or a computation is running
#' 
#' @note Loading state can be programmatically used when a conputation is running for instance.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @rdname loading
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     dashboardHeader(),
#'     dashboardSidebar(),
#'     dashboardBody(
#'      box(
#'       title = "loading spinner",
#'       loadingState()
#'       )
#'     ),
#'     title = "Loading State"
#'   ),
#'   server = function(input, output) { }
#'  )
#' }
#'
#' @export
bs4Loading <- function() {
  shiny::tags$div(
    class = "overlay",
    shiny::tags$i(class = "fa fa-refresh fa-spin")
  )
}




#' AdminLTE3 timeline block
#'
#' \link{timelineBlock} creates a timeline block that may be inserted in a \link{box} or outside.
#'
#' @param ... slot for \link{bs4TimelineLabel} or \link{bs4TimelineItem}.
#' @param reversed Whether the timeline is reversed or not.
#' @param width Timeline width. Between 1 and 12.
#' 
#' @note reversed is useful when the user wants to use the timeline
#' inside a box.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' @rdname timeline
#' @family boxWidgets
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'
#'  shinyApp(
#'    ui = bs4DashPage(
#'     header = dashboardHeader(),
#'     sidebar = dashboardSidebar(),
#'     controlbar = dashboardControlbar(),
#'     footer = dashboardFooter(),
#'     title = "test",
#'     body = dashboardBody(
#'      box(
#'       title = "Timeline",
#'       timelineBlock(
#'        width = 12,
#'        reversed = TRUE,
#'        timelineEnd(color = "danger"),
#'        timelineLabel("10 Feb. 2014", color = "pink"),
#'        timelineItem(
#'         elevation = 4, 
#'         title = "Item 1",
#'         icon = icon("gears"),
#'         color = "olive",
#'         time = "now",
#'         footer = "Here is the footer",
#'         "This is the body"
#'        ),
#'        timelineItem(
#'         title = "Item 2",
#'         border = FALSE
#'        ),
#'        timelineLabel("3 Jan. 2014", color = "lightblue"),
#'        timelineItem(
#'         elevation = 2,
#'         title = "Item 3",
#'         icon = icon("paint-brush"),
#'         status = "orange",
#'         timelineItemMedia(image = "http://placehold.it/150x100"),
#'         timelineItemMedia(image = "http://placehold.it/150x100")
#'        ),
#'        timelineStart(color = "secondary")
#'       )
#'      )
#'     )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#'
#' @export
bs4Timeline <- function(..., reversed = TRUE, width = 6) {
  
  cl <- "timeline"
  if (isTRUE(reversed)) cl <- paste0(cl, " timeline-inverse")
  
  timelineTag <- shiny::tags$div(
    class = cl,
    ...
  )
  
  shiny::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    timelineTag
  )
  
}


#' AdminLTE3 timeline label
#'
#' \link{timelineLabel} creates a timeline label element to highlight an event.
#'
#' @param ... Any element.
#' @param color Label color. Valid colors are defined as follows:
#' \itemize{
#'   \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item \code{gray-dark}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item \code{gray}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item \code{white}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#1f2d3d")}.
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
#'
#' @rdname timeline
#' 
#' @export
bs4TimelineLabel <- function(..., color = NULL) {
  
  cl <- NULL
  if (!is.null(color)) {
    validateStatusPlus(color)
    cl <- paste0("bg-", color)
  }
  
  shiny::tags$div(
    class = "time-label",
    shiny::tags$span(
      class = cl,
      ...
    )
  )
}


#' AdminLTE3 timeline item
#'
#' \link{timelineItem} creates a timeline item that contains information for a 
#' given event like the title, description, date, ...
#'
#' @param ... Any element such as \link{timelineItemMedia} ...
#' @param icon Item icon. Expect \code{\link[shiny]{icon}}.
#' @param color Item color. Valid colors are defined as follows:
#' \itemize{
#'   \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item \code{gray-dark}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item \code{gray}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item \code{white}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#1f2d3d")}.
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
#' @param time Item date or time.
#' @param title Item title.
#' @param border Whether to display a border between the header and the body. TRUE by default.
#' @param footer Item footer if any.
#' @param elevation Timeline elevation (numeric). NULL by default.
#'
#' @rdname timeline
#' 
#' @export
bs4TimelineItem <- function(..., icon = NULL, 
                            color = NULL, time = NULL, title = NULL, 
                            border = TRUE, footer = NULL, elevation = NULL) {
  
  if (!is.null(color)) {
    validateStatusPlus(color)
    icon$attribs$class <- paste0(icon$attribs$class, " bg-", color)
  }
  
  if (!is.null(elevation)) {
    icon$attribs$class <- paste0(icon$attribs$class, " elevation-", elevation)
  }
  
  itemCl <- "timeline-header no-border"
  if (isTRUE(border)) itemCl <- "timeline-header"
  
  shiny::tags$div(
    
    # timelineItem icon and status
    icon,
    
    # timelineItem container
    shiny::tags$div(
      class = "timeline-item",
      
      #timelineItem time/date
      shiny::tags$span(
        class = "time",
        shiny::icon("clock-o"),
        time
      ),
      
      # timelineItem title
      shiny::tags$h3(
        class = if (!is.null(elevation)) {
          paste0(itemCl, " elevation-", elevation)
        } else {
          itemCl
        },
        title
      ),
      
      # timelineItem body
      shiny::tags$div(
        class = "timeline-body",
        ...
      ),
      
      # timelineItem footer
      shiny::tags$div(
        class = "timeline-footer",
        footer
      )
    )
  )
}


#' AdminLTE2 timeline media item
#'
#' \link{timelineItemMedia} create a specific container for images.
#'
#' @param image Media url or path.
#' @param height Media height in pixels.
#' @param width Media width in pixels.
#' 
#' @rdname timeline
#' 
#' @export
bs4TimelineItemMedia <- function(image = NULL, height = NULL, width = NULL) {
  shiny::img(
    class = "margin", 
    src = image, 
    height = height,
    width = width
  )
}




#' AdminLTE3 timeline starting point
#'
#' \link{timelineStart} indicates a starting point.
#'
#' @param icon Item icon such as "clock-o", "times", ...
#' @param color Item color. Valid colors are defined as follows:
#' \itemize{
#'   \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item \code{gray-dark}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item \code{gray}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item \code{white}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#1f2d3d")}.
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
#' 
#' @rdname timeline
#' 
#' @export
bs4TimelineStart <- function(icon = shiny::icon("clock-o"), color = NULL) {
  
  iconTag <- icon
  if (!is.null(color)) {
    validateStatusPlus(color)
    iconTag$attribs$class <- paste0(iconTag$attribs$class, " bg-", color)
  }
  
  shiny::tags$div(iconTag)
}


#' AdminLTE3 timeline ending point
#'
#' \link{timelineEnd} indicates an end point.
#'
#' @param icon Item icon such as "clock-o", "times", ...
#' @param color Item color. Valid colors are defined as follows:
#' \itemize{
#'   \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item \code{gray-dark}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item \code{gray}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item \code{white}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#1f2d3d")}.
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
#'
#' @rdname timeline
#' 
#' @export
bs4TimelineEnd <- function(icon = shiny::icon("hourglass-end"), color = NULL) {
  
  iconTag <- icon
  if (!is.null(color)) {
    validateStatusPlus(color)
    iconTag$attribs$class <- paste0(iconTag$attribs$class, " bg-", color)
  }
  
  shiny::tagList(
    shiny::tags$div(iconTag),
    shiny::br(), 
    shiny::br()
  )
}




#' @title AdminLTE3 stars
#'
#' @description Create a block of stars (ideal for rating)
#'
#' @param maxstar Maximum number of stars by block.
#' @param grade Current score. Should be positive and lower or equal to maxstar.
#' @param color Star color. Valid colors are listed below:
#' \itemize{
#'   \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item \code{gray-dark}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item \code{gray}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item \code{white}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#1f2d3d")}.
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
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @rdname stars
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     dashboardHeader(),
#'     dashboardSidebar(),
#'     dashboardBody(
#'      box(
#'       title = "Star example",
#'       starBlock(5),
#'       starBlock(5, color = "fuchsia"),
#'       starBlock(1, color = "danger"),
#'       starBlock(3, color = "secondary")
#'      )
#'     ),
#'     title = "starBlock"
#'   ),
#'   server = function(input, output) { }
#'  )
#' }
#'
#' @export
bs4Stars <- function(grade, maxstar = 5, color = "warning") {
  
  stopifnot(!is.null(color))
  validateStatusPlus(color)
  stopifnot(!is.null(grade))
  stopifnot(grade >= 0)
  stopifnot(grade <= maxstar)
  
  shiny::tags$td(
    class = "mailbox-star",
    shiny::tags$a(
      href = "javascript:void(0)",
      if (grade > 0) {
        full_star <- lapply(1:grade, FUN = function(i) {
          shiny::tags$i(class = paste0("fa text-", color, " fa-star"))
        })
      },
      if (grade < maxstar) {
        empty_star <- lapply(1:(maxstar - grade), FUN = function(i) {
          shiny::tags$i(class = paste0("fa text-", color, " fa-star-o"))
        })
      }
    ),
    shiny::tags$br()
  )
}




#' @title BS4 jumbotron for AdminLTE3
#'
#' @description Create a jumbotron
#'
#' @param ... Any content.
#' @param title Jumbotron title.
#' @param lead Jumbotron lead.
#' @param href Jumbrotron external link.
#' @param btnName Jumbotron button name.
#' @param status Jumbotron background color. "primary", "success", "warning", "danger" or "info".
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' @rdname jumbotron
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'    ui = dashboardPage(
#'      header = dashboardHeader(),
#'      sidebar = dashboardSidebar(),
#'      controlbar = dashboardControlbar(),
#'      footer = dashboardFooter(),
#'      title = "Jumbotron",
#'      body = dashboardBody(
#'       jumbotron(
#'       title = "Hello, world!",
#'       lead = "This is a simple hero unit, a simple jumbotron-style 
#'       component for calling extra attention to featured 
#'       content or information.",
#'       "It uses utility classes for typography and spacing 
#'       to space content out within the larger container.",
#'       status = "primary",
#'       href = "http://www.google.fr"
#'       )
#'      )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#'
#' @export
bs4Jumbotron <- function(..., title = NULL, lead = NULL, href = NULL, btnName = "More",
                         status = c("primary", "warning", "danger", "info", "success")) {
  
  status <- match.arg(status)
  
  # uncomment below if more status are enabled
  #if (status == "dark") btnStatus <- "gray" else btnStatus <- "dark"
  btnStatus <- "secondary"
  
  jumboCl <- "jumbotron"
  if (!is.null(status)) jumboCl <- paste0(jumboCl, " bg-", status)
  
  # no need to wrap this tag in an external div to set a custom width
  # since the jumbotron will take the whole page width
  shiny::tags$div(
    class = jumboCl,
    shiny::tags$h1(class = "display-4", title),
    shiny::tags$p(class = "lead", lead),
    shiny::tags$hr(class = "my-4"),
    shiny::tags$p(...),
    if (!is.null(btnName)) {
      shiny::tags$a(
        class = paste0("btn btn-", btnStatus, " btn-lg"),
        href = href,
        target = "_blank",
        role = "button",
        btnName
      )
    }
  )
}



#' @title BS4 list group for AdminLTE3
#'
#' @description Create a list group
#'
#' @param ... Slot for \link{listGroupItem}.
#' @param type List group type. 
#' @param width List group width. 4 by default. Between 1 and 12.
#' @param .list Slot for programmatically generated items.
#' 
#' @rdname listgroup
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'    ui = dashboardPage(
#'      header = dashboardHeader(),
#'      sidebar = dashboardSidebar(),
#'      controlbar = dashboardControlbar(),
#'      footer = dashboardFooter(),
#'      title = "test",
#'      body = dashboardBody(
#'       fluidRow(
#'        listGroup(
#'         type = "basic",
#'         listGroupItem("Cras justo odio"),
#'         listGroupItem("Dapibus ac facilisis in"),
#'         listGroupItem("Morbi leo risus")
#'        ),
#'        listGroup(
#'         type = "action",
#'         listGroupItem(
#'          "Cras justo odio",
#'          active = TRUE, 
#'          disabled = FALSE, 
#'          href = "http://www.google.fr"
#'         ),
#'         listGroupItem(
#'          active = FALSE, 
#'          disabled = FALSE, 
#'          "Dapibus ac facilisis in",
#'          href = "http://www.google.fr"
#'         ),
#'         listGroupItem(
#'          "Morbi leo risus",
#'          active = FALSE, 
#'          disabled = TRUE, 
#'          href = "http://www.google.fr"
#'         )
#'        ),
#'        listGroup(
#'         type = "heading",
#'         listGroupItem(
#'          "Donec id elit non mi porta gravida at eget metus. 
#'          Maecenas sed diam eget risus varius blandit.",
#'          active = TRUE, 
#'          disabled = FALSE, 
#'          title = "List group item heading", 
#'          subtitle = "3 days ago", 
#'          footer = "Donec id elit non mi porta."
#'         ),
#'         listGroupItem(
#'          "Donec id elit non mi porta gravida at eget metus. 
#'          Maecenas sed diam eget risus varius blandit.",
#'          active = FALSE, 
#'          disabled = FALSE, 
#'          title = "List group item heading", 
#'          subtitle = "3 days ago", 
#'          footer = "Donec id elit non mi porta."
#'         )
#'        )
#'      )
#'     )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#'
#' @export
bs4ListGroup <- function(..., type = c("basic", "action", "heading"), width = 4, 
                         .list = NULL) {
  
  items <- c(list(...), .list)
  type <- match.arg(type)
  
  # item class depends on selected type
  itemCl <- switch(
    type,
    "basic" = "list-group-item d-flex justify-content-between align-items-center",
    "action" = "list-group-item list-group-item-action",
    "heading" = "list-group-item list-group-item-action flex-column align-items-start"
  )
  
  # build items based on type and options passed
  itemsTag <- lapply(items, function(item) {
    names(item)[1] <- "body"
    if (item$active) itemCl <- paste0(itemCl, " active")
    if (item$disabled) itemCl <- paste0(itemCl, " disabled")
    # item tag
    if (type == "basic") {
      shiny::tags$li(
        class = itemCl,
        item$body
      )
    } else if (type == "action") {
      shiny::tags$a(
        class = itemCl,
        href = item$href,
        target = if (!is.null(item$href)) "_blank",
        item$body
      )
    } else {
      shiny::tags$a(
        class = itemCl,
        href = item$href,
        target = if (!is.null(item$href)) "_blank",
        shiny::tags$div(
          class = "d-flex w-100 justify-content-between",
          shiny::tags$h5(class = "mb-1", item$title),
          if (!is.null(item$subtitle)) {
            shiny::tags$small(item$subtitle)
          }
        ),
        shiny::tags$p(class = "mb-1", item$body),
        if (!is.null(item$footer)) {
          shiny::tags$small(class = if (item$active) NULL else "text-muted", item$footer)
        }
      )
    }
  })
  
  
  listGroupTag <- shiny::tags$ul(
    class = "list-group",
    itemsTag
  )
  
  shiny::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    listGroupTag
  )
  
}




#' @title BS4 list group item for AdminLTE3
#'
#' @description Create a list group item
#'
#' @param ... Item content.
#' @param title Item title (only if type is "heading").
#' @param subtitle Item subtitle (only if type is "heading").
#' @param footer Item footer content (only if type is "heading").
#' @param active Whether the item is active or not. FALSE by default. 
#' Only if type is "action" or "heading".
#' @param disabled Whether the item is disabled or not. FALSE by default. 
#' Only if type is "action" or "heading".
#' @param href Item external link.
#' 
#'
#' @rdname listgroup
#'
#' @export
bs4ListGroupItem <- function(..., title = NULL, subtitle = NULL, 
                             footer = NULL, active = FALSE, disabled = FALSE,
                             href = NULL) {
  
  if (active && disabled) {
    stop("active and disabled cannot be TRUE at the same time!")
  }

  list(
    body = ...,
    title = title,
    subtitle = subtitle,
    footer = footer,
    active = active,
    disabled = disabled,
    href = href
  )
}




#' @title BS4 ionicons
#'
#' @description Create a ionicon. 
#'
#' @param name Name of icon. See \url{https://ionicons.com}.
#' 
#' @note Similar to the icon function from shiny.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'    ui = dashboardPage(
#'      header = dashboardHeader(),
#'      sidebar = dashboardSidebar(),
#'      controlbar = dashboardControlbar(),
#'      footer = dashboardFooter(),
#'      title = "Ionicons",
#'      body = dashboardBody(
#'       ionicon(name ="heart"),
#'       ionicon(name ="beer")
#'     )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#'
#' @export
ionicon <- function(name) {
  if (is.null(name)) stop("Missing icon name")
  cl <- paste0("icon ion-md-", name)
  shiny::tags$i(class = cl)
}



#' AdminLTE3 attachment container
#'
#' \link{attachmentBlock} create an attachment container, nice to wrap articles...
#' and insert in a \link{box}.
#'
#' @param ... Any element.
#' @param image url or path to the image.
#' @param title Attachment title.
#' @param href External link.
#' 
#' @family boxWidgets
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     dashboardHeader(),
#'     dashboardSidebar(),
#'     dashboardBody(
#'      box(
#'       title = "attachmentBlock example",
#'       attachmentBlock(
#'        image = "https://adminlte.io/themes/AdminLTE/dist/img/photo1.png",
#'        title = "Test",
#'        href = "https://google.com",
#'        "This is the content"
#'       )
#'      )
#'     ),
#'     title = "attachmentBlock"
#'   ),
#'   server = function(input, output) { }
#'  )
#' }
#'
#' @export
attachmentBlock <- function(..., image, title = NULL, href = NULL) {
  shiny::tags$div(
    class = "attachment-block clearfix",
    shiny::img(
      class = "attachment-img",
      src = image
    ),
    shiny::tags$div(
      class = "attachment-pushed",
      if (!is.null(title)) {
        shiny::tags$h4(
          class = "attachment-heading",
          shiny::tags$a(
            href = if (!is.null(href)) {
              href
            } else {
              "#"
            },
            target = if (!is.null(href)) {
              "_blank"
            },
            title
          )
        ) 
      },
      shiny::tags$div(
        class = "attachment-text",
        ...
      )
    )
  )
}



#' AdminLTE3 description block
#'
#' \link{descriptionBlock} creates a description block, perfect for writing statistics 
#' to insert in a \link{box}.
#'
#' @param number Any number.
#' @param numberColor Number color. Valid colors are defined as follows:
#' \itemize{
#'   \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item \code{gray-dark}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item \code{gray}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item \code{white}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#1f2d3d")}.
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
#' @param numberIcon Number icon, if any. Expect \code{\link[shiny]{icon}}.
#' @param header Bold text.
#' @param text Additional text.
#' @param rightBorder TRUE by default. Whether to display a right border to
#'   separate two blocks. The last block on the right should not have a right border.
#' @param marginBottom FALSE by default. Set it to TRUE when the
#'   descriptionBlock is used in a \link{boxPad} context.
#'   
#' @rdname box
#' @family boxWidgets
#'
#' @examples
#' # Box with descriptionBlock
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     dashboardHeader(),
#'     dashboardSidebar(),
#'     dashboardBody(
#'      box(
#'       solidHeader = FALSE,
#'       title = "Status summary",
#'       background = NULL,
#'       width = 4,
#'       status = "danger",
#'       footer = fluidRow(
#'         column(
#'           width = 6,
#'           descriptionBlock(
#'             number = "17%", 
#'             numberColor = "pink", 
#'             numberIcon = icon("caret-up"),
#'             header = "$35,210.43", 
#'             text = "TOTAL REVENUE", 
#'             rightBorder = TRUE,
#'             marginBottom = FALSE
#'           )
#'         ),
#'         column(
#'           width = 6,
#'           descriptionBlock(
#'             number = "18%", 
#'             numberColor = "secondary", 
#'             numberIcon = icon("caret-down"),
#'             header = "1200", 
#'             text = "GOAL COMPLETION", 
#'             rightBorder = FALSE,
#'             marginBottom = FALSE
#'           )
#'         )
#'       )
#'      )
#'     ),
#'     title = "Description Blocks"
#'   ),
#'   server = function(input, output) { }
#'  )
#' }
#'
#' @export
descriptionBlock <- function(number = NULL, numberColor = NULL, numberIcon = NULL,
                             header = NULL, text = NULL, rightBorder = TRUE,
                             marginBottom = FALSE) {
  
  cl <- "description-block"
  if (rightBorder) cl <- paste0(cl, " border-right")
  if (marginBottom) cl <- paste0(cl, " mb-4")
  
  numcl <- "description-percentage"
  if (!is.null(numberColor)) {
    validateStatusPlus(numberColor)
    numcl <- paste0(numcl, " text-", numberColor)
  }
  
  shiny::tags$div(
    class = cl,
    shiny::tags$span(
      class = numcl, 
      number,
      if (!is.null(numberIcon)) numberIcon
    ),
    shiny::tags$h5(class = "description-header", header),
    shiny::tags$span(class = "description-text", text)
  )
}



#' AdminLTE3 vertical block container
#'
#' \link{boxPad} creates a vertical container for \link{descriptionBlock}.
#' It has to be included in a \link{box}.
#'
#' @param ... Any element such as \link{descriptionBlock}.
#' @param color Background color. Valid colors are defined as follows:
#' \itemize{
#'   \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item \code{gray-dark}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item \code{gray}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item \code{white}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#1f2d3d")}.
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
#' @param style Custom CSS, if any.
#' 
#' @rdname box
#' @family boxWidgets
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     dashboardHeader(),
#'     dashboardSidebar(),
#'     dashboardBody(
#'      box(
#'       title = "Box with right pad",
#'       status = "warning",
#'       fluidRow(
#'         column(width = 6),
#'         column(
#'           width = 6,
#'           boxPad(
#'             color = "purple",
#'             descriptionBlock(
#'               header = "8390", 
#'               text = "VISITS", 
#'               rightBorder = FALSE,
#'               marginBottom = TRUE
#'             ),
#'             descriptionBlock(
#'               header = "30%", 
#'               text = "REFERRALS", 
#'               rightBorder = FALSE,
#'               marginBottom = TRUE
#'             ),
#'             descriptionBlock(
#'               header = "70%", 
#'               text = "ORGANIC", 
#'               rightBorder = FALSE,
#'               marginBottom = FALSE
#'             )
#'           )
#'         )
#'       )
#'      )
#'     ),
#'     title = "boxPad"
#'   ),
#'   server = function(input, output) { }
#'  )
#' }
#'
#' @export
cardPad <- function(..., color = NULL, style = NULL) {
  cl <- "card-pane-right pt-2 pb-2 pl-4 pr-4"
  if (!is.null(color)) {
    validateStatusPlus(color)
    cl <- paste0(cl, " bg-", color)
  }
  
  shiny::tags$div(
    class = cl,
    style = style,
    ...
  )
}






#' AdminLTE3 product list container
#'
#' \link{productList} creates a container to display commercial items in an elegant container.
#' Insert in a \link{box}.
#'
#' @param ... slot for \link{productListItem}.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' @rdname productList
#'
#' @examples
#' 
#' # Box with productList
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     dashboardHeader(),
#'     dashboardSidebar(),
#'     dashboardBody(
#'      box(
#'       title = "Product List",
#'       status = "primary",
#'       productList(
#'         productListItem(
#'           image = "https://www.pngmart.com/files/1/Haier-TV-PNG.png", 
#'           title = "Samsung TV", 
#'           subtitle = "$1800", 
#'           color = "warning",
#'           "This is an amazing TV, but I don't like TV!"
#'         ),
#'         productListItem(
#'           image = "https://upload.wikimedia.org/wikipedia/commons/7/77/IMac_Pro.svg", 
#'           title = "Imac 27", 
#'           subtitle = "$4999", 
#'           color = "danger",
#'           "This is were I spend most of my time!"
#'         )
#'       )
#'      )
#'     ),
#'     title = "Product List"
#'   ),
#'   server = function(input, output) { }
#'  )
#' }
#'
#' @export
productList <- function(...) {
  shiny::tags$ul(
    class = "products-list product-list-in-card pl-2 pr-2",
    ...
  )
}




#' AdminLTE3 product item
#'
#' \link{productListItem} creates a product item to insert in \link{productList}.
#'
#' @param ... product description.
#' @param image image url, if any.
#' @param title product name.
#' @param subtitle product price.
#' @param color price color. Valid color are listed below:
#' \itemize{
#'  \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#' }
#' @rdname productList
#'
#' @export
productListItem <- function(..., image = NULL, title = NULL, 
                            subtitle = NULL, color = NULL) {
  cl <- "badge float-right"
  if (!is.null(color)) {
    validateStatus(color)
    cl <- paste0(cl, " badge-", color)
  }
  
  shiny::tags$li(
    class = "item",
    shiny::tags$div(
      class = "product-img",
      shiny::tags$img(src = image, alt = "Product Image")
    ),
    shiny::tags$div(
      class = "product-info",
      shiny::tags$a(
        href = "javascript:void(0)", 
        class = "product-title",
        title,
        if (!is.null(subtitle)) shiny::tags$span(class = cl, subtitle)
      ),
      shiny::tags$span(
        class = "product-description",
        ...
      )
    )
  )
}





#' AdminLTE3 user list container
#'
#' \link{userList} creates a user list container to be inserted in a \link{box}.
#'
#' @param ... slot for \link{userListItem}.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' @rdname userList
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     dashboardHeader(),
#'     dashboardSidebar(),
#'     dashboardBody(
#'      box(
#'       title = "User List example",
#'       status = "success",
#'       userList(
#'         userListItem(
#'           image = "https://adminlte.io/themes/v3/dist/img/user1-128x128.jpg", 
#'           title = "Shiny", 
#'           subtitle = "Package 1"
#'         ),
#'         userListItem(
#'           image = "https://adminlte.io/themes/v3/dist/img/user8-128x128.jpg", 
#'           title = "Tidyverse", 
#'           subtitle = "Package 2"
#'         ),
#'         userListItem(
#'           image = "https://adminlte.io/themes/v3/dist/img/user7-128x128.jpg", 
#'           title = "tidyr", 
#'           subtitle = "Package 3"
#'         )
#'       )
#'      )
#'     ),
#'     title = "User List"
#'   ),
#'   server = function(input, output) { }
#'  )
#' }
#'
#' @export
userList <- function(...) {
  shiny::tags$ul(
    class = "users-list clearfix",
    ...
  )
}


#' AdminLTE3 user list item
#'
#' \link{userListItem} creates a user list item.
#'
#' @param image image url or path.
#' @param title Item title.
#' @param subtitle Item subtitle.
#'
#' @rdname userList
#'
#' @export
userListItem <- function(image, title, subtitle = NULL) {
  shiny::tags$li(
    shiny::tags$img(
      src = image, 
      alt = "User Image",
      shiny::tags$a(class = "users-list-name", title),
      if (!is.null(subtitle)) {
        shiny::tags$span(class = "users-list-date", subtitle)
      }
    )
  )
}






#' AdminLTE3 user message container
#'
#' \link{userMessages} creates a user message container. Maybe inserted in a \link{box}.
#'
#' @param ... Slot for \link{userMessage}.
#' @param id Optional. To use with \link{updateUserMessages}.
#' @param status Messages status. Valid colors are defined as follows:
#' \itemize{
#'   \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item \code{gray-dark}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item \code{gray}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item \code{white}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#1f2d3d")}.
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
#' @param width Container width: between 1 and 12.
#' @param height Container height. 
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' @rdname userMessage
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     dashboardHeader(),
#'     dashboardSidebar(),
#'     dashboardBody(
#'      box(
#'       title = "Box with messages",
#'       solidHeader = TRUE,
#'       status = "warning",
#'       userMessages(
#'        width = 12,
#'        status = "teal",
#'        userMessage(
#'          author = "Alexander Pierce",
#'          date = "20 Jan 2:00 pm",
#'          image = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
#'          type = "sent",
#'          "Is this template really for free? That's unbelievable!"
#'        ),
#'        userMessage(
#'          author = "Sarah Bullock",
#'          date = "23 Jan 2:05 pm",
#'          image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
#'          type = "received",
#'          "You better believe it!"
#'        )
#'       )
#'      ),
#'      userMessages(
#'        width = 6,
#'        status = "danger",
#'         userMessage(
#'          author = "Alexander Pierce",
#'          date = "20 Jan 2:00 pm",
#'          image = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
#'          type = "received",
#'          "Is this template really for free? That's unbelievable!"
#'        ),
#'        userMessage(
#'          author = "Sarah Bullock",
#'          date = "23 Jan 2:05 pm",
#'          image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
#'          type = "sent",
#'          "You better believe it!"
#'        )
#'       )
#'     ),
#'     title = "user Message"
#'   ),
#'   server = function(input, output) { }
#'  )
#' }
#'
#' @export
userMessages <- function(..., id = NULL, status, width = 4, height = NULL) {
  cl <- "direct-chat-messages direct-chat"
  if (!is.null(height)) shiny::validateCssUnit(height)
  if (!is.null(status)) {
    validateStatusPlus(status)
    cl <- paste0(cl, " direct-chat-", status)
  }
  msgtag <- shiny::tags$div(
    class = cl, 
    ..., 
    style = if (!is.null(height)) {
      sprintf("height: %s; overflow-y: auto;", height)
    } else {
      "height: 100%;"
    }
  )
  
  shiny::tags$div(
    id = id,
    class = if (!is.null(width)) paste0("col-sm-", width),
    msgtag
  )
  
}





#' AdminLTE3 user message 
#'
#' \link{userMessage} creates a user message html element.
#'
#' @param ... Message text.
#' @param author Message author.
#' @param date Message date.
#' @param image Message author image path or url.
#' @param type Message type: \code{c("sent", "received")}.
#'
#' @rdname userMessage
#'
#' @export
userMessage <- function(..., author = NULL, date = NULL, 
                        image = NULL, type = c("sent", "received")) {
  
  type <- match.arg(type)
  messageCl <- "direct-chat-msg"
  if (type == "sent") messageCl <- paste0(messageCl, " right")
  
  # message info
  messageInfo <- shiny::tags$div(
    class = "direct-chat-info clearfix",
    shiny::tags$span(
      class = if (type == "right") {
        "direct-chat-name float-right"
      } else {
        "direct-chat-name"
      }, 
      author
    ),
    if (!is.null(date)) {
      shiny::tags$span(
        class = if (type == "right") {
          "direct-chat-timestamp float-right"
        } else {
          "direct-chat-timestamp"
        }, 
        date
      )
    }
  )
  
  # message Text
  messageTxt <- shiny::tags$div(class = "direct-chat-text", ...)
  
  # message author image
  messageImg <- shiny::tags$img(class = "direct-chat-img", src = image)
  
  shiny::tags$div(
    class = messageCl,
    messageInfo,
    messageImg, 
    messageTxt
  )
}




#' Update a messages container in the server side
#' 
#' \link{updateUserMessages} allows to interact with a \link{userMessages} container,
#' such as sending, removing or editing messages.
#'
#' @param id \link{userMessages} to target.
#' @param action Action to perform: add, remove or update.
#' @param index Index of item to update or remove.
#' @param content New message content in a list. For actions like add and update only! See example.
#' @param session Shiny session object.
#' @export
#' @rdname userMessage
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     dashboardHeader(),
#'     dashboardSidebar(),
#'     dashboardBody(
#'       fluidRow(
#'         actionButton("remove", "Remove message"),
#'         actionButton("add", "Add message"),
#'         actionButton("update", "Update message")
#'       ),
#'       numericInput("index", "Message index:", 1, min = 1, max = 3),
#'       br(),
#'       br(),
#'       userMessages(
#'         width = 6,
#'         status = "danger",
#'         id = "message",
#'         userMessage(
#'           author = "Alexander Pierce",
#'           date = "20 Jan 2:00 pm",
#'           image = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
#'           type = "received",
#'           "Is this template really for free? That's unbelievable!"
#'         ),
#'         userMessage(
#'           author = "Sarah Bullock",
#'           date = "23 Jan 2:05 pm",
#'           image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
#'           type = "sent",
#'           "You better believe it!"
#'         )
#'       )
#'     ),
#'     title = "user Message"
#'   ),
#'   server = function(input, output, session) {
#'     observeEvent(input$remove, {
#'       updateUserMessages("message", action = "remove", index = input$index)
#'     })
#'     observeEvent(input$add, {
#'       updateUserMessages(
#'         "message", 
#'         action = "add", 
#'         content = list(
#'           author = "David",
#'           date = "Now",
#'           image = "https://i.pinimg.com/originals/f1/15/df/f115dfc9cab063597b1221d015996b39.jpg",
#'           type = "received",
#'           text = tagList(
#'            sliderInput(
#'             "obs", 
#'             "Number of observations:",
#'             min = 0, 
#'             max = 1000, 
#'             value = 500
#'            ),
#'            plotOutput("distPlot")
#'           )
#'         )
#'       )
#'     })
#'     
#'     output$distPlot <- renderPlot({
#'      hist(rnorm(input$obs))
#'     })
#'     
#'     observeEvent(input$update, {
#'       updateUserMessages(
#'         "message", 
#'         action = "update", 
#'         index = input$index,
#'         content = list(
#'          text = tagList(
#'           appButton(
#'            inputId = "reload",
#'            label = "Click me!", 
#'            icon = icon("sync"), 
#'            dashboardBadge(1, color = "orange")
#'           )
#'          )
#'         )
#'       )
#'     })
#'     
#'     observeEvent(input$reload, {
#'      showNotification("Yeah!", duration = 1, type = "default")
#'     })
#'   }
#'  )
#' }
updateUserMessages <- function(id, action = c("add", "remove", "update"), 
                               index = NULL, content = NULL, 
                               session = shiny::getDefaultReactiveDomain()) {
  action <- match.arg(action)
  
  content <- lapply(content, function(c) {
    if (inherits(c, "shiny.tag") || inherits(c, "shiny.tag.list")) {
      # necessary if the user pass input/output with deps
      # that are not yet available in the page before inserting the new tag
      c <- processDeps(c, session)
    }
    c
  })
  
  session$sendCustomMessage(
    "user-messages", 
    list(
      id = id, 
      action = action, 
      index = index,
      body = content
    )
  )
}




#' AdminLTE3 user post
#'
#' Creates a user post. This content may be inserted in a \link{box}.
#'
#' @param ... Post content, slot for \link{userPostTagItems}, \link{userPostMedia}.
#' @param id Unique id of the post.
#' @param image Profile image, if any.
#' @param author Post author.
#' @param description Post description.
#' @param collapsible If TRUE, display a button in the upper right that allows the user to collapse the comment. 
#' @param collapsed Whether the comment is collapsed when the application starts, FALSE by default.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' @rdname userPost
#' @family boxWidgets
#' 
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     dashboardHeader(),
#'     dashboardSidebar(),
#'     dashboardBody(
#'      box(
#'       title = "Box with user comment",
#'       status = "primary",
#'       userPost(
#'        id = 1,
#'        image = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
#'        author = "Jonathan Burke Jr.",
#'        description = "Shared publicly - 7:30 PM today",
#'        "Lorem ipsum represents a long-held tradition for designers, 
#'        typographers and the like. Some people hate it and argue for 
#'        its demise, but others ignore the hate as they create awesome 
#'        tools to help create filler text for everyone from bacon 
#'        lovers to Charlie Sheen fans.",
#'        collapsible = FALSE,
#'        userPostTagItems(
#'         userPostTagItem(dashboardBadge("item 1", color = "info")),
#'         userPostTagItem(dashboardBadge("item 2", color = "danger"), side = "right")
#'        )
#'       ),
#'       userPost(
#'        id = 2,
#'        image = "https://adminlte.io/themes/AdminLTE/dist/img/user6-128x128.jpg",
#'        author = "Adam Jones",
#'        userPostMedia(image = "https://adminlte.io/themes/AdminLTE/dist/img/photo2.png"),
#'        userPostTagItems(
#'         userPostTagItem(dashboardBadge("item 1", color = "success")),
#'         userPostTagItem(dashboardBadge("item 2", color = "danger"), side = "right")
#'        )
#'       )
#'      )
#'     ),
#'     title = "userPost"
#'   ),
#'   server = function(input, output) { }
#'  )
#' }
#' 
#' @export
userPost <- function(..., id = NULL, image, author, 
                     description = NULL, collapsible = TRUE, 
                     collapsed = FALSE) {
  
  id <- paste0("post-", id)
  
  btnCl <- "btn-tool float-right"
  
  
  # if the input tag is an image, it is better to center it...
  items <- list(...)
  items <- lapply(seq_along(items), function(i) {
    # do not apply to other things than tags
    if (class(items[[i]]) == "shiny.tag") {
      if (items[[i]]$name == "img") {
        # wrap the image item in a div to align its content
        shiny::tags$div(
          style = "text-align: center;",
          items[[i]]
        )
      } else {
        items[[i]]
      }
    } else {
      items[[i]]
    }
  })
  
  
  shiny::tags$div(
    class = "post",
    
    shiny::tags$div(
      class = "user-block",
      shiny::img(class = "img-circle img-bordered-sm", src = image),
      shiny::tags$span(
        class = "username", 
        author,
        # box tool
        if (collapsible) {
          shiny::tags$a(
            class = btnCl,
            `data-toggle` = "collapse",
            `data-target` = paste0("#", id),
            `aria-expanded` = tolower(!collapsed),
            `aria-controls` = id,
            if (collapsed) {
              shiny::tags$i(class = "fa fa-plus")
            } else {
              shiny::tags$i(class = "fa fa-minus")
            }
          )
        }
        
      ),
      if (!is.null(description)) {
        shiny::tags$span(class = "description", description)
      }
    ),
    shiny::tags$div(
      class = if (collapsible) {
        if (!collapsed) {
          "collapse show"
        } else {
          "collapse"
        }
      },
      id = id,
      items 
    )
  )
  
}




#' AdminLTE3 user post tool item container
#'
#' \link{userPostTagItems} creates a container to host \link{userPostTagItem}.
#'
#' @param ... Slot for \link{userPostTagItem}.
#'
#' @rdname userPost
#' 
#' @export
userPostTagItems<- function(...) {
  
  shiny::tags$ul(
    class = "list-inline d-flex",
    ...
  )
}




#' AdminLTE3 user post tool item
#'
#' \link{userPostTagItem} creates a user post tool item
#'
#' @param ... Tool content such as label, button, ...
#'
#' @rdname userPost
#' 
#' @export
userPostTagItem <- function(...) {
  
  shiny::tags$li(
    class = "mx-2",
    ...
  )
}



#' AdminLTE3 user post media
#'
#' \link{userPostMedia} creates a container to include an image in \link{userPost}.
#'
#' @param image Image path or url ...
#' @param height Media height in pixels.
#' @param width Media width in pixels.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @export
userPostMedia <- function(image, height = NULL, width = NULL) {
  shiny::img(
    style = "margin: auto;",
    class = "img-fluid", 
    src = image,
    height = height,
    width = width
  )
}



#' @title BS4 sortable section
#'
#' @description Create a sortable UI section
#'
#' @param ... Slot for UI elements such as \link{box}.
#' @param width Section width: between 1 and 12.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' @rdname sortable
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
#'     title = "Sortable UI",
#'     body = dashboardBody(
#'       fluidRow(
#'        lapply(1:3, FUN = function(i) {
#'          sortable(
#'            width = 4,
#'            p(class = "text-center", paste("Column", i)),
#'            lapply(1:2, FUN = function(j) {
#'              box(
#'                title = paste0("I am the ", j,"-th card of the ", i, "-th column"), 
#'                width = 12,
#'                "Click on my header"
#'              )
#'            })
#'          )
#'        })
#'       )
#'     )
#'   ),
#'   server = function(input, output) {}
#'  )
#' }  
#' 
#' @export
bs4Sortable <- function(..., width = 12) {
  
  sectionCl <- "connectedSortable ui-sortable"
  if (!is.null(width)) sectionCl <- paste0(sectionCl, " col-lg-", width)
  
  shiny::tags$section(
    class = sectionCl,
    ...
  ) 
}






#' Boostrap 4 table container
#'
#' Build an argon table container
#'
#' @param ... \link{bs4TableItems}.
#' @param cardWrap Whether to wrap the table in a card. FALSE by default.
#' @param headTitles Table header names. Must have the same length as the number of 
#' \link{bs4TableItem} in \link{bs4TableItems}. Set "" to have an empty title field.
#' @param bordered Whether to display border between elements. FALSE by default.
#' @param striped Whether to displayed striped in elements. FALSE by default.
#' @param width Table width. 12 by default.
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
#'      bs4Table(
#'       cardWrap = TRUE,
#'       bordered = TRUE,
#'       striped = TRUE,
#'       headTitles = c(
#'        "PROJECT",
#'        "BUDGET",
#'        "STATUS",
#'        "USERS",
#'        "COMPLETION",
#'        ""
#'       ),
#'       bs4TableItems(
#'        bs4TableItem("bs4 Design System"),
#'        bs4TableItem(dataCell = TRUE, "$2,500 USD"),
#'        bs4TableItem(
#'         dataCell = TRUE, 
#'         dashboardBadge(
#'          "Pending",
#'          position = "right",
#'          color = "danger",
#'          rounded = TRUE
#'         )
#'        ),
#'        bs4TableItem(
#'         progressBar(value = 50)
#'        ),
#'        bs4TableItem(
#'         dataCell = TRUE, 
#'         "test"
#'        ),
#'        bs4TableItem(
#'         actionButton(
#'          "go",
#'          "Go"
#'         )
#'        )
#'       )
#'      )
#'     ), 
#'     footer = dashboardFooter()
#'   ),
#'   server = function(input, output) { }
#'  )
#' }
#' 
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' @rdname table
#'
#' @export
bs4Table <- function(..., cardWrap = FALSE, headTitles, bordered = FALSE, 
                     striped = FALSE, width = 12) {
  
  # handle theme
  tableCl <- "table"
  if (bordered) tableCl <- paste0(tableCl, " table-bordered")
  if (striped) tableCl <- paste0(tableCl, " table-striped")
  
  # column headers
  tableHead <- shiny::tags$thead(
    shiny::tags$tr(
      lapply(seq_along(headTitles), function(i) shiny::tags$th(headTitles[[i]])) 
    )
  )
  
  # body rows
  tableBody <- shiny::tags$tbody(...)
  
  # table tag
  tableTag <- shiny::tags$table(
    class = tableCl,
    tableHead,
    tableBody
  )
  
  # card wrapper or not
  if (cardWrap) {
    shiny::column(
      width = width,
      shiny::tags$div(
        class = "card",
        shiny::tags$div(
          class = "card-body",
          tableTag
        )
      )
    )
  } else {
    tableTag
  }
}




#' Boostrap 4 table item row
#'
#' Build an bs4 table item row
#'
#' @param ... Slot for \link{tableItem}.
#'
#' @rdname table
#'
#' @export
bs4TableItems <- function(...) {
  shiny::tags$tr(...)
}



#' Bootstrap 4 table item
#'
#' Build an bs4 table item
#'
#' @param ... Any HTML element.
#' @param dataCell Whether the cell should be contain data or text. <td> by default.
#'
#' @rdname table
#'
#' @export
bs4TableItem <- function(..., dataCell = FALSE) {
  if (dataCell) {
    shiny::tags$td(...)
  } else {
    shiny::tags$th(...)
  }
}




# #' @title AdminLTE3 todo list container
# #'
# #' @description Create a todo list container
# #'
# #' @param ... slot for todoListItem.
# #' @param sortable Whether the list elements are sortable or not.
# #'
# #' @author David Granjon, \email{dgranjon@@ymail.com}
# #'
# #' @examples
# #' if (interactive()) {
# #'  library(shiny)
# #'  library(bs4Dash)
# #'  shinyApp(
# #'   ui = dashboardPage(
# #'     dashboardHeader(),
# #'     dashboardSidebar(),
# #'     dashboardBody(
# #'      box(
# #'       "Sortable todo list demo",
# #'       status = "warning",
# #'       todoList(
# #'         todoListItem(
# #'           label = "Design a nice theme",
# #'           "Some text here"
# #'         ),
# #'         todoListItem(
# #'           label = "Make the theme responsive",
# #'           "Some text here"
# #'         ),
# #'         todoListItem(
# #'           checked = TRUE,
# #'           label = "Let theme shine like a star"
# #'         )
# #'        )
# #'       ),
# #'       box(
# #'       "Simple todo list demo",
# #'       status = "warning",
# #'       todoList(
# #'       sortable = FALSE,
# #'         todoListItem(
# #'           label = "Design a nice theme",
# #'           "Some text here"
# #'         ),
# #'         todoListItem(
# #'           label = "Make the theme responsive",
# #'           "Some text here"
# #'         ),
# #'         todoListItem(
# #'           checked = TRUE,
# #'           label = "Let theme shine like a star"
# #'         )
# #'        )
# #'       )
# #'     ),
# #'     title = "Todo Lists"
# #'   ),
# #'   server = function(input, output) { }
# #'  )
# #' }
# #'
# #' @export
# todoList <- function(..., sortable = TRUE) {
#   
#   items <- list(...)
#   
#   if (sortable) {
#     for (i in seq_along(items)) {
#       items[[i]]$children[[1]]$attribs$class <- paste(items[[i]]$children[[1]]$attribs$class, "ui-sortable-handle")
#     }
#   }
#   
#   todoListTag <- shiny::tags$ul(
#     class = if (sortable) "todo-list ui-sortable" else "todo-list",
#     `data-widget` = "todo-list",
#     items
#   )
#   
#   todoListTag
#   
# }
# 
# 
# 
# #' @title AdminLTE2 todo list item
# #'
# #' @description Create a todo list item
# #'
# #' @param ... any element such as labels, ...
# #' @param checked Whether the list item is checked or not.
# #' @param label item label.
# #'
# #' @author David Granjon, \email{dgranjon@@ymail.com}
# #'
# #' @export
# todoListItem <- function(..., checked = FALSE, label = NULL) {
#   cl <- NULL
#   if (checked) cl <- "done"
#   
#   shiny::tags$li(
#     class = cl,
#     
#     # sortable icon
#     shiny::tags$span(
#       class = "handle",
#       shiny::tags$i(class = "fa fa-ellipsis-v"),
#       shiny::tags$i(class = "fa fa-ellipsis-v")
#     ),
#     
#     # checkbox trigger
#     # need to be implemented (custom binding js)
#     #shiny::tags$input(type = "checkbox"),
#     
#     # label
#     shiny::tags$span(class = "text", label),
#     
#     # any element
#     shiny::tags$small(
#       ...
#     )
#   )
#   
# }#




#' Boostrap 4 ribbon
#'
#' \link{bs4Ribbon} build a bootstrap 4 ribbon
#'
#' @param text Ribbon text.
#' @param color Ribbon color. Valid colors are defined as follows:
#' \itemize{
#'   \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item \code{gray-dark}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item \code{gray}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item \code{white}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#1f2d3d")}.
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
#'      fluidRow(
#'       box(
#'        width = 4,
#'        title = "Blue ribbon",
#'        bs4Ribbon(
#'         text = "New",
#'         color = "primary"
#'        )
#'       ),
#'       box(
#'        width = 4,
#'        title = "Purple ribbon",
#'        bs4Ribbon(
#'         text = "New",
#'         color = "indigo"
#'        )
#'       ),
#'       box(
#'        width = 4,
#'        title = "Orange ribbon",
#'        bs4Ribbon(
#'         text = "New",
#'         color = "orange"
#'        )
#'       )
#'      )
#'     ), 
#'     footer = dashboardFooter()
#'   ),
#'   server = function(input, output) { }
#'  )
#' }
#' 
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' @rdname ribbon
#'
#' @export
bs4Ribbon <- function(text, color) {
  validateStatusPlus(color)
  ribbonCl <- paste0("ribbon bg-", color) 
  ribbonWrapperCl <- "ribbon-wrapper"
  shiny::tags$div(
    class = ribbonWrapperCl,
    shiny::tags$div(class = ribbonCl, text)
  )
}




#' Boostrap 4 block quote
#'
#' Build a bootstrap 4 block quote
#'
#' @param ... Content.
#' @param color Block color.  Valid colors are defined as follows:
#' \itemize{
#'   \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item \code{gray-dark}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item \code{gray}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item \code{white}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#1f2d3d")}.
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
#' @rdname quote
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
#'      fluidRow(
#'       blockQuote("Blablabla", color = "indigo"),
#'       blockQuote("Blablabla", color = "danger"),
#'       blockQuote("Blablabla", color = "teal"),
#'       blockQuote("Blablabla", color = "orange"),
#'       blockQuote("Blablabla", color = "warning"),
#'       blockQuote("Blablabla", color = "fuchsia")
#'      )
#'     ), 
#'     footer = dashboardFooter()
#'   ),
#'   server = function(input, output) { }
#'  )
#' }
#' 
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4Quote <- function(..., color) {
  validateStatusPlus(color)
  shiny::tags$blockquote(
    class = paste0("quote-", color),
    ...
  )
}



#' Get all AdminLTE colors.
#' @export
getAdminLTEColors <- function() {
  c(validStatuses, validNuances, validColors)
}