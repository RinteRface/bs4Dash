#' Create a Bootstrap 4 dashboard badge item
#' 
#' Beautiful badge from AdminLTE3 
#'
#' @param ... Badge content.
#' @param position Badge position: "left" or "right".
#' @param status Bdage color. "primary", "danger", "info", "success" or "warning"
#' 
#' @examples 
#' if(interactive()){
#'  library(shiny)
#'  
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'      navbar = bs4DashNavbar(),
#'      sidebar = bs4DashSidebar(),
#'      controlbar = bs4DashControlbar(),
#'      footer = bs4DashFooter(),
#'      title = "test",
#'      body = bs4DashBody(
#'       bs4DashBadge(
#'        position = "right",
#'        status = "warning",
#'        "Warning"
#'       )
#'      )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashBadge <- function(..., position = c("left", "right"), status) {
  
  position <- match.arg(position)
  
  shiny::tags$span(
    class = paste0(position, " badge", " badge-", status),
    ...
  )
}




#' Create a Bootstrap 4 accordion
#' 
#' Beautiful accordion from AdminLTE3 
#'
#' @param ... Slot for bs4DashAccordionItem.
#' 
#' @examples 
#' if(interactive()){
#'  library(shiny)
#'  
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'      navbar = bs4DashNavbar(),
#'      sidebar = bs4DashSidebar(),
#'      controlbar = bs4DashControlbar(),
#'      footer = bs4DashFooter(),
#'      title = "test",
#'      body = bs4DashBody(
#'       bs4DashAccordion(
#'        bs4DashAccordionItem(
#'         id = "item1",
#'         title = "Item 1", 
#'         status = "danger",
#'         "Anim pariatur cliche reprehenderit, enim 
#'         eiusmod high life accusamus terry richardson ad 
#'         squid. 3 wolf moon officia aute, non cupidatat 
#'         skateboard dolor brunch. Food truck quinoa nesciunt 
#'         laborum eiusmod. Brunch 3 wolf moon tempor, sunt 
#'         aliqua put a bird on it squid single-origin coffee 
#'         nulla assumenda shoreditch et. Nihil anim keffiyeh 
#'         helvetica, craft beer labore wes anderson cred 
#'         nesciunt sapiente ea proident. Ad vegan excepteur 
#'         butcher vice lomo. Leggings occaecat craft beer farm-to-table, 
#'         raw denim aesthetic synth nesciunt you probably haven't 
#'         heard of them accusamus labore sustainable VHS"
#'        ),
#'        bs4DashAccordionItem(
#'         id = "item2",
#'         title = "Item 2", 
#'         status = "warning",
#'         "Anim pariatur cliche reprehenderit, enim 
#'         eiusmod high life accusamus terry richardson ad 
#'         squid. 3 wolf moon officia aute, non cupidatat 
#'         skateboard dolor brunch. Food truck quinoa nesciunt 
#'         laborum eiusmod. Brunch 3 wolf moon tempor, sunt 
#'         aliqua put a bird on it squid single-origin coffee 
#'         nulla assumenda shoreditch et. Nihil anim keffiyeh 
#'         helvetica, craft beer labore wes anderson cred 
#'         nesciunt sapiente ea proident. Ad vegan excepteur 
#'         butcher vice lomo. Leggings occaecat craft beer farm-to-table, 
#'         raw denim aesthetic synth nesciunt you probably haven't 
#'         heard of them accusamus labore sustainable VHS"
#'        )
#'       )
#'      )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashAccordion <- function(...) {
  shiny::tags$div(
    class = "accordion",
    ...
  )
}


#' Create a Bootstrap 4 accordion item
#' 
#' To insert in a bs4DashAccordion
#'
#' @param ... Item content.
#' @param id Item unique id.
#' @param title Item title.
#' @param status Item color. "primary", "success", "warning", "danger" or "info". NULL by default.
#' @param width The width of the accordion.
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashAccordionItem <- function(..., id, title = NULL, status = NULL, width = 12) {
  
  
  accordionItemCl <- "card"
  if (!is.null(status)) accordionItemCl <- paste0(accordionItemCl, " card-", status)
  
  # header
  headerTag <- shiny::tags$div(
    class = "card-header",
    shiny::tags$h4(
      class = "card-title",
      shiny::tags$a(
        href = paste0("#", id),
        `data-toggle` = "collapse",
        `data-parent` = "accordion",
        title
      )
    )
  )
  
  # body
  bodyTag <- shiny::tags$div(
    class = "panel-collapse collapse in",
    id = id,
    shiny::tags$div(
      class = "card-body",
      ...
    )
  )
  
  accordionItemTag <- shiny::tags$div(class = accordionItemCl)
  accordionItemTag <- shiny::tagAppendChildren(accordionItemTag, headerTag, bodyTag)
  
  shiny::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    accordionItemTag
  )
}



#' Create a Bootstrap 4 carousel
#' 
#' Beautiful carousel from AdminLTE3 
#'
#' @param ... Slot for bs4DashCarouselItem.
#' @param id Unique carousel id.
#' @param width Carousel width. Between 1 and 12.
#' 
#' @examples 
#' if(interactive()){
#'  library(shiny)
#'  
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'      navbar = bs4DashNavbar(),
#'      sidebar = bs4DashSidebar(),
#'      controlbar = bs4DashControlbar(),
#'      footer = bs4DashFooter(),
#'      title = "test",
#'      body = bs4DashBody(
#'       title = "Carousel",
#'       bs4DashCarousel(
#'        id = "mycarousel",
#'        width = 6,
#'        bs4DashCarouselItem(
#'         active = TRUE,
#'         src = "http://placehold.it/900x500/39CCCC/ffffff&text=I+Love+Bootstrap"
#'        ),
#'        bs4DashCarouselItem(
#'         active = FALSE,
#'         src = "http://placehold.it/900x500/3c8dbc/ffffff&text=I+Love+Bootstrap"
#'        ),
#'        bs4DashCarouselItem(
#'         active = FALSE,
#'         src = "http://placehold.it/900x500/f39c12/ffffff&text=I+Love+Bootstrap"
#'        )
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
bs4DashCarousel <- function(..., id, width = 12) {
  
  items <- list(...)
  
  generateCarouselNav <- function(items) {
    lapply(1:length(items), FUN = function(i) {
      active <- sum(grep(x = items[[i]]$attribs$class, pattern = "active")) == 1
      
      shiny::tags$li(
        `data-target` = paste0("#",id),
        `data-slide-to` = i - 1,
        class = if (isTRUE(active)) "active" else NULL
      )
    })
  }
  
  indicatorsTag <- shiny::tags$ol(
    class = "carousel-indicators",
    generateCarouselNav(items)
  )
  
  bodyTag <- shiny::tags$div(
    class = "carousel-inner",
    ...
  )
  
  controlButtons <- shiny::tagList(
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



#' Create a Bootstrap 4 carousel item
#' 
#' To insert in a bs4DashCarousel
#'
#' @param active Whether the item is active or not at start.
#' @param src Item path or url.
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashCarouselItem <- function(active = FALSE, src = NULL) {
  shiny::tags$div(
    class = if (isTRUE(active)) "carousel-item active" else "carousel-item",
    shiny::tags$img(
      class = "d-block w-100", 
      src = src
    )
  )
}




#' Create a Bootstrap 4 progress bar
#' 
#' AdminLTE3 progress bar
#'
#' @param value Progress bar value
#' @param min Progress bar minimum value.
#' @param max Progress bar maximum value.
#' @param striped Whether the progress bar is striped or not. FALSE by default.
#' @param vertical Whether to display the progress bar in vertical mode. FALSE by default.
#' @param status Progress bar status. "primary", "success", "warning", "danger" or "info".
#' @param width Progress bar width (only if vertical is FALSE).
#' @param height Progress bar height (only if vertical is TRUE).
#' 
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'      navbar = bs4DashNavbar(),
#'      sidebar = bs4DashSidebar(),
#'      controlbar = bs4DashControlbar(),
#'      footer = bs4DashFooter(),
#'      title = "test",
#'      body = bs4DashBody(
#'        title = "Carousel",
#'        bs4Card(
#'         title = "Progress bars",
#'         footer = tagList(
#'           bs4ProgressBar(
#'           value = 5,
#'           striped = FALSE,
#'           status = "info"
#'          ),
#'          bs4ProgressBar(
#'           value = 5,
#'           striped = TRUE,
#'           status = "warning",
#'           width = "20%"
#'          )
#'         ),
#'         bs4ProgressBar(
#'          value = 80,
#'          vertical = TRUE,
#'          status = "success"
#'         ),
#'         bs4ProgressBar(
#'          value = 100,
#'          vertical = TRUE,
#'          striped = TRUE,
#'          status = "danger",
#'          height = "80%"
#'         )
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
bs4ProgressBar <- function(value, min = 0, max = 100, vertical = FALSE, striped = FALSE,
                           status = c("primary", "warning", "danger", "info", "success"),
                           width = "80%", height = "40%") {
  
  status <- match.arg(status)
  stopifnot(value >= min)
  stopifnot(value <= max)
  
  progressCl <- if (isTRUE(vertical)) "progress vertical" else "progress mb-3"
  barCl <- "progress-bar"
  if (!is.null(status)) barCl <- paste0(barCl, " bg-", status)
  if (isTRUE(striped)) barCl <- paste0(barCl, " progress-bar-striped")
  
  barTag <- shiny::tags$div(
    class = barCl,
    role = "progressbar",
    `aria-valuenow` = value,
    `aria-valuemin` = min,
    `aria-valuemax` = max,
    style = if (vertical) {
      paste0("height: ", height)
    } else {
      paste0("width: ", height)
    },
    shiny::tags$span(class = "sr-only", paste0(value, "%"))
  )
  
  progressTag <- shiny::tags$div(class = progressCl)
  progressTag <- shiny::tagAppendChild(progressTag, barTag)
  progressTag
  
}





#' Create a Bootstrap 4 alert
#' 
#' AdminLTE3 alert
#'
#' @param ... Alert content.
#' @param title Alert title.
#' @param closable Whether to allow the user to close the alert. FALSE by default.
#' @param width Alert width. Between 1 and 12.
#' @param elevation Alert elevation.
#' @param status Alert status. "primary", "success", "warning", "danger" or "info".
#' 
#' @examples
#' if(interactive()){
#'  library(shiny)
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
#'        bs4DashAlert(
#'         title = "Be Careful!",
#'         status = "danger",
#'         closable = FALSE,
#'         "Danger alert preview. This alert is not dismissable. 
#'         A wonderful serenity has taken possession of my entire soul, 
#'         like these sweet mornings of spring which 
#'         I enjoy with my whole heart."
#'        ),
#'        bs4DashAlert(
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
bs4DashAlert <- function(..., title, closable = TRUE, width = 6, elevation = NULL,
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
  
  alertCl <- "alert"
  if (!is.null(status)) alertCl <- paste0(alertCl, " alert-", status)
  if (isTRUE(closable)) alertCl <- paste0(alertCl, " alert-dismissible")
  if (!is.null(elevation)) alertCl <- paste0(alertCl, " elevation-", elevation)
  
  alertTag <- shiny::tags$div(
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



#' Create a Bootstrap 4 callout
#' 
#' AdminLTE3 calout
#'
#' @param ... Callout content.
#' @param title Callout title.
#' @param width Callout width. Between 1 and 12.
#' @param elevation Callout elevation.
#' @param status Callout status. "primary", "success", "warning", "danger" or "info".
#' 
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'      navbar = bs4DashNavbar(),
#'      sidebar = bs4DashSidebar(),
#'      controlbar = bs4DashControlbar(),
#'      footer = bs4DashFooter(),
#'      title = "test",
#'      body = bs4DashBody(
#'        title = "Callouts",
#'        bs4DashCallout(
#'         title = "I am a danger callout!",
#'         elevation = 4,
#'         status = "danger",
#'         "There is a problem that we need to fix. 
#'         A wonderful serenity has taken possession of 
#'         my entire soul, like these sweet mornings of 
#'         spring which I enjoy with my whole heart."
#'        ),
#'        bs4DashCallout(
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
bs4DashCallout <- function(..., title, width = 6, elevation = NULL,
                           status = c("primary", "warning", "danger", "info", "success")) {
  
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
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'      navbar = bs4DashNavbar(),
#'      sidebar = bs4DashSidebar(),
#'      controlbar = bs4DashControlbar(),
#'      footer = bs4DashFooter(),
#'      title = "test",
#'      body = bs4DashBody(
#'        title = "Callouts",
#'        bs4Card(
#'         title = "Loading State",
#'         bs4DashLoading()
#'        )
#'      )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#'
#' @export
bs4DashLoading <- function() {
  shiny::tags$div(
    class = "overlay",
    shiny::tags$i(class = "fa fa-refresh fa-spin")
  )
}




#' @title AdminLTE3 timeline block
#'
#' @description Create a timeline block
#'
#' @param ... slot for bs4TimelineLabel or bs4TimelineItem.
#' @param reversed Whether the timeline is reversed or not.
#' @param width Timeline width. Between 1 and 12.
#' 
#' @note reversed is useful when the user wants to use the timeline
#' inside a box.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'     navbar = bs4DashNavbar(),
#'     sidebar = bs4DashSidebar(),
#'     controlbar = bs4DashControlbar(),
#'     footer = bs4DashFooter(),
#'     title = "test",
#'     body = bs4DashBody(
#'      bs4Card(
#'       title = "Timeline",
#'       bs4Timeline(
#'        width = 12,
#'        reversed = TRUE,
#'        bs4TimelineEnd(status = "danger"),
#'        bs4TimelineLabel("10 Feb. 2014", status = "info"),
#'        bs4TimelineItem(
#'         elevation = 4, 
#'         title = "Item 1",
#'         icon = "gears",
#'         status = "success",
#'         time = "now",
#'         footer = "Here is the footer",
#'         "This is the body"
#'        ),
#'        bs4TimelineItem(
#'         title = "Item 2",
#'         border = FALSE
#'        ),
#'        bs4TimelineLabel("3 Jan. 2014", status = "primary"),
#'        bs4TimelineItem(
#'         elevation = 2,
#'         title = "Item 3",
#'         icon = "paint-brush",
#'         status = "warning",
#'         bs4TimelineItemMedia(src = "http://placehold.it/150x100"),
#'         bs4TimelineItemMedia(src = "http://placehold.it/150x100")
#'        ),
#'        bs4TimelineStart(status = "danger")
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
  
  timelineTag <- shiny::tags$ul(
    class = cl,
    ...
  )
  
  shiny::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    timelineTag
  )
  
}


#' @title AdminLTE3 timeline label
#'
#' @description Create a timeline label
#'
#' @param ... any element.
#' @param status label status: see here for a list of valid colors \url{https://adminlte.io/themes/AdminLTE/pages/UI/general.html}.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @export
bs4TimelineLabel <- function(..., status = NULL) {
  
  cl <- "bg-"
  if (!is.null(status)) cl <- paste0(cl, status)
  
  shiny::tags$li(
    class = "time-label",
    shiny::tags$span(
      class = cl,
      ...
    )
  )
}


#' @title AdminLTE3 timeline item
#'
#' @description Create a timeline item
#'
#' @param ... any element such as bs4TimeLineItemMedia ...
#' @param elevation Timeline elevation (numeric). NULL by default.
#' @param icon item icon such as "clock-o", "times", ...
#' @param status item status: see here for a list of valid colors \url{https://adminlte.io/themes/AdminLTE/pages/UI/general.html}.
#' @param time item date or time.
#' @param title item title.
#' @param border Whether to display a border between the header and the body. TRUE by default.
#' @param footer item footer if any.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @export
bs4TimelineItem <- function(..., elevation = NULL, icon = NULL, 
                            status = NULL, time = NULL, title = NULL, 
                            border = TRUE, footer = NULL) {
  
  cl <- "fa fa-"
  if (!is.null(icon)) cl <- paste0(cl, icon)
  if (!is.null(status)) cl <- paste0(cl, " bg-", status)

  itemCl <- "timeline-header no-border"
  if (isTRUE(border)) itemCl <- "timeline-header"
  
  shiny::tags$li(
    
    # timelineItem icon and status
    shiny::tags$i(
      class = if (!is.null(elevation)) {
        paste0(cl, " elevation-", elevation)
      } else {
        cl
      }
    ),
    
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


#' @title AdminLTE2 timeline media item
#'
#' @description Create a timeline media item
#'
#' @param src media url or path.
#' @param height media height in pixels.
#' @param width media width in pixels.
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @export
bs4TimelineItemMedia <- function(src = NULL, height = NULL, width = NULL) {
  shiny::img(
    class = "margin", 
    src = src, 
    height = height,
    width = width
  )
}




#' @title AdminLTE3 timeline starting point
#'
#' @description Create a timeline starting point
#'
#' @param icon item icon such as "clock-o", "times", ...
#' @param status item status: see here for a list of valid colors \url{https://adminlte.io/themes/AdminLTE/pages/UI/general.html}.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @export
bs4TimelineStart <- function(icon = "clock-o", status = NULL) {
  
  cl <- "fa fa-"
  if (!is.null(icon)) cl <- paste0(cl, icon)
  if (!is.null(status)) cl <- paste0(cl, " bg-", status)
  
  shiny::tags$li(
    shiny::tags$i(class = cl)
  )
}


#' @title AdminLTE3 timeline ending point
#'
#' @description Create a timeline ending point
#'
#' @param icon item icon such as "clock-o", "times", ...
#' @param status item status: see here for a list of valid colors \url{https://adminlte.io/themes/AdminLTE/pages/UI/general.html}.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @export
bs4TimelineEnd <- function(icon = "hourglass-end", status = NULL) {
  
  cl <- "fa fa-"
  if (!is.null(icon)) cl <- paste0(cl, icon)
  if (!is.null(status)) cl <- paste0(cl, " bg-", status)
  
  shiny::tagList(
    shiny::tags$li(
      shiny::tags$i(class = cl)
    ),
    shiny::br(), 
    shiny::br()
  )
}