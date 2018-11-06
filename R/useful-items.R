#' Create a Bootstrap 4 dashboard badge item
#' 
#' Beautiful badge from AdminLTE3 
#'
#' @param ... Badge content.
#' @param position Badge position: "left" or "right".
#' @param status Bdage color. "primary", "danger", "info", "success", "warning",
#' "secondary", "dark" or "light".
#' @param rounded Whether the badge is rounded instead of square. FALSE by default.
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
#'       bs4Badge(
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
bs4Badge <- function(..., position = c("left", "right"), status,
                         rounded = FALSE) {
  
  position <- match.arg(position)
  
  shiny::tags$span(
    class = paste0(position, " badge", " badge-", status, if (rounded) " badge-pill"),
    ...
  )
}




#' Create a Bootstrap 4 accordion
#' 
#' Beautiful accordion from AdminLTE3 
#'
#' @param ... Slot for \link{bs4AccordionItem}.
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
#'       bs4Accordion(
#'        bs4AccordionItem(
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
#'        bs4AccordionItem(
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
bs4Accordion <- function(...) {
  shiny::tags$div(
    class = "accordion",
    ...
  )
}


#' Create a Bootstrap 4 accordion item
#' 
#' To insert in a bs4Accordion
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
bs4AccordionItem <- function(..., id, title = NULL, status = NULL, width = 12) {
  
  
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
#' @param ... Slot for \link{bs4CarouselItem}.
#' @param id Unique carousel id.
#' @param width Carousel width. Between 1 and 12.
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
#'       title = "Carousel",
#'       bs4Carousel(
#'        id = "mycarousel",
#'        width = 6,
#'        bs4CarouselItem(
#'         active = TRUE,
#'         src = "http://placehold.it/900x500/39CCCC/ffffff&text=I+Love+Bootstrap"
#'        ),
#'        bs4CarouselItem(
#'         active = FALSE,
#'         src = "http://placehold.it/900x500/3c8dbc/ffffff&text=I+Love+Bootstrap"
#'        ),
#'        bs4CarouselItem(
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
bs4Carousel <- function(..., id, width = 12) {
  
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
#' To insert in a \link{bs4Carousel}
#'
#' @param active Whether the item is active or not at start.
#' @param src Item path or url.
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4CarouselItem <- function(active = FALSE, src = NULL) {
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
#'        title = "Progress bars",
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
bs4Alert <- function(..., title, closable = TRUE, width = 6, elevation = NULL,
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
#' AdminLTE3 callout
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
#'        title = "Callouts",
#'        bs4Callout(
#'         title = "I am a danger callout!",
#'         elevation = 4,
#'         status = "danger",
#'         "There is a problem that we need to fix. 
#'         A wonderful serenity has taken possession of 
#'         my entire soul, like these sweet mornings of 
#'         spring which I enjoy with my whole heart."
#'        ),
#'        bs4Callout(
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
bs4Callout <- function(..., title, width = 6, elevation = NULL,
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
#'        title = "Callouts",
#'        bs4Card(
#'         title = "Loading State",
#'         bs4Loading()
#'        )
#'      )
#'    ),
#'    server = function(input, output) {}
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




#' @title AdminLTE3 timeline block
#'
#' @description Create a timeline block
#'
#' @param ... slot for \link{bs4TimelineLabel} or \link{bs4TimelineItem}.
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
#' @param ... Any element.
#' @param status Label status: see here for a list of valid colors \url{https://adminlte.io/themes/AdminLTE/pages/UI/general.html}.
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
#' @param ... Any element such as \link{bs4TimelineItemMedia} ...
#' @param elevation Timeline elevation (numeric). NULL by default.
#' @param icon Item icon such as "clock-o", "times", ...
#' @param status Item status: see here for a list of valid colors \url{https://adminlte.io/themes/AdminLTE/pages/UI/general.html}.
#' @param time Item date or time.
#' @param title Item title.
#' @param border Whether to display a border between the header and the body. TRUE by default.
#' @param footer Item footer if any.
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
#' @param src Media url or path.
#' @param height Media height in pixels.
#' @param width Media width in pixels.
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
#' @param icon Item icon such as "clock-o", "times", ...
#' @param status Item status: see here for a list of valid colors \url{https://adminlte.io/themes/AdminLTE/pages/UI/general.html}.
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
#' @param icon Item icon such as "clock-o", "times", ...
#' @param status Item status: see here for a list of valid colors \url{https://adminlte.io/themes/AdminLTE/pages/UI/general.html}.
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




#' @title AdminLTE3 stars
#'
#' @description Create a block of stars (ideal for rating)
#'
#' @param maxstar Maximum number of stars by block.
#' @param grade Current score. Should be positive and lower or equal to maxstar.
#' @param status Star color: see \code{validColors()} in the documentation.
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
#'       bs4Card(
#'        title = "Stars",
#'        bs4Stars(grade = 5),
#'        bs4Stars(grade = 5, status = "success"),
#'        bs4Stars(grade = 1, status = "danger"),
#'        bs4Stars(grade = 3, status = "info")
#'       )
#'      )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#'
#' @export
bs4Stars <- function(maxstar = 5, grade, status = "warning") {
  
  stopifnot(!is.null(grade))
  stopifnot(grade >= 0)
  stopifnot(grade <= maxstar)
  
  shiny::tags$td(
    class = "mailbox-star",
    shiny::tags$a(
      href = NULL,
      if (grade > 0) {
        full_star <- lapply(1:grade, FUN = function(i) {
          shiny::tags$i(class = paste0("fa text-", status, " fa-star"))
        })
      },
      if (grade < maxstar) {
        empty_star <- lapply(1:(maxstar - grade), FUN = function(i) {
          shiny::tags$i(class = paste0("fa text-", status, " fa-star-o"))
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
#' @param btn_name Jumbotron button name.
#' @param status Jumbotron background color. "primary", "success", "warning", "danger" or "info".
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
#'       bs4Jumbotron(
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
bs4Jumbotron <- function(..., title = NULL, lead = NULL, href = NULL, btn_name = "More",
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
    shiny::tags$a(
      class = paste0("btn btn-", btnStatus, " btn-lg"),
      href = href,
      target = "_blank",
      role = "button",
      btn_name
    )
  )
}



#' @title BS4 list group for AdminLTE3
#'
#' @description Create a list group
#'
#' @param ... Slot for \link{bs4ListGroupItem}.
#' @param width List group width. 4 by default. Between 1 and 12.
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
#'        bs4ListGroup(
#'        bs4ListGroupItem(
#'         type = "basic",
#'         "Cras justo odio"
#'        ),
#'        bs4ListGroupItem(
#'         type = "basic",
#'         "Dapibus ac facilisis in"
#'        ),
#'        bs4ListGroupItem(
#'         type = "basic",
#'         "Morbi leo risus"
#'        )
#'       ),
#'       bs4ListGroup(
#'        bs4ListGroupItem(
#'         "Cras justo odio",
#'         active = TRUE, 
#'         disabled = FALSE, 
#'         type = "action",
#'         src = "http://www.google.fr"
#'        ),
#'        bs4ListGroupItem(
#'         active = FALSE, 
#'         disabled = FALSE, 
#'         type = "action",
#'         "Dapibus ac facilisis in",
#'         src = "http://www.google.fr"
#'        ),
#'        bs4ListGroupItem(
#'         "Morbi leo risus",
#'         active = FALSE, 
#'         disabled = TRUE, 
#'         type = "action",
#'         src = "http://www.google.fr"
#'        )
#'       ),
#'       bs4ListGroup(
#'        bs4ListGroupItem(
#'         "Donec id elit non mi porta gravida at eget metus. 
#'         Maecenas sed diam eget risus varius blandit.",
#'         active = TRUE, 
#'         disabled = FALSE, 
#'         type = "heading",
#'         title = "List group item heading", 
#'         subtitle = "3 days ago", 
#'         footer = "Donec id elit non mi porta."
#'        ),
#'        bs4ListGroupItem(
#'         "Donec id elit non mi porta gravida at eget metus. 
#'         Maecenas sed diam eget risus varius blandit.",
#'         active = FALSE, 
#'         disabled = FALSE, 
#'         type = "heading",
#'         title = "List group item heading", 
#'         subtitle = "3 days ago", 
#'         footer = "Donec id elit non mi porta."
#'        )
#'       )
#'      )
#'     )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#'
#' @export
bs4ListGroup <- function(..., width = 4) {
  
  listGroupTag <- shiny::tags$ul(
    class = "list-group",
    ...
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
#' @param active Whether the item is active or not. FALSE by default. 
#' Only if type is "action" or "heading".
#' @param disabled Whether the item is disabled or not. FALSE by default. 
#' Only if type is "action" or "heading".
#' @param type Item type. Choose between "basic", "action" and "heading".
#' @param src Item external link.
#' @param title Item title (only if type is "heading").
#' @param subtitle Item subtitle (only if type is "heading").
#' @param footer Item footer content (only if type is "heading").
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4ListGroupItem <- function(..., active = FALSE, disabled = FALSE, 
                             type = c("basic", "action", "heading"),
                             src = "#", title = NULL, subtitle = NULL, 
                             footer = NULL) {
  
  if (isTRUE(active) && isTRUE(disabled)) {
    stop("active and disabled cannot be TRUE at the same time!")
  }
  type <- match.arg(type)
  
  itemCl <- switch(
    type,
    "basic" = "list-group-item d-flex justify-content-between align-items-center",
    "action" = "list-group-item list-group-item-action",
    "heading" = "list-group-item list-group-item-action flex-column align-items-start"
  )
  if (isTRUE(active)) itemCl <- paste0(itemCl, " active")
  if (isTRUE(disabled)) itemCl <- paste0(itemCl, " disabled")
  
  
  # item tag
  if (type == "basic") {
    shiny::tags$li(
      class = itemCl,
      ...
    )
  } else if (type == "action") {
    shiny::tags$a(
      class = itemCl,
      href = src,
      target = "_blank",
      ...
    )
  } else {
    shiny::tags$a(
      class = itemCl,
      href = src,
      target = "_blank",
      shiny::tags$div(
        class = "d-flex w-100 justify-content-between",
        shiny::tags$h5(class = "mb-1", title),
        shiny::tags$small(subtitle)
      ),
      shiny::tags$p(class = "mb-1", ...),
      shiny::tags$small(class = if (isTRUE(active)) NULL else "text-muted", footer)
    )
  }
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
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'      navbar = bs4DashNavbar(),
#'      sidebar = bs4DashSidebar(),
#'      controlbar = bs4DashControlbar(),
#'      footer = bs4DashFooter(),
#'      title = "test",
#'      body = bs4DashBody(
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



#' @title AdminLTE3 attachment container
#'
#' @description Create an attachment container, nice to wrap articles...
#'
#' @param ... Any element.
#' @param src Url or path to the image.
#' @param title Attachment title.
#' @param title_url External link.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  shinyApp(
#'   ui = bs4DashPage(
#'     bs4DashNavbar(),
#'     bs4DashSidebar(),
#'     bs4DashControlbar(),
#'     bs4DashBody(
#'      bs4Card(
#'       title = "Attachment example",
#'       attachmentBlock(
#'        src = "http://kiev.carpediem.cd/data/afisha/o/2d/c7/2dc7670333.jpg",
#'        title = "Test",
#'        title_url = "http://google.com",
#'        "This is the content"
#'       )
#'      )
#'     ),
#'     title = "AttachmentBlock"
#'   ),
#'   server = function(input, output) { }
#'  )
#' }
#'
#' @export
attachmentBlock <- function(..., src = NULL, title = NULL, title_url = NULL) {
  shiny::tags$div(
    class = "attachment-block clearfix",
    shiny::img(
      class = "attachment-img",
      src = src
    ),
    shiny::tags$div(
      class = "attachment-pushed",
      shiny::tags$h4(
        class = "attachment-heading",
        shiny::tags$a(
          href = title_url,
          target = "_blank",
          title
        )
      ),
      shiny::tags$div(
        class = "attachment-text",
        ...
      )
    )
  )
}



#' @title AdminLTE3 description block
#'
#' @description Create a description block, perfect for writing statistics
#'
#' @param number Any number.
#' @param number_color Number color: see here for a list of valid colors \url{https://adminlte.io/themes/AdminLTE/pages/UI/general.html}.
#' @param number_icon Number icon, if any. Should be written like "fa fa-times".
#' @param header Bold text.
#' @param text Additional text.
#' @param right_border TRUE by default. Whether to display a right border to
#'   separate two blocks. The last block on the right should not have a right border.
#' @param margin_bottom FALSE by default. Set it to TRUE when the
#'   descriptionBlock is used in a \link{cardPad} context.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  shinyApp(
#'   ui = bs4DashPage(
#'     bs4DashNavbar(),
#'     bs4DashSidebar(),
#'     bs4DashControlbar(),
#'     bs4DashBody(
#'      bs4Card(
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
#'             number_color = "success", 
#'             number_icon = "fa fa-caret-up",
#'             header = "$35,210.43", 
#'             text = "TOTAL REVENUE", 
#'             right_border = TRUE,
#'             margin_bottom = FALSE
#'           )
#'         ),
#'         column(
#'           width = 6,
#'           descriptionBlock(
#'             number = "18%", 
#'             number_color = "danger", 
#'             number_icon = "fa fa-caret-down",
#'             header = "1200", 
#'             text = "GOAL COMPLETION", 
#'             right_border = FALSE,
#'             margin_bottom = FALSE
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
descriptionBlock <- function(number = NULL, number_color = NULL, number_icon = NULL,
                             header = NULL, text = NULL, right_border = TRUE,
                             margin_bottom = FALSE) {
  
  cl <- "description-block"
  if (isTRUE(right_border)) cl <- paste0(cl, " border-right")
  if (isTRUE(margin_bottom)) cl <- paste0(cl, " mb-4")
  
  numcl <- "description-percentage"
  if (!is.null(number_color)) numcl <- paste0(numcl, " text-", number_color)
  
  shiny::tags$div(
    class = cl,
    shiny::tags$span(
      class = numcl, 
      number,
      if (!is.null(number_icon)) shiny::tags$i(class = number_icon)
    ),
    shiny::tags$h5(class = "description-header", header),
    shiny::tags$span(class = "description-text", text)
  )
}



#' @title AdminLTE3 vertical block container
#'
#' @description Create a vertical container for \link{descriptionBlock} or other elements
#'
#' @param ... Any element such as \link{descriptionBlock}.
#' @param color Background color: see here for a list of valid colors \url{https://adminlte.io/themes/AdminLTE/pages/UI/general.html}.
#' @param style Custom CSS, if any.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  shinyApp(
#'   ui = bs4DashPage(
#'     bs4DashNavbar(),
#'     bs4DashSidebar(),
#'     bs4DashControlbar(),
#'     bs4DashBody(
#'      bs4Card(
#'       title = "Box with right pad",
#'       status = "warning",
#'       fluidRow(
#'         column(width = 6),
#'         column(
#'           width = 6,
#'           cardPad(
#'             color = "info",
#'             descriptionBlock(
#'               header = "8390", 
#'               text = "VISITS", 
#'               right_border = FALSE,
#'               margin_bottom = TRUE
#'             ),
#'             descriptionBlock(
#'               header = "30%", 
#'               text = "REFERRALS", 
#'               right_border = FALSE,
#'               margin_bottom = TRUE
#'             ),
#'             descriptionBlock(
#'               header = "70%", 
#'               text = "ORGANIC", 
#'               right_border = FALSE,
#'               margin_bottom = FALSE
#'             )
#'           )
#'         )
#'       )
#'      )
#'     ),
#'     title = "cardPad"
#'   ),
#'   server = function(input, output) { }
#'  )
#' }
#'
#' @export
cardPad <- function(..., color = NULL, style = NULL) {
  cl <- "card-pane-right pt-2 pb-2 pl-4 pr-4"
  if (!is.null(color)) cl <- paste0(cl, " bg-", color)
  
  shiny::tags$div(
    class = cl,
    style = style,
    ...
  )
}



#' @title AdminLTE3 user message container
#'
#' @description Create a user message container
#'
#' @param ... Slot for \link{userMessage}.
#' @param status Messages status. See here for a list of valid colors 
#' \url{https://adminlte.io/themes/AdminLTE/pages/UI/general.html}.
#' @param width Container width: between 1 and 12.
#' 
#' @note Better to include in a \link{bs4Card}.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  shinyApp(
#'   ui = bs4DashPage(
#'     bs4DashNavbar(),
#'     bs4DashSidebar(),
#'     bs4DashControlbar(),
#'     bs4DashBody(
#'      userMessages(
#'       status = "success",
#'       userMessage(
#'        author = "Alexander Pierce",
#'        date = "20 Jan 2:00 pm",
#'        src = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
#'        side = NULL,
#'        "Is this template really for free? That's unbelievable!"
#'       )
#'      )
#'     ),
#'     title = "user Message"
#'   ),
#'   server = function(input, output) { }
#'  )
#' }
#'
#' @export
userMessages <- function(..., status, width = 4) {
  cl <- "direct-chat-messages direct-chat"
  if (!is.null(status)) cl <- paste0(cl, " direct-chat-", status)
  msgtag <- shiny::tags$div(class = cl, ...)
  
  shiny::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    msgtag
  )
  
}

#' @title AdminLTE3 user message 
#'
#' @description Create a user message
#'
#' @param ... Message text.
#' @param author Message author.
#' @param date Message date.
#' @param src Message author image path or url.
#' @param side Side where author is displayed: NULL (left) or "right".
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
userMessage <- function(..., author = NULL, date = NULL, 
                        src = NULL, side = c(NULL, "right")) {
  
  side <- match.arg(side)
  
  messageCl <- "direct-chat-msg"
  if (!is.null(side)) messageCl <- paste0(messageCl, " right")
  
  # message info
  messageInfo <- shiny::tags$div(
    class = "direct-chat-info clearfix",
    shiny::tags$span(class = "direct-chat-name float-left", author),
    shiny::tags$span(class = "direct-chat-timestamp float-right", date)
  )
  
  # message Text
  messageTxt <- shiny::tags$div(class = "direct-chat-text", ...)
  
  # message author image
  messageImg <- shiny::tags$img(class = "direct-chat-img", src = src)
  
  shiny::tags$div(
    class = messageCl,
    messageInfo,
    messageImg, 
    messageTxt
  )
}



#' @title AdminLTE3 user post
#'
#' @description Create a user post
#'
#' @param ... Post content, slot for \link{userPostTagItems}, \link{userPostMedia}.
#' @param id Unique id of the post.
#' @param src Profile image, if any.
#' @param author Post author.
#' @param description Post description.
#' @param collapsible If TRUE, display a button in the upper right that allows the user to collapse the comment. 
#' @param collapsed Whether the comment is collapsed when the application starts, FALSE by default.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  shinyApp(
#'   ui = bs4DashPage(
#'     bs4DashNavbar(),
#'     bs4DashSidebar(),
#'     bs4DashControlbar(),
#'     bs4DashBody(
#'      bs4Card(
#'       title = "Box with user comment",
#'       status = "primary",
#'       userPost(
#'        id = 1,
#'        src = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
#'        author = "Jonathan Burke Jr.",
#'        description = "Shared publicly - 7:30 PM today",
#'        "Lorem ipsum represents a long-held tradition for designers, 
#'        typographers and the like. Some people hate it and argue for 
#'        its demise, but others ignore the hate as they create awesome 
#'        tools to help create filler text for everyone from bacon 
#'        lovers to Charlie Sheen fans.",
#'        userPostTagItems(
#'         userPostTagItem(bs4Badge("item 1", status = "warning")),
#'         userPostTagItem(bs4Badge("item 2", status = "danger"))
#'        )
#'       ),
#'       userPost(
#'        id = 2,
#'        src = "https://adminlte.io/themes/AdminLTE/dist/img/user6-128x128.jpg",
#'        author = "Adam Jones",
#'        description = "Shared publicly - 5 days ago",
#'        userPostMedia(src = "https://adminlte.io/themes/AdminLTE/dist/img/photo2.png"),
#'        userPostTagItems(
#'         userPostTagItem(bs4Badge("item 1", status = "info")),
#'         userPostTagItem(bs4Badge("item 2", status = "danger"))
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
userPost <- function(..., id = NULL, src = NULL, author = NULL, 
                     description = NULL, collapsible = TRUE, 
                     collapsed = FALSE) {
  
  cl <- "collapse"
  if (!isTRUE(collapsed)) cl <- paste0(cl, " in")
  if (collapsed) collapsed <- "false" else collapsed <- "true"
  
  shiny::tags$div(
    class = "post",
    
    shiny::tags$div(
      class = "user-block",
      shiny::img(class = "img-circle img-bordered-sm", src = src),
      shiny::tags$span(
        class = "username", 
        author,
        
        # box tool
        if (collapsible) {
          shiny::tags$button(
            class = "float-right btn-box-tool",
            `data-toggle` = "collapse",
            `data-target` = paste0("#", id),
            `aria-expanded` = collapsed,
            type = "button",
            shiny::tags$i(class = "fa fa-eye")
          )
        }
        
      ),
      shiny::tags$span(class = "description", description)
    ),
    shiny::tags$p(
      class = cl,
      id = id,
      `aria-expanded` = collapsed,
      ...
    )
  )
  
}




#' @title AdminLTE3 user post tool item container
#'
#' @description Create a user post tool item container
#'
#' @param ... Slot for \link{userPostTagItem}.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @export
userPostTagItems<- function(...) {
  
  shiny::tags$ul(
    class = "list-inline d-flex",
    ...
  )
}




#' @title AdminLTE3 user post tool item
#'
#' @description Create a user post tool item
#'
#' @param ... Tool content such as label, button, ...
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @export
userPostTagItem <- function(...) {
  
  shiny::tags$li(
    class = "mx-2",
    ...
  )
}



#' @title AdminLTE2 user post media
#'
#' @description Create a user post media (image)
#'
#' @param src Image path or url ...
#' @param height Media height in pixels.
#' @param width Media width in pixels.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @export
userPostMedia <- function(src = NULL, height = NULL, width = NULL) {
  shiny::img(
    style = "margin: auto;",
    class = "img-fluid", 
    src = src,
    height = height,
    width = width
  )
}



#' @title BS4 sortable section
#'
#' @description Create a sortable UI section
#'
#' @param ... Slot for UI elements such as \link{bs4Card}.
#' @param width Section width: between 1 and 12.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' 
#' @examples 
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  shiny::shinyApp(
#'   ui = bs4DashPage(
#'     navbar = bs4DashNavbar(),
#'     sidebar = bs4DashSidebar(),
#'     controlbar = bs4DashControlbar(),
#'     footer = bs4DashFooter(),
#'     title = "test",
#'     body = bs4DashBody(
#'       bs4TabItems(
#'         bs4TabItem(
#'           tabName = "sortabled",
#'           fluidRow(
#'             lapply(1:3, FUN = function(i) {
#'               bs4Sortable(
#'                 width = 4,
#'                 p(class = "text-center", paste("Column", i)),
#'                 lapply(1:2, FUN = function(j) {
#'                   bs4Card(
#'                     title = paste0("I am the ", j,"-th card of the ", i, "-th column"), 
#'                     width = 12,
#'                     "Click on my header"
#'                   )
#'                 })
#'               )
#'             })
#'           )
#'         )
#'       )
#'     )
#'   ),
#'   server = function(input, output) {}
#'  )
#' }  
#' 
#' @export
bs4Sortable <- function(..., width = NULL) {
  
  sectionCl <- "connectedSortable ui-sortable"
  if (!is.null(width)) sectionCl <- paste0(sectionCl, " col-lg-", width)
  
  shiny::fluidRow(
    shiny::tags$section(
      class = sectionCl,
      ...
    ) 
  )
}