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
#' @param ... Slot for bs4AccordionItem.
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
#' @param ... Slot for bs4CarouselItem.
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
#' To insert in a bs4Carousel
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
#' @param ... Slot for bs4ListGroupItem.
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