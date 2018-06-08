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
#' @param status Item color. "primary", "success", "warning", "danger". NULL by default.
#' @param width The width of the accordion.
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashAccordionItem <- function(..., id, title = NULL, status = NULL, width = 6) {
  
  
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