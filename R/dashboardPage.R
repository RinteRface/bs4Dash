#' Create a Boostrap 4 dashboard page
#'
#' Build an adminLTE3 dashboard page
#'
#' @param navbar Slot for \link{bs4DashNavbar}.
#' @param sidebar Slot for \link{bs4DashSidebar}.
#' @param body Slot for \link{bs4DashBody}.
#' @param controlbar Slot for \link{bs4DashControlbar} (right side).
#' @param footer Slot for \link{bs4DashFooter}.
#' @param title App title.
#' @param old_school Whether to use the wonderful sketchy design for Bootstrap 4. FALSE
#' by default.
#' @param sidebar_mini Whether to see sidebar icons when \link{bs4DashSidebar}.
#' is collapsed. TRUE by default.
#' @param sidebar_collapsed Whether the sidebar is collapsed of not at start. FALSE by default.
#' @param controlbar_collapsed Whether the sidebar is collapsed of not at start. TRUE by default.
#' @param controlbar_overlay Whether the sidebar covers the content when expanded.
#' Default to TRUE.
#' @param enable_preloader Whether to enable a page loader. FALSE by default.
#' @param loading_duration Loader duration in seconds. 2s by default.
#' @param loading_background  Background color during page startup. Blue by default: 
#' \url{https://www.w3schools.com/cssref/css_colors.asp}.
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'     enable_preloader = TRUE,
#'     navbar = bs4DashNavbar(),
#'     sidebar = bs4DashSidebar(),
#'     controlbar = bs4DashControlbar(),
#'     footer = bs4DashFooter(),
#'     title = "test",
#'     body = bs4DashBody()
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashPage <- function(navbar = NULL, sidebar = NULL, body = NULL, 
                        controlbar = NULL, footer = NULL, title = NULL,
                        old_school = FALSE, sidebar_mini = TRUE, 
                        sidebar_collapsed = FALSE, controlbar_collapsed = TRUE, 
                        controlbar_overlay = TRUE, enable_preloader = FALSE, 
                        loading_duration = 2, loading_background = "#1E90FF"){
  
  sidebar_cl <- NULL
  if (sidebar_mini) sidebar_cl <- paste0(sidebar_cl, " sidebar-mini")
  
  if (!is.null(controlbar)) {
    if (!controlbar_collapsed) {
      sidebar_cl <- paste0(sidebar_cl, " control-sidebar-slide-open")
    }
    if (!controlbar_overlay) {
      sidebar_cl <- paste0(sidebar_cl, " control-sidebar-push-slide")
    } 
  }
  
  # create the body content
  bodyContent <- shiny::tags$div(
    class = "wrapper",
    style = if (enable_preloader) "visibility: hidden;" else NULL,
    navbar,
    sidebar,
    # page content
    body,
    controlbar,
    if (!is.null(footer)) footer
  )
  
  # create the preloader tag if enabled
  preloaderTag <- if (enable_preloader) {
    shiny::tags$div(
      class = "pre",
      style = "z-index: 1000;",
      shiny::HTML(
        '<svg version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 30 30" enable-background="new 0 0 30 30" xml:space="preserve" width="30" height="30">

		      <rect fill="#FBBA44" width="15" height="15">
            <animateTransform attributeName="transform" attributeType="XML" type="translate" dur="1.7s" values="0,0;15,0;15,15;0,15;0,0;" repeatCount="indefinite"/>
		      </rect>	

		      <rect x="15" fill="#E84150" width="15" height="15">
            <animateTransform attributeName="transform" attributeType="XML" type="translate" dur="1.7s" values="0,0;0,15;-15,15;-15,0;0,0;" repeatCount="indefinite"/>
		      </rect>	
      
		      <rect x="15" y="15" fill="#62B87B" width="15" height="15">
            <animateTransform attributeName="transform" attributeType="XML" type="translate" dur="1.7s" values="0,0;-15,0;-15,-15;0,-15;0,0;" repeatCount="indefinite"/>
		      </rect>	

		      <rect y="15" fill="#2F6FB6" width="15" height="15">
            <animateTransform attributeName="transform" attributeType="XML" type="translate" dur="1.7s" values="0,0;0,-15;15,-15;15,0;0,0;" repeatCount="indefinite"/>
		      </rect>
        </svg>
        '
      )
    )
    
  }
  
  # page wrapper
  shiny::tags$html(
    # Head
    shiny::tags$head(
      shiny::tags$meta(charset = "utf-8"),
      shiny::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      shiny::tags$meta(`http-equiv` = "x-ua-compatible", content = "ie=edge"),
      
      shiny::tags$title(title)
    ),
    # Body
    addDeps(
      theme = old_school,
      shiny::tags$body(
        class = sidebar_cl,
        `sidebar-start-open` = tolower(!sidebar_collapsed),
        
        # set up a time-out for the preloader
        # The body content disappears first,
        # then 2s after, the preloader disappears and 
        # the body content is shown
        onload = if (enable_preloader) {
          duration <- loading_duration * 1000
          paste0(
            "$(document).ready(function() {
              setTimeout(function(){
                $('.pre').remove();
                $('.wrapper').css('visibility', 'visible');
                $('body').css('background', '#fff');
                }, ", duration, ");
            });
            "
          )
        },
        
        # preloader, if any
        if (enable_preloader) {
          shiny::tagList(
            # preloader CSS
            shiny::singleton(
              shiny::tags$style(
                paste0(
                  "body {
                    background: ", loading_background, ";
                   }
                  .pre  {
                    position: absolute;
                    top: 50%;
                    left: 50%;
                    margin-top: -15px;
                    margin-left: -15px;
                  }
                "
                )
              )
            ),
            # preloader tag
            preloaderTag,
            bodyContent
          )
        } else {
          bodyContent
        }
      )
    )
  )
}
