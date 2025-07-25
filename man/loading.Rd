% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/useful-items.R, R/aliases.R
\name{bs4Loading}
\alias{bs4Loading}
\alias{loadingState}
\title{AdminLTE3 loading state element}
\usage{
bs4Loading()

loadingState()
}
\description{
When a section is still work in progress or a computation is running
}
\note{
Loading state can be programmatically used when a conputation is running for instance.
}
\examples{
if(interactive()){
 library(shiny)
 library(bs4Dash)

 shinyApp(
  ui = dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
     box(
      title = "loading spinner",
      loadingState()
      )
    ),
    title = "Loading State"
  ),
  server = function(input, output) { }
 )
}

}
\author{
David Granjon, \email{dgranjon@ymail.com}
}
