% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/useful-items.R, R/aliases.R
\name{bs4Sortable}
\alias{bs4Sortable}
\alias{sortable}
\title{BS4 sortable section}
\usage{
bs4Sortable(..., width = 12)

sortable(..., width = 12)
}
\arguments{
\item{...}{Slot for UI elements such as \link{box}.}

\item{width}{Section width: between 1 and 12.}
}
\description{
Create a sortable UI section
}
\examples{
if (interactive()) {
 library(shiny)
 library(bs4Dash)

 shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(),
    sidebar = dashboardSidebar(),
    controlbar = dashboardControlbar(),
    footer = dashboardFooter(),
    title = "Sortable UI",
    body = dashboardBody(
      fluidRow(
       lapply(1:3, FUN = function(i) {
         sortable(
           width = 4,
           p(class = "text-center", paste("Column", i)),
           lapply(1:2, FUN = function(j) {
             box(
               title = paste0("I am the ", j,"-th card of the ", i, "-th column"),
               width = 12,
               "Click on my header"
             )
           })
         )
       })
      )
    )
  ),
  server = function(input, output) {}
 )
}

}
\author{
David Granjon, \email{dgranjon@ymail.com}
}
