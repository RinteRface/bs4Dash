% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/useful-items.R, R/aliases.R
\name{bs4Accordion}
\alias{bs4Accordion}
\alias{bs4AccordionItem}
\alias{updateAccordion}
\alias{accordion}
\alias{accordionItem}
\title{Bootstrap 4 accordion container}
\usage{
bs4Accordion(..., id, width = 12, .list = NULL)

bs4AccordionItem(
  ...,
  title,
  status = NULL,
  collapsed = TRUE,
  solidHeader = TRUE
)

updateAccordion(id, selected, session = shiny::getDefaultReactiveDomain())

accordion(..., id, width = 12, .list = NULL)

accordionItem(..., title, status = NULL, collapsed = TRUE, solidHeader = TRUE)
}
\arguments{
\item{...}{slot for \link{accordionItem}.}

\item{id}{Accordion to target.}

\item{width}{The width of the accordion.}

\item{.list}{To pass \link{accordionItem} within a list.}

\item{title}{Optional title.}

\item{status}{The status of the item. This determines the item's background
color.  Valid statuses are defined as follows:
\itemize{
\item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
\item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
\item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
\item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
\item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
\item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
\item \code{gray-dark}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
\item \code{gray}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
\item \code{white}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#fff")}.
\item \code{indigo}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6610f2")}.
\item \code{lightblue}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3c8dbc")}.
\item \code{navy}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#001f3f")}.
\item \code{purple}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#605ca8")}.
\item \code{fuchsia}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#f012be")}.
\item \code{pink}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#e83e8c")}.
\item \code{maroon}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#d81b60")}.
\item \code{orange}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ff851b")}.
\item \code{lime}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#01ff70")}.
\item \code{teal}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#39cccc")}.
\item \code{olive}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3d9970")}.
}}

\item{collapsed}{If TRUE, start collapsed. This must be used with
\code{collapsible=TRUE}.}

\item{solidHeader}{Should the header be shown with a solid color background?}

\item{selected}{Index of the newly selected \link{accordionItem}.}

\item{session}{Shiny session object.}
}
\description{
\link{accordion} creates an accordion container.
Accordions are part of collapsible elements.

\link{accordionItem} is to be inserted in a \link{accordion}.

\link{updateAccordion} toggles an \link{accordion} on the client.
}
\examples{
if (interactive()) {
 library(shiny)
 library(bs4Dash)

 shinyApp(
  ui = dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
      accordion(
       id = "accordion1",
        accordionItem(
          title = "Accordion 1 Item 1",
          status = "danger",
          collapsed = TRUE,
          "This is some text!"
        ),
        accordionItem(
          title = "Accordion 1 Item 2",
          status = "indigo",
          collapsed = FALSE,
          "This is some text!"
        )
      ),
      accordion(
       id = "accordion2",
        accordionItem(
          title = "Accordion 2 Item 1",
          status = "info",
          collapsed = TRUE,
          "This is some text!"
        ),
        accordionItem(
          title = "Accordion 2 Item 2",
          status = "success",
          collapsed = FALSE,
          "This is some text!"
        )
      ),
      accordion(
        id = "accordion_dynamic",
        .list = lapply(
          1:2,
          function(i)
            accordionItem(
              title = paste('Accordion 1 Item', i),
              status = "danger",
              collapsed = ifelse (i == 1, TRUE, FALSE),
              "This is some text!"
            )
         )
       )
    ),
    title = "Accordion"
  ),
  server = function(input, output) {
   observe({
     print(input$accordion1)
     print(input$accordion2)
     print(input$accordion_dynamic)
   })
  }
 )
}


# Update accordion
if (interactive()) {
 library(shiny)
 library(bs4Dash)

 shinyApp(
  ui = dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
      radioButtons("controller", "Controller", choices = c(1, 2)),
      br(),
      accordion(
        id = "accordion1",
        accordionItem(
          title = "Accordion 1 Item 1",
          status = "danger",
          collapsed = TRUE,
          "This is some text!"
        ),
        accordionItem(
          title = "Accordion 1 Item 2",
          status = "warning",
          collapsed = TRUE,
          "This is some text!"
        )
      )
    ),
    title = "Update Accordion"
  ),
  server = function(input, output, session) {
    observeEvent(input$controller, {
      updateAccordion(id = "accordion1", selected = input$controller)
    })
    observe(print(input$accordion1))
    observeEvent(input$accordion1, {
      showNotification(sprintf("You selected accordion N° \%s", input$accordion1), type = "message")
    })
  }
 )
}
}
\author{
David Granjon, \email{dgranjon@ymail.com}
}
