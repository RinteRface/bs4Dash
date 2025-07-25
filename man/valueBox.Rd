% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render-functions.R, R/cards.R, R/aliases.R
\name{renderbs4ValueBox}
\alias{renderbs4ValueBox}
\alias{bs4ValueBoxOutput}
\alias{bs4ValueBox}
\alias{valueBox}
\alias{valueBoxOutput}
\alias{renderValueBox}
\title{Create a value box (server side)}
\usage{
renderbs4ValueBox(expr, env = parent.frame(), quoted = FALSE)

bs4ValueBoxOutput(outputId, width = 4)

bs4ValueBox(
  value,
  subtitle,
  icon = NULL,
  color = NULL,
  width = 3,
  href = NULL,
  footer = NULL,
  gradient = FALSE,
  elevation = NULL
)

valueBox(
  value,
  subtitle,
  icon = NULL,
  color = NULL,
  width = 3,
  href = NULL,
  footer = NULL,
  gradient = FALSE,
  elevation = NULL
)

valueBoxOutput(outputId, width = 4)

renderValueBox(expr, env = parent.frame(), quoted = FALSE)
}
\arguments{
\item{expr}{An expression that returns a Shiny tag object, \code{\link[shiny:HTML]{HTML()}},
or a list of such objects.}

\item{env}{The parent environment for the reactive expression. By default,
this is the calling environment, the same as when defining an ordinary
non-reactive expression. If \code{expr} is a quosure and \code{quoted} is \code{TRUE},
then \code{env} is ignored.}

\item{quoted}{If it is \code{TRUE}, then the \code{\link[=quote]{quote()}}ed value of \code{expr}
will be used when \code{expr} is evaluated. If \code{expr} is a quosure and you
would like to use its expression as a value for \code{expr}, then you must set
\code{quoted} to \code{TRUE}.}

\item{outputId}{Output variable name.}

\item{width}{The width of the box, using the Bootstrap grid system. This is
used for row-based layouts. The overall width of a region is 12, so the
default width of 4 occupies 1/3 of that width. For column-based
layouts, use \code{NULL} for the width; the width is set by the column that
contains the box.}

\item{value}{The value to display in the box. Usually a number or short text.}

\item{subtitle}{Subtitle text.}

\item{icon}{An icon tag, created by \code{\link[shiny]{icon}}.}

\item{color}{The color of the item. This determines the item's background
color.  Valid colors are defined as follows:
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

\item{href}{An optional URL to link to in the footer. Should both \code{footer}
and this parameter be set, \code{footer} will take precedence.}

\item{footer}{Optional html content for the footer of the box.}

\item{gradient}{Whether to use gradient style for background color. Default to FALSE.}

\item{elevation}{Value box elevation.}
}
\description{
This is the server-side function for creating a dynamic
\code{\link{bs4ValueBox}}.

This is the UI-side function for creating a dynamic \code{\link{bs4ValueBox}}.

A beautiful AdminLTE3 value box.
}
\examples{
if (interactive()) {
  library(shiny)
  library(bs4Dash)

  shiny::shinyApp(
    ui = dashboardPage(
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      title = "test",
      body = dashboardBody(
        fluidRow(
          valueBoxOutput("vbox"),
          infoBoxOutput("ibox")
        )
      )
    ),
    server = function(input, output) {
      output$vbox <- renderValueBox({
        valueBox(
          value = 150,
          subtitle = "New orders",
          color = "primary",
          icon = icon("shopping-cart"),
          href = "#"
        )
      })

      output$ibox <- renderInfoBox({
        infoBox(
          title = "Comments",
          fill = TRUE,
          gradient = TRUE,
          color = "success",
          value = 41410,
          icon = icon("comments")
        )
      })
    }
  )
}
if (interactive()) {
  library(shiny)
  library(bs4Dash)

  shinyApp(
    ui = dashboardPage(
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      title = "test",
      body = bs4DashBody(
        fluidRow(
          valueBox(
            value = 150,
            subtitle = "New orders",
            color = "primary",
            icon = icon("cart-shopping")
          ),
          valueBox(
            value = "53\%",
            subtitle = "New orders",
            color = "indigo",
            icon = icon("gears"),
            footer = div("Hello World")
          ),
          valueBox(
            value = "44",
            subtitle = "User Registrations",
            color = "teal",
            icon = icon("sliders")
          )
        )
      )
    ),
    server = function(input, output) {}
  )
}
}
\seealso{
\code{\link{bs4ValueBoxOutput}} for the corresponding UI-side function.

\code{\link{renderbs4ValueBox}} for the corresponding server-side
function and examples.

Other cards: 
\code{\link{bs4CardLayout}()},
\code{\link{bs4SocialCard}()},
\code{\link{bs4TabCard}()},
\code{\link{bs4UserCard}()},
\code{\link{descriptionBlock}()},
\code{\link{renderbs4InfoBox}()}
}
\author{
David Granjon, \email{dgranjon@ymail.com}
}
\concept{cards}
