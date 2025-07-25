% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dashboardHeader.R, R/aliases.R
\name{bs4DropdownMenu}
\alias{bs4DropdownMenu}
\alias{messageItem}
\alias{notificationItem}
\alias{taskItem}
\alias{dropdownMenu}
\title{Boostrap 4 dashboard dropdown menu}
\usage{
bs4DropdownMenu(
  ...,
  type = c("messages", "notifications", "tasks"),
  badgeStatus = "primary",
  icon = NULL,
  headerText = NULL,
  .list = NULL,
  href = NULL
)

messageItem(
  from,
  message,
  icon = shiny::icon("user"),
  time = NULL,
  href = NULL,
  image = NULL,
  color = "secondary",
  inputId = NULL
)

notificationItem(
  text,
  icon = shiny::icon("triangle-exclamation"),
  status = "success",
  href = NULL,
  inputId = NULL
)

taskItem(text, value = 0, color = "info", href = NULL, inputId = NULL)

dropdownMenu(
  ...,
  type = c("messages", "notifications", "tasks"),
  badgeStatus = "primary",
  icon = NULL,
  headerText = NULL,
  .list = NULL,
  href = NULL
)
}
\arguments{
\item{...}{Items to put in the menu. Typically, message menus should contain
\code{\link{messageItem}}s, notification menus should contain
\code{\link{notificationItem}}s, and task menus should contain
\code{\link{taskItem}}s.}

\item{type}{The type of menu. Should be one of "messages", "notifications",
"tasks".}

\item{badgeStatus}{The status of the badge which displays the number of items
in the menu. This determines the badge's color. Valid statuses are defined as follows:
\itemize{
\item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
\item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
\item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
\item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
\item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
\item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
}.
A value of \code{NULL} means to not display a badge.}

\item{icon}{An icon tag, created by \code{\link[shiny]{icon}}.}

\item{headerText}{An optional text argument used for the header of the
dropdown menu (this is only visible when the menu is expanded). If none is
provided by the user, the default is "You have \code{x} messages," where
\code{x} is the number of items in the menu (if the \code{type} is
specified to be "notifications" or "tasks," the default text shows "You
have \code{x} notifications" or  "You have \code{x} tasks," respectively).}

\item{.list}{An optional list containing items to put in the menu Same as the
\code{...} arguments, but in list format. This can be useful when working
with programmatically generated items.}

\item{href}{An optional URL to link to.}

\item{from}{Who the message is from.}

\item{message}{Text of the message.}

\item{time}{String representing the time the message was sent. Any string may
be used. For example, it could be a relative date/time like "5 minutes",
"today", or "12:30pm yesterday", or an absolute time, like "2014-12-01 13:45".
If NULL, no time will be displayed.}

\item{image}{User image.}

\item{color}{A color for the bar. Valid colors are defined as follows:
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

\item{inputId}{Whether to allow the item to act as a \link[shiny]{actionButton}.}

\item{text}{The task text.}

\item{status}{The status of the item. This determines the item's background
color. Valid statuses are defined as follows:
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

\item{value}{A percent value to use for the bar.}
}
\description{
\link{dropdownMenu} creates an adminLTE3 dashboard dropdown menu, to be inserted in
a \link{dashboardHeader}.

\link{messageItem} creates a message item to place in a \link{dropdownMenu}.

\link{messageItem} creates a message item to place in a \link{dropdownMenu}.

\link{taskItem} creates a task item to place in a \link{dropdownMenu}.
}
\examples{
if (interactive()) {
  library(shiny)
  library(bs4Dash)

  shinyApp(
    ui = dashboardPage(
      header = dashboardHeader(
        rightUi = dropdownMenu(
          badgeStatus = "danger",
          type = "messages",
          messageItem(
            inputId = "triggerAction1",
            message = "message 1",
            from = "Divad Nojnarg",
            image = "https://adminlte.io/themes/v3/dist/img/user3-128x128.jpg",
            time = "today",
            color = "lime"
          )
        ),
        leftUi = tagList(
          dropdownMenu(
            badgeStatus = "info",
            type = "notifications",
            notificationItem(
              inputId = "triggerAction2",
              text = "Error!",
              status = "danger"
            )
          ),
          dropdownMenu(
            badgeStatus = "info",
            type = "tasks",
            taskItem(
              inputId = "triggerAction3",
              text = "My progress",
              color = "orange",
              value = 10
            )
          )
        )
      ),
      sidebar = dashboardSidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      title = "dropdownMenu",
      body = dashboardBody()
    ),
    server = function(input, output) {
      observeEvent(input$triggerAction1, {
        showModal(modalDialog(
          title = "Important message",
          "This is an important message!"
        ))
      })
    }
  )
}
}
\seealso{
\code{\link{dashboardHeader}} for example usage.
}
\author{
David Granjon, \email{dgranjon@ymail.com}
}
\concept{menu items}
