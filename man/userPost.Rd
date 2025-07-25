% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/useful-items.R
\name{userPost}
\alias{userPost}
\alias{userPostTagItems}
\alias{userPostTagItem}
\title{AdminLTE3 user post}
\usage{
userPost(
  ...,
  id = NULL,
  image,
  author,
  description = NULL,
  collapsible = TRUE,
  collapsed = FALSE
)

userPostTagItems(...)

userPostTagItem(...)
}
\arguments{
\item{...}{Tool content such as label, button, ...}

\item{id}{Unique id of the post.}

\item{image}{Profile image, if any.}

\item{author}{Post author.}

\item{description}{Post description.}

\item{collapsible}{If TRUE, display a button in the upper right that allows the user to collapse the comment.}

\item{collapsed}{Whether the comment is collapsed when the application starts, FALSE by default.}
}
\description{
Creates a user post. This content may be inserted in a \link{box}.

\link{userPostTagItems} creates a container to host \link{userPostTagItem}.

\link{userPostTagItem} creates a user post tool item
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
     box(
      title = "Box with user comment",
      status = "primary",
      userPost(
       id = 1,
       image = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
       author = "Jonathan Burke Jr.",
       description = "Shared publicly - 7:30 PM today",
       "Lorem ipsum represents a long-held tradition for designers,
       typographers and the like. Some people hate it and argue for
       its demise, but others ignore the hate as they create awesome
       tools to help create filler text for everyone from bacon
       lovers to Charlie Sheen fans.",
       collapsible = FALSE,
       userPostTagItems(
        userPostTagItem(dashboardBadge("item 1", color = "info")),
        userPostTagItem(dashboardBadge("item 2", color = "danger"), side = "right")
       )
      ),
      userPost(
       id = 2,
       image = "https://adminlte.io/themes/AdminLTE/dist/img/user6-128x128.jpg",
       author = "Adam Jones",
       userPostMedia(image = "https://adminlte.io/themes/AdminLTE/dist/img/photo2.png"),
       userPostTagItems(
        userPostTagItem(dashboardBadge("item 1", color = "success")),
        userPostTagItem(dashboardBadge("item 2", color = "danger"), side = "right")
       )
      )
     )
    ),
    title = "userPost"
  ),
  server = function(input, output) { }
 )
}

}
\seealso{
Other boxWidgets: 
\code{\link{attachmentBlock}()},
\code{\link{bs4CardLabel}()},
\code{\link{bs4CardSidebar}()},
\code{\link{bs4Carousel}()},
\code{\link{bs4SocialCard}()},
\code{\link{bs4Timeline}()},
\code{\link{cardDropdown}()},
\code{\link{cardProfile}()},
\code{\link{descriptionBlock}()}
}
\author{
David Granjon, \email{dgranjon@ymail.com}
}
\concept{boxWidgets}
