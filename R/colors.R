sidebarDarkStatuses <- c(
  "primary",
  "primary-subtle",
  "secondary",
  "secondary-subtle",
  "success",
  "success-subtle",
  "danger",
  "danger-subtle",
  "warning-subtle",
  "info-subtle",
  "light-subtle",
  "dark",
  "dark-subtle",
  "body-secondary",
  "body-tertiary",
  "body",
  "black"
)

sidebarLightStatuses <- c(
  "primary-subtle",
  "secondary-subtle",
  "success-subtle",
  "danger-subtle",
  "warning",
  "warning-subtle",
  "info",
  "info-subtle",
  "light",
  "light-subtle",
  "dark-subtle",
  "body-secondary",
  "body-tertiary",
  "body",
  "white",
  "transparent"
)

validateSidebarStatus <- function(status, skin) {
  if (!is.null(status)) {
    is_status_compatible <- switch(
      skin,
      "dark" = status %in% sidebarDarkStatuses,
      "light" = status %in% sidebarLightStatuses
    )
    if (!is_status_compatible) {
      stop(
        sprintf(
          "%s skin isn't compatible with color %s.
          The dark and light skins are compatible
          with sidebarDarkStatuses and sidebarLightStatuses,
          respectively.",
          skin,
          status
        )
      )
    }
  }
}

#' Get all AdminLTE colors.
#' @export
getAdminLTEColors <- function() {
  c(validStatuses, validNuances, validColors)
}

# Returns TRUE if a status is valid; throws error otherwise.
validateStatus <- function(status) {
  
  if (status %in% validStatuses) {
    return(TRUE)
  }
  
  stop("Invalid status: ", status, ". Valid statuses are: ",
       paste(validStatuses, collapse = ", "), ".")
}

#' Valid statuses
#'
#' These status strings correspond to colors as defined in Bootstrap's CSS.
#' Although the colors can vary depending on the particular CSS selector, they
#' generally appear as follows:
#'
#' \itemize{
#'   \item \code{primary} Blue (sometimes dark blue)
#'   \item \code{secondary} Light gray
#'   \item \code{info} Blue
#'   \item \code{success} Green
#'   \item \code{warning} Orange
#'   \item \code{danger} Red
#'   \item \code{light} Light
#'   \item \code{ddark} Dark
#' }
#'
#' @usage NULL
#' @format NULL
#'
#' @keywords internal
validStatuses <- c("primary", "secondary", "info", "success", "warning", "danger", "light", "dark")

# Returns TRUE if a nuance is valid; throws error otherwise.
validateNuance <- function(nuance) {
  
  if (nuance %in% validNuances) {
    return(TRUE)
  }
  
  stop("Invalid nuance: ", nuance, ". Valid nuances are: ",
       paste(validNuances, collapse = ", "), ".")
}

#' Valid nuances
#'
#' These nuances strings correspond to colors as defined in AdminLTE's CSS.
#' Although the colors can vary depending on the particular CSS selector, they
#' generally appear as follows:
#'
#' \itemize{
#'   \item \code{gray-dark} Gray dark
#'   \item \code{gray} Gray
#'   \item \code{white} White
#' }
#'
#' @usage NULL
#' @format NULL
#'
#' @keywords internal
validNuances <- c("gray-dark", "gray", "white", "black")

# Returns TRUE if a color is a valid color defined in AdminLTE, throws error
# otherwise.
validateColor <- function(color) {
  if (color %in% validColors) {
    return(TRUE)
  }
  
  stop("Invalid color: ", color, ". Valid colors are: ",
       paste(validColors, collapse = ", "), ".")
}

#' Valid colors
#'
#' These are valid colors for various dashboard components. Valid colors are
#' listed below:
#'
#' \itemize{
#'   \item \code{indigo} Indigo
#'   \item \code{purple} Purple
#'   \item \code{pink} Pink
#'   \item \code{orange} Orange
#'   \item \code{teal} Blue/Green
#' }
#'
#' @usage NULL
#' @format NULL
#'
#' @keywords internal
validColors <- c("indigo", "purple", "pink", "orange", "teal")

# Returns TRUE if a status is valid; throws error otherwise.
validateStatusPlus <- function(status) {
  
  if (status %in% validStatusesPlus) {
    return(TRUE)
  }
  
  stop("Invalid status: ", status, ". Valid statuses are: ",
       paste(validStatusesPlus, collapse = ", "), ".")
}

#' Valid statuses extra
#' @usage NULL
#' @format NULL
#'
#' @keywords internal
validStatusesPlus <- c(validStatuses, validNuances, validColors)

# used to generate color tags in the documentation
rd_color_tag <- function(color, label = color) {
  style <- sprintf(
    "width:12px;height:12px;background:%s;border-radius:2px;display:inline-block;margin-right:5px;",
    color
  )
  sprintf(
    "\\ifelse{html}{\\out{<span style='%s'></span>%s}}{%s}",
    style, label, label
  )
}
