# Packages ----------------------------------------------------------------

library(jstools)

# Validate ----------------------------------------------------------------

bs4DashJS <- list.files(
  path = "srcjs/bs4Dash-2.0.0",
  recursive = TRUE,
  full.names = TRUE
)

jshint_file(input = bs4DashJS, options = jshint_options(jquery = TRUE, globals = list("Shiny", "app")))


# Concat + Compress + source maps ----------------------------------------------------------------


# modified to register source maps
terser_file <- function (input, options = terser_options(), output = NULL) {
  input <- normalizePath(path = input, mustWork = TRUE)
  if (length(input) == 1) {
    inputs <- readLines(con = input, encoding = "UTF-8")
  }
  else {
    inputs <- lapply(input, readLines, encoding = "UTF-8")
    names(inputs) <- basename(input)
  }
  result <- terser(code = inputs, options = options)
  if (!is.null(result$error)) {
    message(result$error$name, result$error$message)
    return(invisible(result))
  }
  else {
    if (!is.null(output)) {
      writeLines(text = result$code, con = output)
      writeLines(text = result$map, con = "inst/bs4Dash-2.0.0/bs4Dash.min.js.map")
      return(invisible(result))
    }
    else {
      return(result)
    }
  }
}

terser_file(
  input = bs4DashJS, 
  output = "inst/bs4Dash-2.0.0/bs4Dash.min.js", 
  options = terser_options(
    sourceMap = list(
      filename = "inst/bs4Dash-2.0.0/bs4Dash.min.js",
      url = "bs4Dash.min.js.map"
    )
  )
)