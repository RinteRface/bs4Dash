# Packages ----------------------------------------------------------------

library(jstools)

# Validate ----------------------------------------------------------------

bs4DashJS <- list.files(
  path = "srcjs/bs4Dash-2.0.0",
  recursive = TRUE,
  full.names = TRUE
)

# jshint_file(input = bs4DashJS, options = jshint_options(jquery = TRUE, globals = list("Shiny", "app")))

outputDir <- "inst/bs4Dash-2.0.0"

# Concat -----------------------------------------------------------------

# This just aggregates all srcjs files into one big .js file. There is no minifications, ... See next step for terser
system(sprintf("cat %s > %s/bs4Dash.js", paste(bs4DashJS, collapse = " "), outputDir))

# Concat + Compress + source maps ----------------------------------------------------------------

terser_file(
  input = bs4DashJS,
  output = sprintf("%s/bs4Dash.min.js", outputDir),
  options = terser_options(
    sourceMap = list(
      root = "../../bs4Dash-build",
      filename = "bs4Dash.min.js",
      url = "bs4Dash.min.js.map",
      includeSources = TRUE
    )
  )
)