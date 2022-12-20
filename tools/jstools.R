# Packages ----------------------------------------------------------------

library(jstools)
# install bs4Dash first to get latest version
pkg_version <- as.character(utils::packageVersion("bs4Dash"))

# Rename srcjs folder in case pkg version changes
tmp_old_version <- list.dirs("srcjs", full.names = FALSE, recursive = FALSE)
old_version <- strsplit(tmp_old_version, "-")[[1]][2]
new_srcjs_folder_name <- sprintf("srcjs/bs4Dash-%s", pkg_version)

if (old_version != pkg_version) {
  file.rename(
    file.path("srcjs", tmp_old_version), 
    new_srcjs_folder_name
  )
}

# Validate ----------------------------------------------------------------

bs4DashJS <- list.files(
  path = new_srcjs_folder_name,
  recursive = TRUE,
  full.names = TRUE
)

# jshint_file(input = bs4DashJS, options = jshint_options(jquery = TRUE, globals = list("Shiny", "app")))

outputDir <- sprintf("inst/bs4Dash-%s", pkg_version)

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
