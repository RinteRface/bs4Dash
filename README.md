# bs4Dash
[![Build Status](https://travis-ci.org/DivadNojnarg/bs4Dash.svg?branch=master)](https://travis-ci.org/DivadNojnarg/bs4Dash)
[![version](http://www.r-pkg.org/badges/version/bs4Dash)](https://CRAN.R-project.org/package=bs4Dash)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-ff69b4.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Project Status](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

> Bootstrap 4 shinydashboard using AdminLTE3

<img src="man/figures/bs4DashClassic.png">
<br>

## Important note

bs4Dash needs the following code to be added in the header in order to
work on shinyapps.io:

```r
shiny::tags$head(
  shiny::tags$script(
    "// handle shinyapps.io: we need to extract the worker id and
     // paste it in the url so that the apps works correctly
     // get the shiny app.io workerId
     var workerId = $('base').attr('href');
     // ensure that this code does not run on shiny server/pro and locally
     if (typeof workerId != 'undefined') {
       // get the initial page url
       var url = window.location.href;
       // get the name of the first selected tab
       // replace the url by the url for shinyapp.io
       window.location.replace(url + workerId);
     }
    "
  )
)
```

This code has to be embeded as follows:

```r
library(shiny)
library(bs4Dash)

shiny::shinyApp(
  ui = bs4DashPage(
    navbar = bs4DashNavbar(),
    sidebar = bs4DashSidebar(),
    controlbar = bs4DashControlbar(),
    footer = bs4DashFooter(),
    title = "test",
    body = bs4DashBody(
      shiny::tags$head(
        shiny::tags$script(
          "// handle shinyapps.io: we need to extract the worker id and
           // paste it in the url so that the apps works correctly
           // get the shiny app.io workerId
           var workerId = $('base').attr('href');
           // ensure that this code does not run on shiny server/pro and locally
           if (typeof workerId != 'undefined') {
             // get the initial page url
             var url = window.location.href;
             // get the name of the first selected tab
             // replace the url by the url for shinyapp.io
             window.location.replace(url + workerId);
           }
          "
        )
    )
  ),
  server = function(input, output) {}
)
```

This issue is **fixed** on the new github version but you need to 
**wait for the next CRAN release** (next week) since shinyapps.io
uses the latest CRAN version.

See a working example [here](https://dgranjon.shinyapps.io/bs4DashDemo/).

## Installation

This package is on CRAN:

```r
# from CRAN
install.packages("bs4Dash")
# latest devel version
devtools::install_github("DivadNojnarg/bs4Dash")
```

## Demo

See a preview of the package [here](http://130.60.24.205/bs4Dash/showcase/classic) and
[here](http://130.60.24.205/bs4Dash/showcase/old_school) or run

```r
library(bs4Dash)
# classic theme
bs4DashGallery()
# old_school theme
bs4DashGallery(theme = "old_school")
```

An applied example can be found [here](http://130.60.24.205/dreamRs_ratp/) (the 
original dashboard was made by [Philippine Rheins](https://twitter.com/PhilippineRs) 
from [dreamRs](https://twitter.com/dreamRs_fr)).

## Issues

Issues are listed [here](https://github.com/DivadNojnarg/bs4Dash/issues). 