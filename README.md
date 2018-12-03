# bs4Dash <img src="https://rinterface.com/inst/images/bs4Dash.svg" width="200px" align="right"/>

[![Build Status](https://travis-ci.org/RinteRface/bs4Dash.svg?branch=master)](https://travis-ci.org/RinteRface/bs4Dash)
[![version](http://www.r-pkg.org/badges/version/bs4Dash)](https://CRAN.R-project.org/package=bs4Dash)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-ff69b4.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![cranlogs](https://cranlogs.r-pkg.org/badges/bs4Dash)](https://CRAN.R-project.org/package=bs4Dash)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/bs4Dash)](https://www.rpackages.io/package/bs4Dash)

> Bootstrap 4 shinydashboard using AdminLTE3

<br>

<div class="row">
<div class="col-sm-4" align="center">
<div class="card">
<a href="https://rinterface.com/shiny/showcase/lorenz/" target="_blank"><img src="man/figures/bs4Dash_Lorenz.png"></a>
</div>
</div>
<div class="col-sm-4" align="center">
<div class="card">
<a href="https://rinterface.com/shiny/showcase/lotkaVolterra/" target="_blank"><img src="man/figures/bs4Dash_Lotka.png"></a>
</div>
</div>
<div class="col-sm-4" align="center">
<div class="card">
<a href="https://rinterface.com/shiny/showcase/ratp/" target="_blank"><img src="man/figures/bs4Dash_ratp.png"></a>
</div>
</div>
</div>

<br>

See a working example on shinyapps.io [here](https://dgranjon.shinyapps.io/bs4DashDemo/).

## Useful Informations (if any)

...

## Installation

This package is on CRAN:

```r
# from CRAN
install.packages("bs4Dash")
# latest devel version
devtools::install_github("RinteRface/bs4Dash")
```

## Demo

See a preview of the package [here](https://rinterface.com/shiny/bs4Dash/classic/) and
[here](https://rinterface.com/shiny/bs4Dash/classic/old_school) or run

```r
library(bs4Dash)
# classic theme
bs4DashGallery()
# old_school theme
bs4DashGallery(theme = "old_school")
```

An applied example can be found [here](https://rinterface.com/shiny/showcase/ratp/) (the 
original dashboard was made by [Philippine Rheins](https://twitter.com/PhilippineRs) 
from [dreamRs](https://twitter.com/dreamRs_fr)).

## Issues

Issues are listed [here](https://github.com/RinteRface/bs4Dash/issues). 