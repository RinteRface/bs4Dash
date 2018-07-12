# bs4Dash
[![Build Status](https://travis-ci.org/DivadNojnarg/bs4Dash.svg?branch=master)](https://travis-ci.org/DivadNojnarg/bs4Dash)
[![version](http://www.r-pkg.org/badges/version/bs4Dash)](https://CRAN.R-project.org/package=bs4Dash)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-ff69b4.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Project Status](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

> Bootstrap 4 shinydashboard using AdminLTE3

<video width="640" height="480" controls style="display: block; margin: 0 auto;" muted>
  <source src="figures/bs4DashDemo-classic.mp4" type="video/mp4">
</video>

<br>

<video width="640" height="480" controls style="display: block; margin: 0 auto;" muted>
  <source src="figures/bs4DashDemo-old-school.mp4" type="video/mp4">
</video>

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

## Issues

Issues are listed [here](https://github.com/DivadNojnarg/bs4Dash/issues). 