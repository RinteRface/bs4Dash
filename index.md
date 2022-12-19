# bs4Dash 

[![R build status](https://github.com/RinteRface/bs4Dash/workflows/R-CMD-check/badge.svg)](https://github.com/RinteRface/bs4Dash/actions)
[![version](https://www.r-pkg.org/badges/version/bs4Dash)](https://CRAN.R-project.org/package=bs4Dash)
[![cranlogs](https://cranlogs.r-pkg.org/badges/bs4Dash)](https://CRAN.R-project.org/package=bs4Dash)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/bs4Dash)](https://www.rpackages.io/package/bs4Dash)
[![Codecov test coverage](https://codecov.io/gh/RinteRface/bs4Dash/branch/master/graph/badge.svg)](https://codecov.io/gh/RinteRface/bs4Dash?branch=master)

<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/marvel-devices.css@1.0.0/assets/devices.min.css" type="text/css" />

> Bootstrap 4 shinydashboard using [AdminLTE3](https://github.com/ColorlibHQ/AdminLTE)

<br>

<div class="card">
<a href="https://dgranjon.shinyapps.io/virtual_patient_v2/" target="_blank"><img src="https://community.rstudio.com/uploads/default/original/2X/e/eb1013fd09ccf10cbe13da3f0168eebfcb0eba75.gif"></a>
</div>

<br>

<div class="card">
<a href="https://analytichealth.co.uk/pharmly-portal/" target="_blank"><img src="https://analytichealth.co.uk/wp-content/uploads/2021/10/PA-bs4Dash.gif"></a>
</div>

<br>


<div class="marvel-device iphone8 black">
<div class="camera"></div>
<div class="screen">
<iframe width="100%" src="https://dgranjon.shinyapps.io/virtual_patient_v2/" allowfullscreen="" frameborder="0" scrolling="yes" height="770px"></iframe>
</div>
<div class="home"></div>
</div>

## New users moving to bs4Dash v2.0.0

Taking the simple `{shinydashboard}` example:

```r
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),

      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)
```

Starting from v2.0.0, moving to `{bs4Dash}` is rather simple:

```r
library(bs4Dash)
ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),

      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)
```


## Upgrading bs4Dash to 2.0.0
- `{bs4Dash}` is undergoing major rework to make it easier to come from `{shinydashboard}`. The current development version 2.0.0 provides a 1:1 supports, in other word moving from `{shinydashboard}` to `{bs4Dash}` is accomplished by changing `library(shinydashboard)` to `library(bs4Dash)`. 

- `{bs4Dash}` v2.0.0 also provides 1:1 with `{shinydashboardPlus}` to ease compatibility.

- Apps built with `{bs4Dash}` version <= 0.5.0 are definitely not compatible with v2.0.0 due to substantial breaking changes in the API. We advise users to keep the old version for old apps and move to to the new version for newer apps.

## Installation

```r
# latest devel version
devtools::install_github("RinteRface/bs4Dash")
# from CRAN
install.packages("bs4Dash")
```

## Demo

See a working example on shinyapps.io [here](https://dgranjon.shinyapps.io/bs4DashDemo/).
You may also run:

```r
library(bs4Dash)
bs4DashGallery()
```

## Issues

Issues are listed [here](https://github.com/RinteRface/bs4Dash/issues). 


## Acknowledgement

I warmly thank [Glyphicons](https://www.glyphicons.com) creator for providing them for free with Bootstrap. 

## Code of Conduct
  
Please note that the bs4Dash project is released with a [Contributor Code of Conduct](https:/contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree toabide by its terms.
