# bs4Dash <img src="https://rinterface.com/inst/images/bs4Dash.svg" width="200px" align="right"/>

[![R build status](https://github.com/RinteRface/bs4Dash/workflows/R-CMD-check/badge.svg)](https://github.com/RinteRface/bs4Dash/actions)
[![version](https://www.r-pkg.org/badges/version/bs4Dash)](https://CRAN.R-project.org/package=bs4Dash)
[![cranlogs](https://cranlogs.r-pkg.org/badges/bs4Dash)](https://CRAN.R-project.org/package=bs4Dash)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/bs4Dash)](https://CRAN.R-project.org/package=bs4Dash)
[![Codecov test coverage](https://codecov.io/gh/RinteRface/bs4Dash/branch/master/graph/badge.svg)](https://codecov.io/gh/RinteRface/bs4Dash?branch=master)

> Bootstrap 4 shinydashboard using [AdminLTE3](https://github.com/ColorlibHQ/AdminLTE)

<br>

<div class="card">
<img src="https://community.rstudio.com/uploads/default/original/2X/e/eb1013fd09ccf10cbe13da3f0168eebfcb0eba75.gif">
</div>

<br>

<div class="card">
<a href="https://analytichealth.co.uk/pharmly-portal/" target="_blank"><img src="https://analytichealth.co.uk/wp-content/uploads/2021/10/PA-bs4Dash.gif"></a>
</div>

<br>

## From {shinydashboard} to {bs4Dash}

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

Moving to `{bs4Dash}` is rather simple, as we just replace `library(shinydashboard)`:

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

## Installation

```r
# latest devel version
pak::pak("RinteRface/bs4Dash")
# latest devel version from r-universe
install.packages("bs4Dash", repos = c("cynkra.r-universe.dev", "cloud.r-project.org"))
# from CRAN
install.packages("bs4Dash")
```

## Demo

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
