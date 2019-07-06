library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(shinyEffects)
library(rlang)

fctList <- ls("package:shiny")
# select all functions containing inputs
inputIdx <- grep(pattern = "Input", x = fctList)
# remove all non input functions + strange inputs
inputList <- c(
  fctList[inputIdx][-c(8, 9, 10, 14, 17:30)], 
  "radioButtons", 
  "actionButton",
  "actionLink"
)[-9]


dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

generateInput <- function(item) {
  parms <- formals(item)
  parms <- unlist(dropNulls(
    lapply(seq_along(parms), function(i) {
      if (is.symbol(parms[[i]]) & 
          names(parms)[[i]] != "..." |
          names(parms)[[i]] == "choices") {
        id <- round(runif(1, min = 0, max = 1e007))
        value <- switch(
          names(parms)[i],
          "min" = 0,
          "max" = 10,
          "value" = 5,
          "inputId" = id,
          "label" = paste0("input$", id),
          "choices" = letters[1:3]
        )
        if (!is.numeric(value)) value <- paste0("\"", value, "\"")
        if (length(value) > 1) {
          value <- paste(value, collapse = ",")
          paste0(names(parms)[[i]], "=", "c(", value ,")")
        } else {
          paste0(names(parms)[[i]], "=", value)
        }
      }
    })
  ))
  # aggregate parms in 1 string separated by comma
  parms <- paste(parms, collapse = ",")
  
  # return the corresponding expression
  parse(text = paste0(item, "(", parms, ')'))
}

test <- generateInput("checkboxGroupInput")

tags <- tabItems(
  tabItem(
    tabName = "test",
    fluidRow(
      column(6, lapply(seq_along(inputList), function(i) eval(generateInput(inputList[i])))),
      column(6, lapply(seq_along(inputList), function(i) verbatimTextOutput(paste0("out", i))))
    )
  )
)


shinyApp(
  ui = bs4DashPage(
    sidebar_collapsed = TRUE,
    controlbar_collapsed = TRUE,
    enable_preloader = FALSE,
     navbar = bs4DashNavbar(skin = "dark"),
     body = bs4DashBody(tags),
     sidebar = bs4DashSidebar(skin = "dark"),
     controlbar = bs4DashControlbar(skin = "dark"),
     footer = bs4DashFooter()
  ),
  server = function(input, output, session) {
    rv <- reactiveValues(inputs = NULL)
    observe({
      rv$inputs <- reactiveValuesToList(input)
      print(names(rv$inputs))
    })
    
    lapply(seq_along(inputList), function(i) {
      output[[paste0("out", i)]] <- renderPrint({
        paste0("input$", names(rv$inputs)[i], " is ", rv$inputs[[i]])
      })
    })
    
  }
)
