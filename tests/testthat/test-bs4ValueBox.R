getCardWrapperCl <- function(card) {
  card$attribs$class
}

getCardCl <- function(card) {
  card$children[[1]]$attribs$class
}

getCardChildren <- function(card) {
  bs4Dash:::dropNulls(card$children[[1]]$children)
}


test_that("structure", {
  golem::expect_shinytag(bs4ValueBox(2, "Value"))
  expect_error(bs4ValueBox(2))
  expect_error(bs4ValueBox(subtitle = "Value"))
  expect_error(bs4ValueBox(2, "Value", href = "ppp", footer = "kkk"))

  # inner (value + footer). Footer is just here for styling purpose
  valueBoxTag <- bs4ValueBox(2, "Value")
  valueBoxChildren <- getCardChildren(valueBoxTag)
  expect_length(valueBoxChildren, 2)
  expect_match(valueBoxChildren[[1]]$attribs$class, "inner")
  expect_match(valueBoxChildren[[2]]$attribs$class, "small-box-footer")

  # inner + icon
  valueBoxTag <- bs4ValueBox(2, "Value", icon = shiny::icon("gears"))
  valueBoxChildren <- getCardChildren(valueBoxTag)
  expect_length(valueBoxChildren, 3)
  expect_match(valueBoxChildren[[1]]$attribs$class, "inner")
  expect_match(valueBoxChildren[[2]]$attribs$class, "icon")
  expect_match(valueBoxChildren[[3]]$attribs$class, "small-box-footer")

  # wrong icon
  expect_error(
    expect_output(bs4ValueBox(2, "Value", icon = "popoiiuu"))
  )

  # inner + icon + footer
  valueBoxTag <- bs4ValueBox(
    2,
    "Value",
    footer = "test",
    icon = shiny::icon("gears")
  )
  valueBoxChildren <- getCardChildren(valueBoxTag)
  expect_length(valueBoxChildren, 3)
  expect_match(valueBoxChildren[[1]]$attribs$class, "inner")
  expect_match(valueBoxChildren[[2]]$attribs$class, "icon")
  expect_match(valueBoxChildren[[3]]$attribs$class, "small-box-footer")

  # inner + href
  valueBoxTag <- bs4ValueBox(2, "Value", href = "test")
  valueBoxChildren <- getCardChildren(valueBoxTag)
  expect_length(valueBoxChildren, 2)
  expect_match(valueBoxChildren[[2]]$attribs$class, "small-box-footer")
})

test_that("box width", {
  expect_error(bs4ValueBox(2, "Value", width = "2"))
  expect_error(bs4ValueBox(2, "Value", width = -1))
  valueBoxTag <- bs4ValueBox(2, "Value", width = 4)
  wrapperTagCl <- getCardWrapperCl(valueBoxTag)
  expect_match(wrapperTagCl, "col-sm-4")
})

test_that("status css class", {
  valueBoxTag <- bs4ValueBox(2, "Value")
  valueBoxCl <- getCardCl(valueBoxTag)
  expect_match(valueBoxCl, "small-box")

  valueBoxTag <- bs4ValueBox(2, "Value", color = "primary")
  valueBoxCl <- getCardCl(valueBoxTag)
  expect_match(valueBoxCl, "small-box bg-primary")

  expect_error(
    bs4ValueBox(2, "Value", color = NULL, gradient = TRUE)
  )

  expect_error(bs4ValueBox(2, "Value", color = "prout"))

  valueBoxTag <- bs4ValueBox(2, "Value", color = "primary", gradient = TRUE)
  valueBoxCl <- getCardCl(valueBoxTag)
  expect_match(valueBoxCl, "small-box bg-gradient-primary")
})

test_that("box elevation", {
  valueBoxTag <- bs4ValueBox(2, "Value", elevation = 4)
  valueBoxCl <- getCardCl(valueBoxTag)
  expect_match(valueBoxCl, "elevation-4")

  expect_error(bs4ValueBox(2, "Value", elevation = 6))
  expect_error(bs4ValueBox(2, "Value", elevation = -1))
  expect_error(bs4ValueBox(2, "Value", elevation = "2"))
})
