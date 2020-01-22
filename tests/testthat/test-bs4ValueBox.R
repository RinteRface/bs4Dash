context("bs4ValueBox")

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
  expect_shinytag(bs4ValueBox(2, "Value"))
  expect_error(bs4ValueBox(2))
  expect_error(bs4ValueBox(subtitle = "Value"))
  expect_error(bs4ValueBox(2, "Value", href = "ppp", footer = "kkk"))
  
  # inner
  valueBoxTag <- bs4ValueBox(2, "Value")
  valueBoxChildren <- getCardChildren(valueBoxTag)
  expect_length(valueBoxChildren, 1)
  expect_match(valueBoxChildren[[1]]$attribs$class, "inner")
  
  # inner + icon
  valueBoxTag <- bs4ValueBox(2, "Value", icon = "cogs")
  valueBoxChildren <- getCardChildren(valueBoxTag)
  expect_length(valueBoxChildren, 2)
  expect_match(valueBoxChildren[[1]]$attribs$class, "inner")
  expect_match(valueBoxChildren[[2]]$attribs$class, "icon")
  
  # inner + icon + footer
  valueBoxTag <- bs4ValueBox(2, "Value", footer = "test", icon = "cogs")
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