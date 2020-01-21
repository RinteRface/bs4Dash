context("bs4Card")

getCardWrapperCl <- function(card) {
  card$attribs$class
}

getCardCl <- function(card) {
  card$children[[1]]$attribs$class
}

getCardId <- function(card) {
  card$children[[1]]$attribs$id
}

getCardChildren <- function(card) {
  bs4Dash:::dropNulls(card$children[[1]]$children)
}


getCardHeader <- function(card) {
  getCardChildren(card)[[1]]
}

getCardHeaderCl <- function(card) {
  header <- getCardHeader(card)
  header$attribs$class
}

getCardBody <- function(card) {
  bs4Dash:::dropNulls(getCardChildren(card)[[2]]$children)
}

getCardFooter <- function(card) {
  getCardChildren(card)[[3]]
}

getCardBodyStyle <- function(card) {
  card$children[[1]]$children[[2]]$attribs$style
}

getCardTools <- function(card) {
  header <- getCardHeader(card)
  bs4Dash:::dropNulls(header$children[[2]]$children)
}

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
