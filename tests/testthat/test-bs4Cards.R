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
  bs4Dash:::dropNulls(header$children[[2]]$children[[2]])
}

test_that("is shiny tag?", {
  expect_s3_class(bs4Card(), "shiny.tag")
})

test_that("card structure", {
  cardChildren <- getCardChildren(bs4Card(footer = "footer"))
  expect_equal(class(cardChildren), "list")
  expect_length(cardChildren, 3)

  cardChildren <- getCardChildren(bs4Card())
  expect_length(cardChildren, 2)

  # if collapsible is FALSE, there is still header with title "\u200C"
  cardChildren <- getCardChildren(bs4Card(collapsible = FALSE))
  expect_length(cardChildren, 2)
  expect_equal(
    getCardChildren(bs4Card(collapsible = FALSE))[[1]]$children[[
      1
    ]]$attribs$class,
    "card-title"
  )

  # if collapsible is FALSE but title is not NULL, the header is included
  cardChildren <- getCardChildren(bs4Card(
    collapsible = FALSE,
    title = "card title"
  ))
  expect_length(cardChildren, 2)
})

test_that("card tools", {
  parms <- list()

  # collapsible/closable and maximizable are contained in a sublist
  parms$collapsible <- TRUE
  cardTag <- do.call(bs4Card, parms)
  toolsTag <- getCardTools(cardTag)
  expect_length(toolsTag, 1)

  parms$closable <- TRUE
  cardTag <- do.call(bs4Card, parms)
  toolsTag <- getCardTools(cardTag)
  expect_length(toolsTag, 2)

  parms$maximizable <- TRUE
  cardTag <- do.call(bs4Card, parms)
  toolsTag <- getCardTools(cardTag)
  expect_length(toolsTag, 3)

  parms$dropdownMenu <- boxDropdown(
    boxDropdownItem("plop"),
    boxDropdownItem("plop2")
  )
  cardTag <- do.call(bs4Card, parms)
  toolsTag <- getCardTools(cardTag)
  expect_length(toolsTag, 4)

  parms$sidebar <- boxSidebar()
  cardTag <- do.call(bs4Card, parms)
  toolsTag <- getCardTools(cardTag)
  expect_length(toolsTag, 5)

  # label does not belong to the toolsTag list
  parms$label <- boxLabel(text = "label", status = "danger")
  cardTag <- do.call(bs4Card, parms)
  toolsTag <- getCardTools(cardTag)
  expect_length(toolsTag, 5)
})

test_that("default", {
  cardTag <- bs4Card()
  cardCl <- getCardCl(cardTag)
  expect_match(cardCl, "card bs4Dash")
})

test_that("status", {
  cardTag <- bs4Card(status = "success")
  cardCl <- getCardCl(cardTag)
  expect_match(cardCl, "card bs4Dash card-success")
})

test_that("gradient", {
  expect_error(bs4Card(gradient = TRUE, background = NULL))
  cardTag <- bs4Card(
    gradient = TRUE,
    background = "danger",
    solidHeader = TRUE,
    status = "danger"
  )
  cardCl <- getCardCl(cardTag)
  expect_match(cardCl, "card bs4Dash card-danger bg-gradient-danger")
})

test_that("solidheader", {
  expect_error(bs4Card(
    solidHeader = FALSE,
    status = "primary",
    background = "purple"
  ))

  cardTag <- bs4Card(solidHeader = FALSE, status = "warning")
  cardCl <- getCardCl(cardTag)
  expect_match(cardCl, "card bs4Dash card-warning card-outline")
})

test_that("card bs4Dash sidebar class", {
  cardTag <- bs4Card(
    sidebar = bs4CardSidebar(startOpen = TRUE)
  )
  cardCl <- getCardCl(cardTag)
  expect_match(cardCl, "card bs4Dash direct-chat direct-chat-contacts-open")
})

test_that("collapsible and collapsed", {
  expect_error(bs4Card(collapsible = FALSE, collapsed = TRUE))

  cardTag <- bs4Card(collapsible = TRUE, collapsed = TRUE)
  cardCl <- getCardCl(cardTag)
  expect_match(cardCl, "card bs4Dash collapsed-card")
})

test_that("elevation", {
  cardTag <- bs4Card(elevation = 4)
  cardCl <- getCardCl(cardTag)
  expect_match(cardCl, "card bs4Dash elevation-")
  expect_error(bs4Card(elevation = 6))
  expect_error(bs4Card(elevation = -1))
  expect_error(bs4Card(elevation = "2"))
})

test_that("headerBorder", {
  cardTag <- bs4Card(headerBorder = FALSE)
  cardHeaderCl <- getCardHeaderCl(cardTag)
  expect_match(cardHeaderCl, "card-header border-0")
})

# test_that("overflow without height", {
#   expect_error(bs4Card(height = "500px", overflow = TRUE))
# })

test_that("height", {
  # check if shiny::validateCssUnit does its job
  expect_error(bs4Card(height = "prout"))

  cardTag <- bs4Card(height = "400px")
  bodyStyle <- getCardBodyStyle(cardTag)
  expect_match(bodyStyle, "height: 400px")

  # cardTag <- bs4Card(overflow = TRUE)
  # bodyStyle <- getCardBodyStyle(cardTag)
  # expect_match(bodyStyle, "overflow-y: auto; max-height: 500px;")
})

test_that("body content", {
  cardTag <- bs4Card()
  bodyTag <- getCardBody(cardTag)
  expect_length(bodyTag, 0)

  cardTag <- bs4Card("prout")
  bodyTag <- getCardBody(cardTag)
  expect_length(bodyTag, 1)
})

test_that("card sidebar in card body", {
  cardTag <- bs4Card(sidebar = bs4CardSidebar())
  bodyTag <- getCardBody(cardTag)
  expect_length(bodyTag, 1)

  cardTag <- bs4Card("prout", sidebar = bs4CardSidebar())
  bodyTag <- getCardBody(cardTag)
  expect_length(bodyTag, 2)
})

test_that("find id", {
  cardTag <- bs4Card(id = "test")
  id <- getCardId(cardTag)
  expect_match(id, "test")
})

test_that("card width", {
  expect_error(bs4Card(width = "2"))
  expect_error(bs4Card(width = -1))
  cardTag <- bs4Card(width = 6)
  wrapperTagCl <- getCardWrapperCl(cardTag)
  expect_match(wrapperTagCl, "col-sm-6")
})

test_that("footer", {
  # if no footer is provided, the footer should not
  # even appear in the card tag
  cardTag <- bs4Card()
  expect_error(getCardFooter(cardTag))

  cardTag <- bs4Card(footer = "prout")
  cardFooterTag <- getCardFooter(cardTag)
  expect_match(cardFooterTag$attribs$class, "card-footer")
  expect_length(cardFooterTag$children, 1)
})
