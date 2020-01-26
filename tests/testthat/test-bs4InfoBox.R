context("bs4InfoBox")

getCardCl <- function(card) {
  card$children[[1]][[2]]$attribs$class
}

getCardChildren <- function(card) {
  bs4Dash:::dropNulls(card$children[[1]][[2]]$children)
}

getCardBody <- function(card) {
  getCardChildren(card)[[2]]
}

getCardBodyChildren <- function(card) {
  bs4Dash:::dropNulls(getCardBody(card)$children)
}


test_that("Is shiny tag", {
  golem::expect_shinytag(bs4InfoBox(title = "Infobox", status = "danger", value = 4))
})

test_that("overall structure", {
  infoBoxTag <- bs4InfoBox(title = "Infobox", value = 4)
  infoBoxChildren <- getCardChildren(infoBoxTag)
  expect_length(infoBoxChildren, 1)
  
  infoBoxTag <- bs4InfoBox(title = "Infobox", value = 4, icon = "cogs")
  infoBoxChildren <- getCardChildren(infoBoxTag)
  expect_length(infoBoxChildren, 2)
})

test_that("css class", {
  expect_error(
    bs4InfoBox(
      title = "Infobox", 
      value = 4,
      status = "danger", 
      gradientColor = "warning"
    )
  )
  
  infoBoxTag <- bs4InfoBox(title = "Infobox", value = 4)
  infoBoxCl <- getCardCl(infoBoxTag)
  expect_match(infoBoxCl, "info-box")
  
  infoBoxTag <- bs4InfoBox(title = "Infobox", status = "danger", value = 4)
  infoBoxCl <- getCardCl(infoBoxTag)
  expect_match(infoBoxCl, "info-box bg-danger")
  
  infoBoxTag <- bs4InfoBox(title = "Infobox", value = 4, gradientColor = "danger")
  infoBoxCl <- getCardCl(infoBoxTag)
  expect_match(infoBoxCl, "info-box bg-gradient-danger")
})

test_that("elevation", {
  infoBoxTag <- bs4InfoBox(title = "Infobox", value = 4, elevation = 4)
  infoBoxCl <- getCardCl(infoBoxTag)
  expect_match(infoBoxCl, "elevation-4")
  
  expect_error(bs4InfoBox(title = "Infobox", value = 4, elevation = 6))
  expect_error(bs4InfoBox(title = "Infobox", value = 4, elevation = -1))
  expect_error(bs4InfoBox(title = "Infobox", value = 4, elevation = "2"))
})

test_that("icon class", {
  expect_error(bs4InfoBox(title = "Infobox", value = 4, iconStatus = "primary"))
  expect_error(bs4InfoBox(title = "Infobox", value = 4, iconElevation = 2))
  expect_error(
    bs4InfoBox(
      title = "Infobox", 
      value = 4,
      icon = "cogs", 
      iconElevation = -1
    )
  )
  expect_error(
    bs4InfoBox(
      title = "Infobox", 
      value = 4,
      icon = "cogs", 
      iconElevation = "2"
    )
  )
  expect_error(
    bs4InfoBox(
      title = "Infobox", 
      value = 4,
      icon = "cogs", 
      iconElevation = 6
    )
  )
  
  infoBoxTag <- bs4InfoBox(
    title = "Infobox", 
    value = 4,
    icon = "cogs"
  )
  infoBoxIconCl <- getCardChildren(infoBoxTag)[[1]]$attribs$class
  expect_match(infoBoxIconCl, "info-box-icon")
  
  infoBoxTag <- bs4InfoBox(
    title = "Infobox",
    value = 4,
    icon = "cogs",
    iconStatus = "primary"
  )
  infoBoxIconCl <- getCardChildren(infoBoxTag)[[1]]$attribs$class
  expect_match(infoBoxIconCl, "info-box-icon bg-primary")
  
  infoBoxTag <- bs4InfoBox(
    title = "Infobox",
    value = 4,
    icon = "cogs",
    iconStatus = "primary",
    iconElevation = 3
  )
  infoBoxIconCl <- getCardChildren(infoBoxTag)[[1]]$attribs$class
  expect_match(infoBoxIconCl, "info-box-icon bg-primary elevation-3")
})

test_that("tabName", {
  # if no icon is provided, specifiying a tabName does not make sense
  # since it requires to click on the icon tag...
  expect_error(bs4InfoBox(title = "Infobox", tabName = "pouet"))
  
  infoBoxTag <- bs4InfoBox(
    title = "Infobox",
    value = 4,
    icon = "cogs",
    tabName = "pouic"
  )
  infoBoxId <- getCardChildren(infoBoxTag)[[1]]$attribs$id
  expect_true(!is.null(infoBoxId))
  
  infoBoxTag <- bs4InfoBox(
    title = "Infobox", 
    value = 4,
    icon = "cogs"
  )
  infoBoxId <- getCardChildren(infoBoxTag)[[1]]$attribs$id
  expect_true(is.null(infoBoxId))
})

test_that("body structure", {
  infoBoxTag <- bs4InfoBox(
    title = "Infobox", 
    value = 4,
    icon = "cogs"
  )
  infoBoxBodyChildren <- getCardBodyChildren(infoBoxTag)
  expect_length(infoBoxBodyChildren, 2)
  
  infoBoxTag <- bs4InfoBox(
    title = "Infobox", 
    value = 4,
    icon = "cogs",
    shiny::p("Extra element")
  )
  infoBoxBodyChildren <- getCardBodyChildren(infoBoxTag)
  expect_length(infoBoxBodyChildren, 3)
})

test_that("width", {
  expect_error(bs4InfoBox(title = "Infobox", value = 4, width = -1))
  expect_error(bs4InfoBox(title = "Infobox", value = 4, width = 13))
  expect_error(bs4InfoBox(title = "Infobox", value = 4, width = "2"))
  
  infoBoxTag <- bs4InfoBox(title = "Infobox", value = 4, width = 6)
  infoBoxWrapperCl <- infoBoxTag$attribs$class
  expect_match(infoBoxWrapperCl, "col-sm-6")
})
