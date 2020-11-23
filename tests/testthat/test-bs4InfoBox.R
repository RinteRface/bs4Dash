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
  golem::expect_shinytag(bs4InfoBox(title = "Infobox", color = "danger", value = 4))
})

test_that("overall structure", {
  infoBoxTag <- bs4InfoBox(title = "Infobox", value = 4)
  infoBoxChildren <- getCardChildren(infoBoxTag)
  expect_length(infoBoxChildren, 2)
  
  expect_error(bs4InfoBox(title = "Infobox", value = 4, icon = NULL))
})

test_that("css class", {
  expect_error(
    bs4InfoBox(
      title = "Infobox", 
      value = 4,
      color = NULL, 
      gradient = TRUE,
      fill = TRUE,
    )
  )
  
  expect_error(
    bs4InfoBox(
      title = "Infobox", 
      value = 4,
      color = NULL, 
      fill = TRUE
    )
  )
  
  expect_error(
    bs4InfoBox(
      title = "Infobox", 
      value = 4,
      color = NULL, 
      gradient = TRUE
    )
  )
  
  infoBoxTag <- bs4InfoBox(title = "Infobox", value = 4)
  infoBoxCl <- getCardCl(infoBoxTag)
  expect_match(infoBoxCl, "info-box")
  
  # class is only applied to icon, not the card when fill is FALSE
  infoBoxTag <- bs4InfoBox(title = "Infobox", color = "danger", value = 4)
  infoBoxCl <- getCardCl(infoBoxTag)
  expect_match(infoBoxCl, "info-box")
  iconCl <- infoBoxTag$children[[1]][[2]]$children[[1]]$attribs$class
  expect_match(iconCl, "info-box-icon bg-danger")
  
  infoBoxTag <- bs4InfoBox(title = "Infobox", value = 4, gradient = TRUE, fill = TRUE, color = "danger")
  infoBoxCl <- getCardCl(infoBoxTag)
  expect_match(infoBoxCl, "info-box bg-gradient-danger")
  
  infoBoxTag <- bs4InfoBox(title = "Infobox", value = 4, fill = TRUE, color = "danger")
  infoBoxCl <- getCardCl(infoBoxTag)
  expect_match(infoBoxCl, "info-box bg-danger")
})

test_that("elevation", {
  infoBoxTag <- bs4InfoBox(title = "Infobox", value = 4, elevation = 4)
  infoBoxCl <- getCardCl(infoBoxTag)
  expect_match(infoBoxCl, "elevation-4")
  
  expect_error(bs4InfoBox(title = "Infobox", value = 4, elevation = 6))
  expect_error(bs4InfoBox(title = "Infobox", value = 4, elevation = -1))
  expect_error(bs4InfoBox(title = "Infobox", value = 4, elevation = "2"))
})

test_that("icon elevation", {
  infoBoxTag <- bs4InfoBox(
    title = "Infobox",
    value = 4,
    icon = shiny::icon("cogs"),
    iconElevation = 3
  )
  infoBoxIconCl <- getCardChildren(infoBoxTag)[[1]]$attribs$class
  expect_match(infoBoxIconCl, "info-box-icon elevation-3")
})

test_that("tabName", {
  
  infoBoxTag <- bs4InfoBox(
    title = "Infobox", 
    value = 4,
    icon = shiny::icon("cogs")
  )
  infoBoxId <- getCardChildren(infoBoxTag)[[1]]$attribs$id
  expect_true(is.null(infoBoxId))
  
  infoBoxTag <- bs4InfoBox(
    title = "Infobox",
    value = 4,
    icon = shiny::icon("cogs"),
    tabName = "pouic"
  )
  infoBoxId <- getCardChildren(infoBoxTag)[[1]]$attribs$id
  expect_true(!is.null(infoBoxId))
  
})

test_that("body structure", {
  infoBoxTag <- bs4InfoBox(
    title = "Infobox", 
    value = 4,
    icon = shiny::icon("cogs")
  )
  infoBoxBodyChildren <- getCardBodyChildren(infoBoxTag)
  expect_length(infoBoxBodyChildren, 2)
  
  infoBoxTag <- bs4InfoBox(
    title = "Infobox", 
    value = 4,
    icon = shiny::icon("cogs"),
    subtitle = shiny::p("Extra element")
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
