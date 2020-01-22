context("bs4InfoBox")

getCardCl <- function(card) {
  card$children[[1]][[2]]$attribs$class
}

test_that("css class", {
  expect_error(
    bs4InfoBox(
      title = "Infobox", 
      status = "danger", 
      gradientColor = "warning"
    )
  )
  
  infoBoxTag <- bs4InfoBox(title = "Infobox")
  infoBoxCl <- getCardCl(infoBoxTag)
  expect_match(infoBoxCl, "info-box")
  
  infoBoxTag <- bs4InfoBox(title = "Infobox", status = "danger")
  infoBoxCl <- getCardCl(infoBoxTag)
  expect_match(infoBoxCl, "info-box bg-danger")
  
  infoBoxTag <- bs4InfoBox(title = "Infobox", gradientColor = "danger")
  infoBoxCl <- getCardCl(infoBoxTag)
  expect_match(infoBoxCl, "info-box bg-gradient-danger")
})

test_that("elevation", {
  infoBoxTag <- bs4InfoBox(title = "Infobox", elevation = 4)
  infoBoxCl <- getCardCl(infoBoxTag)
  expect_match(infoBoxCl, "elevation-4")
  
  expect_error(bs4InfoBox(title = "Infobox", elevation = 6))
  expect_error(bs4InfoBox(title = "Infobox", elevation = -1))
  expect_error(bs4InfoBox(title = "Infobox", elevation = "2"))
})