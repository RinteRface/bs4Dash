context("bs4CardLabel")

test_that("is shiny tag?", {
  golem::expect_shinytag(bs4CardLabel(text = 1, status = "warning"))
})

test_that("Long text warning", {
  expect_warning(bs4CardLabel(text = "jssjjsjssjsjsj", status = "danger"))
})

test_that("basis", {
  expect_error(bs4CardLabel(text = 1))
})

test_that("status", {
  labelCl <- bs4CardLabel(text = 1, status = "danger")$attribs$class
  expect_match(labelCl, "badge bg-danger")
})

test_that("Is wrapper tag span?", {
  wrapperType <- bs4CardLabel(text = 1, status = "primary")$name
  expect_match(wrapperType, "span")
})

test_that("no tooltip", {
  labelTagChildren <- bs4CardLabel(text = 1, status = "primary")$children
  expect_length(labelTagChildren, 1)
})

test_that("tooltip", {
  labelTagProps <- bs4CardLabel(text = 1, tooltip = "prout", status = "primary")$attribs
  expect_length(labelTagProps, 3)
})