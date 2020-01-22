context("bs4CardSidebar")

getButtonAttr <- function(sidebarCard){
  sidebarCard[[2]]$attribs
}

test_that("structure", {
  sidebarTag <- bs4CardSidebar()
  golem::expect_shinytaglist(sidebarTag)
  expect_length(sidebarTag, 3)
  expect_match(sidebarTag[[1]]$name, "head")
  expect_match(sidebarTag[[2]]$attribs$class, "btn btn-tool")
  expect_match(sidebarTag[[3]]$attribs$class, "direct-chat-contacts")
})

test_that("start open", {
  sidebarTag <- bs4CardSidebar(startOpen = TRUE)
  startOpen <- getButtonAttr(sidebarTag)$`data-start-open`
  expect_match(startOpen, "true")
})
