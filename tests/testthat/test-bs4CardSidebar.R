getButtonAttr <- function(sidebarCard) {
  sidebarCard[[1]]$attribs
}

test_that("structure", {
  sidebarTag <- bs4CardSidebar()
  golem::expect_shinytaglist(sidebarTag)
  expect_length(sidebarTag, 2)
  expect_null(sidebarTag[[1]]$attribs$class)
  expect_match(sidebarTag[[2]]$attribs$class, "direct-chat-contacts")
})

test_that("start open", {
  sidebarTag <- bs4CardSidebar(startOpen = TRUE)
  startOpen <- getButtonAttr(sidebarTag)$`data-start-open`
  expect_match(startOpen, "true")
})
