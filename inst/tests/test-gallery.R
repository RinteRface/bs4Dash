# This file is for testing the applications in the inst/examples/showcase directory.

library(testthat)

test_that("bs4DashGallery app works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  
  p <- processx::process$new(
    "Rscript", 
    c( "-e",  "options('shiny.port'= 3515);bs4Dash::bs4DashGallery()" )
  )
  
  test <- crrry::CrrryOnPage$new(
    chrome_bin = pagedown::find_chrome(),
    chrome_port = httpuv::randomPort(),
    url = "http://localhost:3515",
    headless = TRUE
  )
  
  # give time for the app to launch
  test$wait_for_shiny_ready()
  
  # access sidebar inputs and click on toggle
  getSidebarValue <- '$("#sidebar").data("shiny-input-binding").getValue($("#sidebar"))'
  sidebar_state_1 <- test$call_js(getSidebarValue)
  Sys.sleep(2)
  test$wait_for_shiny_ready()
  test$call_js('$("#sidebarToggle").click()')
  Sys.sleep(2)
  test$wait_for_shiny_ready()
  sidebar_state_2 <- test$call_js(getSidebarValue)
  Sys.sleep(2)
  test$wait_for_shiny_ready()
  # Sidebar state should be different
  expect_true(sidebar_state_1$result$value != sidebar_state_2$result$value)
  
  test$stop()
  p$kill()
  
  #test$gremlins_horde()
  #Sys.sleep(10)
  #test$wait_for_shiny_ready()
  #test$stop()
})
