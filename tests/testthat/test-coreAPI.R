
#' @author Scott Russell scott.russell@thermofisher.com
#' @description Tests of main class creation
#' 

context("Tests for coreAPI")

lapply(environments, function(x) {
  api <- CoreAPIV2::coreAPI("test_environment/Template.json")
  
  test_that("test coreAPI sets class configuration NULL when JSON configuration value is ''", {
    expect_that(is.null(api$context), equals(TRUE))
  })
  
  test_that("test coreAPI sets class configuration to JSON configuration value", {
    expect_match(api$coreUrl, "HOSTNAME")
  })
})
