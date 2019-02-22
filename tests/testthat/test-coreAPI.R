#' @author Scott Russell scott.russell@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description Tests of main class creation
#'

context("Tests for coreAPI")

testthat::setup(Sys.setenv(HOST = "localhost"))

testapi <- coreAPI("test_environment/Auth-Template.json")

test_that("test coreAPI sets class configuration NULL when JSON configuration value is ''", {
  expect_that(is.null(testapi$context), equals(TRUE))
})

test_that("test coreAPI sets class configuration to JSON configuration value", {
  expect_match(testapi$host, Sys.getenv("HOST"))
})

testthat::teardown(Sys.unsetenv("HOST"))
