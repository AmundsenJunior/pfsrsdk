#' @author Scott Russell scott.russell@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description Tests of coreAPI object creation
#'

context("Tests for coreAPI")

testthat::setup({
  Sys.setenv(SEMVER = "1.2.3", API_USERNAME = "envvarusername")
})

testapi <- coreAPI("test_environment/Auth-Template.json")

test_that("coreAPI sets object configuration from JSON configuration value despite presence of environment variable", {
  expect_match(testapi$username, "USER")
})

test_that("coreAPI sets object configuration NULL when JSON configuration value is ''", {
  expect_that(is.null(testapi$context), equals(TRUE))
})

test_that("coreAPI sets object configuration to environment variable when JSON configuration value is empty", {
  expect_match(testapi$semVer, Sys.getenv("SEMVER"))
})

testthat::teardown({
  Sys.unsetenv("SEMVER")
  Sys.unsetenv("API_USERNAME")
})
