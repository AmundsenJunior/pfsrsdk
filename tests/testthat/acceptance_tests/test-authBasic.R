#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Francisco Marin francisco.marin@thermofisher.com
#' @description Tests for basic authentication.
#'
context("Tests for authBasic")

test_that(paste("test successful login with populated JSESSIONID on semantic version:", con$coreApi$semVer), {
  expect_that(is.null(con$coreApi$jsessionId), equals(FALSE))
})

test_that(paste("test Updating Metadata of semantic version:", con$coreApi$semVer), {
  metadata <- updateMetadata(con$coreApi, useVerbose = TRUE)
  expect_match(httr::http_status(metadata$response)$category, "Success")
})

test_that(paste("test logout of semantic version:", con$coreApi$semVer), {
  logout <- logOut(con$coreApi, useVerbose = verbose)
  expect_match(logout$success, "Success")
})

test_that(paste("single account with bad password returns error on semantic version:", con$coreApi$semVer), {
  verbose <- FALSE
  api <- coreAPI(env$auth)
  bapi <- api
  bapi$password <- "badpassword"
  expect_warning(authBasic(bapi, useVerbose = verbose))
})
