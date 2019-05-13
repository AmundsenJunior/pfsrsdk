#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description Tests for basic authentication.
#'
context("Tests for authentication")

test_that(paste("test login parameters for environment and updating metadata", env$auth), {
  expect_that(is.null(con$coreApi$jsessionId), equals(FALSE))
})

test_that(paste("test Updating Metadata of:", env$auth), {
  metadata <- updateMetadata(con$coreApi, useVerbose = TRUE)
  expect_match(httr::http_status(metadata$response)$category, "Success")
})

test_that(paste("test logout of:", env$auth), {
  logout <- logOut(con$coreApi, useVerbose = verbose)
  expect_match(logout$success, "Success")
})

test_that(paste("single account with bad password returns error on:", env$auth), {
  verbose <- FALSE
  api <- coreAPI(env$auth)
  bapi <- api
  bapi$password <- "badpassword"
  expect_error(authBasic(bapi, useVerbose = verbose))
})
