
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code Test for basic authentication.
#'
context("Tests for authentication")

# Completed regression for 5.3.8 and 6.0.0

cat(paste0("\n environments:\n", environments, "\n"))

lapply(environments, function(x) {
  con <- Connect(x)
  test_that(paste("test login parameters for environment and updating metadata", x), {
    expect_that(is.null(con$coreApi$jsessionId), equals(FALSE))
  })

  test_that(paste("test Updating MetaData of: ", x), {
    metadata <- CoreAPIV2::updateMetadata(con$coreApi, useVerbose = TRUE)
    print(httr::http_status(metadata$response))
    expect_match(httr::http_status(metadata$response)$category, "Success")
  })
  test_that(paste("test logout of: ", x), {
    logout <- CoreAPIV2::logOut(con$coreApi, useVerbose = verbose)
    expect_match(logout$success, "Success")
  })

  test_that("single account with bad password returns error", {
    verbose <- FALSE
    api <- CoreAPIV2::coreAPI(x)
    bapi <- api
    bapi$pwd <- "badpassword"
    expect_error(CoreAPIV2::authBasic(bapi, useVerbose = verbose))
  })
})
