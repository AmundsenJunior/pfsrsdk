#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @description \code Tests for logOut

context("Tests for logOut")

test_that(paste("test logOut() on:", env$auth), {
  expect_equivalent(logOut(con$coreApi)$success, "Success")
})
