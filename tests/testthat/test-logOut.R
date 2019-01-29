#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code Tests for logOut

context("Tests for logOut")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test logOut() on:", env$auth), {
  expect_equivalent(CoreAPIV2::logOut(con$coreApi)$success, "Success")
})
