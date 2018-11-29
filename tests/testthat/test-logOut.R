#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code Tests for logOut

context("Tests for logOut")

# Completed regression for 5.3.8 and 6.0.1

lapply(environments, function(x) {
  
  con <- Connect(x)
  test_that(paste("test logOut() on: ", x), {
    expect_true(CoreAPIV2::logOut(con$coreApi)$success)
  })
  
})

