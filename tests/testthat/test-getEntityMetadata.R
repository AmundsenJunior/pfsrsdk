#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getEntityMetadata().
context("Tests for getEntityMetadata")

# Completed regression for 5.3.8 and 6.0.1

lapply(environments, function(x) {
  con <- Connect(x)
  
  test_that(paste("test getEntityMetadata() on: ", x), {
    out <- CoreAPIV2::getEntityMetadata(con$coreApi, TESTPOCOTYPE, useVerbose = FALSE)

    # Check to verify that the function returned a list. dependending on tenant this list may be empty
    expect_equivalent(class(out$template), "list", all = verbose)
  })

  CoreAPIV2::logOut(con$coreApi)
})
