#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code Tests for updatemetadata

context("test-updatemetadata")

lapply(environments, function(x) {
  
  con <- Connect(x)
  test_that(paste("test updatemetadata() on: ", x), {
    meta <- updateMetadata(con$coreApi, useVerbose = verbose)
    expect_equivalent(meta$response$status_code, 200, all = verbose)
  })
  
  CoreAPIV2::logOut(con$coreApi)
})
