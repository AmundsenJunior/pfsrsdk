
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code{updateEntityAttributes} Tests for updateEntityAttributes.


context("Tests for updateEntityAttributes")

# Completed regression for 5.3.8 and 6.0.1

lapply(environments, function(x) {
  con <- Connect(x)
  test_that(paste("test updateEntityAttributes() on: ", x), {
    PC60 <- CoreAPIV2::getEntityByName(con$coreApi, TESTPOCO, POCO60NAME, FALSE, FALSE)
    # test update attributes
    timeStamp <- timestamp()
    updateValues <- list(TST_STRING = timeStamp, TST_BOOL = F)
    ue <- CoreAPIV2::updateEntityAttributes(con$coreApi, TESTPOCO, PC60$entity[[1]]$Barcode, updateValues, useVerbose = FALSE)
    expect_match(ue$entity$TST_STRING, timeStamp, all = verbose)
  })

  CoreAPIV2::logOut(con$coreApi)
})
