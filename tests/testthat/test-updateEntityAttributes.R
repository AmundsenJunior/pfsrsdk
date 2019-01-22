
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{updateEntityAttributes} Tests for updateEntityAttributes.


context("Tests for updateEntityAttributes")

# Completed regression for 5.3.8 and 6.0.1

lapply(environments, function(x) {
  con <- Connect(x)
  
  test_that(paste("test updateEntityAttributes() on: ", x), {
    barcode <- CoreAPIV2::getEntityByName(con$coreApi, TESTPOCOUPDATETYPE, TESTPOCOUPDATENAME, FALSE, FALSE)$entity[[1]]$Barcode
    
    ue <- CoreAPIV2::updateEntityAttributes(con$coreApi, TESTPOCOUPDATETYPE, barcode, TESTPOCOUPDATEATTRLIST, useVerbose = FALSE)
    expect_equal(ue$entity[[names(TESTPOCOUPDATEATTRLIST)[1]]], TESTPOCOUPDATEATTRLIST[[names(TESTPOCOUPDATEATTRLIST)[1]]], all = verbose)
    expect_equal(ue$entity[[names(TESTPOCOUPDATEATTRLIST)[2]]], TESTPOCOUPDATEATTRLIST[[names(TESTPOCOUPDATEATTRLIST)[2]]], all = verbose)
  })

  CoreAPIV2::logOut(con$coreApi)
})
