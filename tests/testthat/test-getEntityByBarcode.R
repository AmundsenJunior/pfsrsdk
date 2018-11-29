
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code Tests for getEntityAssociations.
  
context("Tests for getEntityByBarcode")

# Completed regression for 5.3.8 and 6.0.1

lapply(environments, function(x) {
  
  con <- Connect(x)
  test_that(paste("test getEntityByBarcode() on: ", x), {
    b <- CoreAPIV2::getEntityByBarcode(con$coreApi, PERSISTENTBARCODEENTITY, PERSISTENTBARCODE, 
                                       useVerbose = verbose)$entity
    expect_match(b$Barcode, PERSISTENTBARCODE, all = verbose)
    
  })
  
  CoreAPIV2::logOut(con$coreApi)
})
