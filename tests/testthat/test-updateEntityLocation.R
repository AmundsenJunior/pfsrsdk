#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for updateEntityLocation
context("Tests for updateEntityLocation")

# Completed regression for 5.3.8 and 6.0.1

cat(paste0("\n environments:\n", environments, "\n"))

lapply(environments, function(x) {
  con <- Connect(x)
  
  test_that(paste("test updateEntityLocation for:", x), {
    barcode <- CoreAPIV2::getEntityByName(con$coreApi, TESTPOCO, POCO60NAME, useVerbose = verbose)$entity[[1]]$Barcode
    
    updateLoc <- CoreAPIV2::updateEntityLocation(con$coreApi, TESTPOCO, barcode, POCO60LOC2, useVerbose = FALSE)
    expect_equivalent(httr::status_code(updateLoc$response), 200)
    
    loc <- CoreAPIV2::getEntityLocation(con$coreApi, TESTPOCO, barcode, useVerbose = FALSE)
    expect_match(POCO60LOC2, loc$entity[[1]]$Barcode)
  })
  
  CoreAPIV2::logOut(con$coreApi)
})
