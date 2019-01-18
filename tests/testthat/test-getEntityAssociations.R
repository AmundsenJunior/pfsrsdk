
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getEntityAssociations.

context("Tests for getEntityAssociations")

# Completed regression for 5.3.8 and 6.0.1

lapply(environments, function(x) {
  con <- Connect(x)
  
  test_that(paste("test getEntityAssociations() on:", x), {
    assoc <- CoreAPIV2::getEntityByName(con$coreApi, TESTPOCOGETASSOCTYPE, TESTPOCOGETASSOCNAME, FALSE, FALSE)
    poco <- CoreAPIV2::getEntityByName(con$coreApi, TESTPOCOTYPE, TESTPOCONAME, FALSE, FALSE)
    
    as <- CoreAPIV2::getEntityAssociations(con$coreApi, TESTPOCOTYPE, poco$entity[[1]]$Barcode, associationContext = TESTPOCOGETASSOCCONTEXT, fullMetadata = TRUE, useVerbose = FALSE)
    
    expect_equal(as$response$status_code, 200)
    expect_match(as$entity[[1]]$Barcode, assoc$entity[[1]]$Barcode)
  })

  CoreAPIV2::logOut(con$coreApi)
})
