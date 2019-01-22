
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description Tests for entity associations.

context("Tests for updateEntityAssociations")

# Completed regression for 5.3.8 and 6.0.1

lapply(environments, function(x) {
  con <- Connect(x)
  
  test_that(paste("test updateEntityAssociations() on:", x), {
    assoc <- CoreAPIV2::getEntityByName(con$coreApi, TESTPOCOUPDATEASSOC, TESTPOCOUPDATEASSOCNAME, FALSE, FALSE)
    poco <- CoreAPIV2::getEntityByName(con$coreApi, TESTPOCOUPDATETYPE, TESTPOCOUPDATENAME, FALSE, FALSE)

    updateValues <- list()
    updateValues[[TESTPOCOUPDATEASSOCCONTEXT]] <- c(TESTPOCOUPDATEASSOC, assoc$entity[[1]]$Barcode)

    us <- CoreAPIV2::updateEntityAssociations(con$coreApi, TESTPOCOUPDATETYPE, poco$entity[[1]]$Barcode, updateValues, useVerbose = FALSE)
    expect_equivalent(httr::status_code(us$response), 200)
    
    as <- CoreAPIV2::getEntityAssociations(con$coreApi, TESTPOCOUPDATETYPE, poco$entity[[1]]$Barcode, associationContext = TESTPOCOUPDATEASSOCCONTEXT, fullMetadata = TRUE, useVerbose = FALSE)
    expect_match(as$entity[[1]]$Barcode, assoc$entity[[1]]$Barcode)
  })

  CoreAPIV2::logOut(con$coreApi)
})
