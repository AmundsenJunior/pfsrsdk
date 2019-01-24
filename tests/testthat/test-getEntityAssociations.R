#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getEntityAssociations.

context("Tests for getEntityAssociations")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test getEntityAssociations() on:", env$auth), {
  assoc <- CoreAPIV2::getEntityByName(con$coreApi, data$testPocoGetAssocType, data$testPocoGetAssocName, FALSE, FALSE)
  poco <- CoreAPIV2::getEntityByName(con$coreApi, data$testPocoType, data$testPocoName, FALSE, FALSE)
    
  as <- CoreAPIV2::getEntityAssociations(con$coreApi, data$testPocoType, poco$entity[[1]]$Barcode, associationContext = data$testPocoGetAssocContext, fullMetadata = TRUE, useVerbose = verbose)
    
  expect_equal(as$response$status_code, 200)
  expect_match(as$entity[[1]]$Barcode, assoc$entity[[1]]$Barcode)
})
