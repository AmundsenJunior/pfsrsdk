#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getEntityAssociations.

context("Tests for getEntityAssociations")

test_that(paste("test getEntityAssociations() on:", env$auth), {
  assoc <- getEntityByName(con$coreApi, data$testPocoGetAssocType, data$testPocoGetAssocName, FALSE, FALSE)
  poco <- getEntityByName(con$coreApi, data$testPocoType, data$testPocoName, FALSE, FALSE)

  as <- getEntityAssociations(con$coreApi, data$testPocoType, poco$entity[[1]]$Barcode, associationContext = data$testPocoGetAssocContext, fullMetadata = TRUE, useVerbose = verbose)

  expect_equal(as$response$status_code, 200)
  expect_match(as$entity[[1]]$Barcode, assoc$entity[[1]]$Barcode)
})
