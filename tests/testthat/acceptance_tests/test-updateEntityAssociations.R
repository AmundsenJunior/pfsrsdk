#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description Tests for entity associations.

context("Tests for updateEntityAssociations")

test_that(paste("test updateEntityAssociations() on semantic version:", con$coreApi$semVer), {
  assoc <- getEntityByName(con$coreApi, data$testPocoUpdateAssocType, data$testPocoUpdateAssocName, FALSE, useVerbose = FALSE)
  poco <- getEntityByName(con$coreApi, data$testPocoUpdateType, data$testPocoUpdateName, FALSE, useVerbose = FALSE)

  updateValues <- list()
  updateValues[[data$testPocoUpdateAssocContext]] <- c(data$testPocoUpdateAssocType, assoc$entity[[1]]$Barcode)

  us <- updateEntityAssociations(con$coreApi, data$testPocoUpdateType, poco$entity[[1]]$Barcode, updateValues, useVerbose = verbose)
  expect_equivalent(httr::status_code(us$response), 200)

  as <- getEntityAssociations(con$coreApi, data$testPocoUpdateType, poco$entity[[1]]$Barcode, associationContext = data$testPocoUpdateAssocContext, fullMetadata = TRUE, useVerbose = verbose)
  expect_match(as$entity[[1]]$Barcode, assoc$entity[[1]]$Barcode)
})
