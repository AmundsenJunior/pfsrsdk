#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getEntityAssociations.

context("Tests for getEntityAssociations")

test_that(paste("test getEntityAssociations() on:", env$auth), {
  assoc <- getEntityByName(con$coreApi, data$testPocoGetAssocType, data$testPocoGetAssocName, FALSE, useVerbose = FALSE)
  poco <- getEntityByName(con$coreApi, data$testPocoType, data$testPocoName, FALSE, useVerbose = FALSE)

  as <- getEntityAssociations(con$coreApi, data$testPocoType, poco$entity[[1]]$Barcode, associationContext = data$testPocoGetAssocContext, fullMetadata = FALSE, useVerbose = verbose)

  expect_equal(as$response$status_code, 200)
  expect_match(as$entity[[1]]$Barcode, assoc$entity[[1]]$Barcode)
})

test_that(paste("getEntityAssociations returns successful with fullMetadata on:", env$auth), {
  poco <- getEntityByName(con$coreApi, data$testPocoType, data$testPocoName, TRUE, useVerbose = FALSE)
  as <- getEntityAssociations(con$coreApi, data$testPocoType, poco$entity[[1]]$Barcode, associationContext = data$testPocoGetAssocContext, fullMetadata = TRUE, useVerbose = verbose)

  expect_true(!is.null(as$entity[[1]]$`Id@odata.type`))
})
