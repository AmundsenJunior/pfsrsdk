#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description Tests for attachFile.
#'
context("Tests for attachFile()")

barcode <- getEntityByName(con$coreApi, data$testPocoType, data$testPocoName, FALSE, FALSE)$entity[[1]]$Barcode
filePath <- tempfile(fileext = ".csv")
write.csv(x = runif(n = 1000), file = filePath)

test_that(paste("test attachFile() OData call on:", env$auth), {
  attachedFile <- attachFile(
    coreApi = con$coreApi,
    entityType = data$testPocoType,
    barcode = barcode,
    filePath = filePath,
    targetAttributeName = data$testPocoFileAttrName
  )

  expect_equivalent(attachedFile$response$status_code, 204, all = verbose)
  expect_null(attachedFile$response$entity)
})

test_that(paste("test attachFile() CoreSDK call on:", env$auth), {
  attachedFile <- attachFile(
    coreApi = con$coreApi,
    entityType = data$testPocoType,
    barcode = barcode,
    filePath = filePath
  )

  expect_equivalent(attachedFile$response$status_code, 200, all = verbose)
  expect_true(attachedFile$entity$response$success)
})

teardown({
  barcode <- NULL
  unlink(filePath)
})
