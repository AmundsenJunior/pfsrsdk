
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description Tests for attachFile.
#'
context("Tests for attachFile()")

lapply(environments, function(x) {
  con <- Connect(x)
  barcode <- CoreAPIV2::getEntityByName(con$coreApi, TESTPOCO, POCO60NAME, FALSE, FALSE)$entity[[1]]$Barcode
  
  test_that(paste("test attachFile() OData call on: ", x), {
    
    # Create a file to use
    filePath <- tempfile(fileext = ".csv")
    write.csv(x = runif(n = 1000), file = filePath)

    attachedFile <- CoreAPIV2::attachFile(
      coreApi = con$coreApi,
      entityType = TESTPOCO,
      barcode = barcode,
      filePath = filePath,
      targetAttributeName = TESTPOCOFILEATTRNAME
    )

    expect_equivalent(attachedFile$response$status_code, 204, all = verbose)
    expect_null(attachedFile$response$entity)
  })

  test_that(paste("test attachFile() CoreSDK call on: ", x), {
    attachedFile <- NULL
    attachedFile <- CoreAPIV2::attachFile(
      coreApi = con$coreApi,
      entityType = TESTPOCO,
      barcode = barcode,
      filePath = filePath
    )

    expect_equivalent(attachedFile$response$status_code, 200, all = verbose)
    expect_true(attachedFile$entity$response$success)
  })

  CoreAPIV2::logOut(con$coreApi)
})
