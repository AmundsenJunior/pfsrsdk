
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description Tests for attachFile.
#'
context("Tests for attachFile()")

lapply(environments, function(x) {
  con <- Connect(x)
  
  barcode <- CoreAPIV2::getEntityByName(con$coreApi, TESTPOCOTYPE, TESTPOCONAME, FALSE, FALSE)$entity[[1]]$Barcode
  filePath <- tempfile(fileext = ".csv")
  write.csv(x = runif(n = 1000), file = filePath)
  
  test_that(paste("test attachFile() OData call on:", x), {
    attachedFile <- CoreAPIV2::attachFile(
      coreApi = con$coreApi,
      entityType = TESTPOCOTYPE,
      barcode = barcode,
      filePath = filePath,
      targetAttributeName = TESTPOCOFILEATTRNAME
    )

    expect_equivalent(attachedFile$response$status_code, 204, all = verbose)
    expect_null(attachedFile$response$entity)
  })

  test_that(paste("test attachFile() CoreSDK call on:", x), {
    skip("SDK command 'file-attach' fails on CI envs. See RSDK-80")
    
    attachedFile <- CoreAPIV2::attachFile(
      coreApi = con$coreApi,
      entityType = TESTPOCOTYPE,
      barcode = barcode,
      filePath = filePath
    )

    expect_equivalent(attachedFile$response$status_code, 200, all = verbose)
    expect_true(attachedFile$entity$response$success)
  })

  CoreAPIV2::logOut(con$coreApi)
})
