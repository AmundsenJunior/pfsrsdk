
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description Tests for attachFile.
#'
context("Tests for attachFile()")

# Completed regression for 5.3.8 and 6.0.1

lapply(environments, function(x) {
  con <- Connect(x)
  test_that(paste("test attachFile() on: ", x), {
    # Get a Barcode to use

    barcode <- CoreAPIV2::getEntityByName(con$coreApi, TESTPOCO, POCO60NAME, FALSE, FALSE)$entity[[1]]$Barcode

    # Create a file to use
    filePath <- tempfile(fileext = ".csv")
    write.csv(x = runif(n = 1000), file = filePath)
    fileName <- paste0("testfile", as.character(floor(runif(1, 1, 101))), ".txt")

    attachedFile <- attachFile(
      coreApi = con$coreApi,
      entityType = TESTPOCO,
      barcode = barcode,
      filePath = filePath,
      targetAttributeName = TESTPOCOFILEATTRNAME
    )

    expect_equivalent(attachedFile$response$status_code, 204, all = verbose)

    attachedFile <- NULL
    attachedFile <- attachFile(
      coreApi = con$coreApi,
      entityType = TESTPOCO,
      barcode = barcode,
      filePath = filePath
    )

    expect_equivalent(attachedFile$response$status_code, 200, all = verbose)
  })

  CoreAPIV2::logOut(con$coreApi)
})
