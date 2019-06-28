#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for setCellContents

context("Tests for setCellContents")

test_that(paste("test setCellContents() on semantic version:", con$coreApi$semVer), {
  result <- setCellContents(con$coreApi,
    data$nonExperimentContainerType,
    data$nonExperimentContainerBarcode,
    data$setCellContentsContainerCellId,
    data$sampleLotType,
    data$sampleLotBarcode,
    data$wellContentsAmount,
    data$wellContentsAmountUnit,
    data$wellContentsConcentration,
    data$wellContentsConcentrationUnit,
    useVerbose = verbose
  )

  expect_equal(result$response$status_code, 200)

  expect_equal(result$entity$Barcode, data$nonExperimentContainerBarcode)
})
