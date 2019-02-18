#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for setWellContents

context("Tests for setWellContents")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test setWellContents() on:", env$auth), {
  result <- CoreAPIV2::setWellContents(con$coreApi,
    data$nonExperimentContainerType,
    data$nonExperimentContainerBarcode,
    data$containerWellNum,
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
