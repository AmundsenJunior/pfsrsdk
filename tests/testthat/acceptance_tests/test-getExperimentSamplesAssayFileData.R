#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @description \code Tests for getExperimentSamplesAssayFileData

context("Tests for getExperimentSamplesAssayFileData")

test_that(paste("test getExperimentSamplesAssayFileData() on semantic version:", con$coreApi$semVer), {
  result <- getExperimentSamplesAssayFileData(con$coreApi,
    data$experimentFileAssayType,
    data$experimentSampleBarcodeWithFileAttr,
    data$fileAttrName,
    useVerbose = verbose
  )
  expect_equal(result$response$status_code, 200)

  expect_gt(length(result$entity), 0)
})
