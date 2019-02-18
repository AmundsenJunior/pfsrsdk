#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code Tests for getExperimentSamplesAssayFileData

context("Tests for getExperimentSamplesAssayFileData")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test getExperimentSamplesAssayFileData() on:", env$auth), {
  result <- CoreAPIV2::getExperimentSamplesAssayFileData(con$coreApi,
    data$experimentFileAssayType,
    data$experimentSampleBarcodeWithFileAttr,
    data$fileAttrName,
    useVerbose = verbose
  )
  expect_equal(result$response$status_code, 200)

  expect_gt(length(result$entity), 0)
})
