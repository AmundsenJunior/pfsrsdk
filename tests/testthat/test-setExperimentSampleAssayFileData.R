#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for setExperimentSampleAssayFileData

context("Tests for setExperimentSampleAssayFileData")

# Completed regression for 5.3.8 and 6.0.1

filePath <- tempfile(fileext = ".csv")
write.csv(x = runif(n = 1000), file = filePath)

test_that(paste("test setExperimentSampleAssayFileData() on:", env$auth), {
  result <- setExperimentSampleAssayFileData(con$coreApi,
    data$experimentFileAssayType,
    data$experimentSampleBarcodeWithFileAttr,
    data$fileAttrName,
    filePath,
    useVerbose = verbose
  )

  expect_equal(result$response$status_code, 204)
  expect_null(result$response$entity)
})
