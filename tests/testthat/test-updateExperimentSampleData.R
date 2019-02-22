#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{updateExperimentSampleData} Tests for updateExperimentSampleData.

context("Tests for updateExperimentSampleData")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test updateExperimentSampleData() on: ", env$auth), {
  skip("SDK command 'updateExperimentSampleData' fails on CI envs. See RSDK-100")

  result <- updateExperimentSampleData(con$coreApi, data$experimentAssayType, data$experimentSampleBarcode, jsonlite::unbox(data$experimentAssayUpdateAttrList), useVerbose = verbose)

  expect_equal(result$response$status_code, 200)

  expect_equal(result$entity[[names(data$experimentAssayUpdateAttrList)[1]]], data$experimentAssayUpdateAttrList[[1]])
})
