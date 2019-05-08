#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{updateExperimentSampleData} Tests for updateExperimentSampleData.

context("Tests for updateExperimentSampleData")

test_that(paste("test updateExperimentSampleData() on: ", env$auth), {
  result <- updateExperimentSampleData(
    con$coreApi,
    data$experimentAssayUpdateAssayType,
    data$experimentAssayUpdateSampleBarcode,
    data$experimentAssayUpdateAttrList,
    useVerbose = verbose
  )

  expect_equal(result$response$status_code, 200)

  expect_equal(
    result$entity[[names(data$experimentAssayUpdateAttrList)[1]]],
    data$experimentAssayUpdateAttrList[[1]]
  )
})
