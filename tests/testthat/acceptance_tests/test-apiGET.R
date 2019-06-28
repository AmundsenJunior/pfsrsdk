#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Francisco Marin francisco.marin@thermofisher.com
#' @description test the apiGET function.

context("Tests for apiGET")

header <- c("Accept" = "application/json")
query <- "?count=true"

test_that(paste("apiGET will return an entity on semantic version:", con$coreApi$semVer), {
  res <- apiGET(con$coreApi, resource = data$persistentEntityType, query = "", headers = header, useVerbose = verbose)
  expect_equal(res$response$status_code, 200)
})

test_that(paste("apiGET will return an entity with more than 100 results on semantic version:", con$coreApi$semVer), {
  # not verbose and query
  res <- apiGET(con$coreApi, resource = data$sampleType, query = query, headers = header, useVerbose = FALSE)
  expect_equal(res$response$status_code, 200)
  expect_gt(length(res$content), 100)
  # verbose and query
  res <- apiGET(con$coreApi, resource = data$sampleType, query = query, headers = header, useVerbose = TRUE)
  expect_equal(res$response$status_code, 200)
  expect_gt(length(res$content), 100)
  # verbose and no query
  res <- apiGET(con$coreApi, resource = data$sampleType, query = "", headers = header, useVerbose = TRUE)
  expect_equal(res$response$status_code, 200)
  expect_gt(length(res$content), 100)
  # not verbose and no query
  res <- apiGET(con$coreApi, resource = data$sampleType, query = "", headers = header, useVerbose = FALSE)
  expect_equal(res$response$status_code, 200)
  expect_gt(length(res$content), 100)
})

test_that(paste("apiGET returns an error message on non-existing entity on semantic version:", con$coreApi$semVer), {
  expect_warning(
    {
      response <- apiGET(
        coreApi = con$coreApi,
        resource = paste0(data$nonExistingEntityType, "('", data$nonExistingEntityBarcode, "')"),
        query = "",
        useVerbose = verbose
      )
      response$error$message
    },
    data$nonExistingEntityErrorMessage
  )
})

test_that(paste("apiGET return object contains only content on semantic version:", con$coreApi$semVer), {
  res <- apiGET(con$coreApi, resource = data$persistentEntityType, query = "", headers = header, useVerbose = verbose, fullReturn = FALSE)
  expect_null(res$response)
})

test_that(paste("apiGET returns error when fullReturn is FALSE on semantic version:", con$coreApi$semVer), {
  expect_warning(
    {
      response <- apiGET(
        coreApi = con$coreApi,
        resource = paste0(data$nonExistingEntityType, "('", data$nonExistingEntityBarcode, "')"),
        query = "",
        useVerbose = verbose
      )
      response$error$message
    },
    data$nonExistingEntityErrorMessage
  )
})

teardown({
  header <- NULL
  query <- NULL
})
