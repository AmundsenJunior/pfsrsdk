#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Francisco Marin francisco.marin@thermofisher.com
#' @description test apiPOST function.

context("Tests for apiPOST")

header <- c("Content-Type" = "application/json", "If-Match" = "*")

test_that(paste0("apiPOST will create an entity on semantic version: ", con$coreApi$semVer), {
  res <- apiPOST(
    coreApi = con$coreApi,
    resource = data$persistentEntityType,
    body = "{}",
    encode = "raw",
    headers = header,
    useVerbose = verbose
  )
  expect_equal(res$response$status_code, 201)
})

test_that(paste("apiPOST returns an error message on non-existing entity on semantic version:", con$coreApi$semVer), {
  expect_warning(
    {
      response <- apiPOST(
        coreApi = con$coreApi, resource = data$nonExistingEntityType, body = list(), encode = "json",
        headers = NULL,
        special = NULL,
        useVerbose = FALSE
      )
      response$error$message
    },
    data$nonExistingEntityErrorMessage
  )
})

test_that(paste0("apiPOST return object contains only content on semantic version:", con$coreApi$semVer), {
  res <- apiPOST(
    coreApi = con$coreApi,
    resource = data$persistentEntityType,
    body = "{}",
    encode = "raw",
    headers = header,
    useVerbose = verbose,
    fullReturn = FALSE
  )
  expect_null(res$response)
})

test_that(paste("apiPOST returns error when fullReturn is FALSE on semantic version:", con$coreApi$semVer), {
  expect_warning(
    {
      response <- apiPOST(
        coreApi = con$coreApi, resource = data$nonExistingEntityType, body = list(), encode = "json",
        headers = NULL,
        special = NULL,
        useVerbose = FALSE,
        fullReturn = FALSE
      )
      response$error$message
    },
    data$nonExistingEntityErrorMessage
  )
})

teardown({
  header <- NULL
})
