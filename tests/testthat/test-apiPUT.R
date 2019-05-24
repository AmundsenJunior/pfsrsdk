#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Francisco Marin francisco.marin@thermofisher.com
#' @description test apiPUT function.

context("Tests for apiPUT")

entity <- apiGET(con$coreApi, resource = paste0(data$persistentEntityType, "('", data$persistentEntityBarcode, "')"), query = "", headers = c("Accept" = "application/json"), useVerbose = verbose)
header <- c("Content-Type" = "application/json", "If-Match" = "*")

test_that(paste0("apiPUT will update an entity on: ", env$auth), {
  content <- entity$content
  content["Name"] <- data$persistentEntityName
  body <- content[-1]
  res <- apiPUT(
    coreApi = con$coreApi,
    resource = data$persistentEntityType,
    query = paste0("('", content$Barcode, "')"),
    body,
    encode = "raw",
    headers = header,
    useVerbose = verbose,
    unbox = TRUE
  )
  expect_equal(res$response$status_code, 200)
  res <- NULL
})

test_that(paste("apiPUT returns an error message on non-existing entity on:", env$auth), {
  expect_warning(
    {
      response <- apiPUT(
        coreApi = con$coreApi,
        resource = paste0(data$nonExistingEntityType, "('", data$nonExistingEntityBarcode, "')"),
        query = NULL,
        body = list(),
        encode = "json",
        headers = NULL,
        special = NULL,
        useVerbose = FALSE,
        unbox = TRUE,
        valueFlag = FALSE
      )
      response$error$message
    },
    data$nonExistingEntityErrorMessage
  )
})

test_that(paste("apiPUT return object contains only content on:", env$auth), {
  content <- entity$content
  content["Name"] <- data$persistentEntityName
  body <- content[-1]
  res <- apiPUT(
    coreApi = con$coreApi,
    resource = data$persistentEntityType,
    query = paste0("('", content$Barcode, "')"),
    body,
    encode = "raw",
    headers = header,
    useVerbose = verbose,
    unbox = TRUE,
    fullReturn = FALSE
  )
  expect_null(res$response)
  res <- NULL
})

test_that(paste("apiPUT returns error when fullReturn is FALSE on:", env$auth), {
  expect_warning(
    {
      response <- apiPUT(
        coreApi = con$coreApi,
        resource = paste0(data$nonExistingEntityType, "('", data$nonExistingEntityBarcode, "')"),
        query = NULL,
        body = list(),
        encode = "json",
        headers = NULL,
        special = NULL,
        useVerbose = FALSE,
        unbox = TRUE,
        valueFlag = FALSE,
        fullReturn = FALSE
      )
      response$error$message
    },
    data$nonExistingErrorMessage
  )
})

teardown({
  header <- NULL
})
