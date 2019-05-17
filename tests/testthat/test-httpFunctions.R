#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Francisco Marin francisco.marin@thermofisher.com
#' @description test all the basic api functions.

context("Tests for httpFunctions")

test_that(paste0("apiGET will return an entity on: ", env$auth), {
  header <- c("Accept" = "application/json")
  res <- apiGET(con$coreApi, resource = data$persistentEntityType, query = "", headers = header, useVerbose = verbose)
  expect_equal(res$response$status_code, 200)
})

test_that(paste0("apiGET will return an entity with more than 100 results on: ", env$auth), {
  header <- c("Accept" = "application/json")
  # not verbose and query
  res <- apiGET(con$coreApi, resource = data$sampleType, query = "?$count=true", headers = header, useVerbose = FALSE)
  expect_equal(res$response$status_code, 200)
  expect_gt(length(res$content), 100)
  # verbose and query
  res <- apiGET(con$coreApi, resource = data$sampleType, query = "?$count=true", headers = header, useVerbose = TRUE)
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



test_that(paste0("apiPOST will create an entity on: ", env$auth), {
  header <- c("Content-Type" = "application/json", "If-Match" = "*")
  res <<- apiPOST(con$coreApi, resource = data$persistentEntityType, body = "{}", encode = "raw", headers = header, useVerbose = verbose)
  expect_equal(res$all_headers[[1]]$status, 201)
  options(res = res)
})

test_that(paste0("apiPUT will update an entity on: ", env$auth), {
  header <- c("Content-Type" = "application/json", "If-Match" = "*")
  res <- getOption("res")
  content <- httr::content(res, as = "parsed")
  content["Name"] <- paste0("TEST from Odata: ", content$Barcode)
  body <- content[-1]
  res <- apiPUT(con$coreApi, resource = data$persistentEntityType, query = paste0("('", content$Barcode, "')"), body, encode = "raw", headers = header, useVerbose = verbose, unbox = TRUE)
  expect_equal(res$all_headers[[1]]$status, 200)
  res <<- NULL
})

test_that(paste("test apiGET() on:", env$auth), {
  expect_warning(
    {
      response <- apiGET(
        coreApi = con$coreApi, resource = "NON_EXISTING_ENTITY('NEE1')",
        query = "",
        useVerbose = verbose
      )
      response$error$message
    },
    "Status Code: 404, Error: Cannot find EntitySet, Singleton, ActionImport or FunctionImport with name 'NON_EXISTING_ENTITY'."
  )
})

test_that(paste("test apiPOST() on:", env$auth), {
  expect_warning(
    {
      response <- apiPOST(
        coreApi = con$coreApi, resource = "NON_EXISTING_ENTITY", body = list(), encode = "json",
        headers = NULL,
        special = NULL,
        useVerbose = FALSE
      )
      response$error$message
    },
    "Status Code: 404, Error: Cannot find EntitySet, Singleton, ActionImport or FunctionImport with name 'NON_EXISTING_ENTITY'."
  )
})

test_that(paste("test apiPUT() on:", env$auth), {
  expect_warning(
    {
      response <- apiPUT(
        coreApi = con$coreApi, resource = "NON_EXISTING_ENTITY('NEE1')", query = NULL, body = list(), encode = "json",
        headers = NULL,
        special = NULL,
        useVerbose = FALSE,
        unbox = TRUE,
        valueFlag = FALSE
      )
      response$error$message
    },
    "Status Code: 404, Error: Cannot find EntitySet, Singleton, ActionImport or FunctionImport with name 'NON_EXISTING_ENTITY'."
  )
})
