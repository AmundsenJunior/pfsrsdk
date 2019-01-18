
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description test all the basic api functions.

context("test-httpFunctions")

# Completed regression for 5.3.8 and 6.0.1
lapply(environments, function(x) {
  con <- Connect(x)

  test_that(paste0("apiGET will return an entity on: ", x), {
    header <- c("Accept" = "application/json")
    print(paste0("Entity to get is: ", PERSISTENTBARCODEENTITY))
    res <- CoreAPIV2::apiGET(con$coreApi, resource = PERSISTENTBARCODEENTITY, query = "", headers = header, useVerbose = verbose)
    expect_equal(res$response$status_code, 200)
  })

  test_that(paste0("apiPOST will create an entity on: ", x), {
    header <- c("Content-Type" = "application/json", "If-Match" = "*")
    print(paste0("Entity to create is: ", PERSISTENTBARCODEENTITY))
    res <<- CoreAPIV2::apiPOST(con$coreApi, resource = PERSISTENTBARCODEENTITY, body = "{}", encode = "raw", headers = header, useVerbose = verbose)
    expect_equal(res$all_headers[[1]]$status, 201)
    options(res = res)
  })

  test_that(paste0("apiPUT will update an entity on: ", x), {
    header <- c("Content-Type" = "application/json", "If-Match" = "*")
    res <- getOption("res")
    content <- httr::content(res, as = "parsed")
    content["Name"] <- paste0("TEST from Odata: ", content$Barcode)
    body <- content[-1]
    res <- CoreAPIV2::apiPUT(con$coreApi, resource = PERSISTENTBARCODEENTITY, query = paste0("('", content$Barcode, "')"), body, encode = "raw", headers = header, useVerbose = FALSE, unbox = TRUE)
    expect_equal(res$all_headers[[1]]$status, 200)
  })

  CoreAPIV2::logOut(con$coreApi)
})
