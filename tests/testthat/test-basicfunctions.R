
#' @author Adam Wheeler ajwtech@gmail.com
#' @description test all the basic api functions.

context("test-basicfunctions")

# Completed regression for 5.3.8 and 6.0.0

lapply(environments, function(x) {
  con <- Connect(x)

  test_that(paste0("apiGET will return a poco on: ", x), {
    header <- c("Accept" = "application/json")
    print(paste0("poco to get is: ", TESTPOCO))

    res <- apiGET(con$coreApi, resource = TESTPOCO, query = "", headers = header, useVerbose = verbose)
    expect_equal(res$response$status_code, 200)
  })

  test_that(paste0("apiPOST will create a poco on: ", x), {
    header <- c("Content-Type" = "application/json", "If-Match" = "*")
    print(paste0("poco to create is: ", TESTPOCO))
    res <<- apiPOST(con$coreApi, resource = TESTPOCO, body = "{}", encode = "raw", headers = header, useVerbose = verbose)
    expect_equal(res$all_headers[[1]]$status, 201)
    options(res = res)
  })

  test_that(paste0("apiPUT will update a poco on: ", x), {
    header <- c("Content-Type" = "application/json", "If-Match" = "*")
    res <- getOption("res")
    content <- httr::content(res, as = "parsed")
    content["Name"] <- paste0("TEST from Odata: ", content$Barcode)
    body <- content[-1]
    res <- CoreAPIV2::apiPUT(con$coreApi, resource = TESTPOCO, query = paste0("('", content$Barcode, "')"), body, encode = "raw", headers = header, useVerbose = FALSE, unbox = TRUE)
    expect_equal(res$all_headers[[1]]$status, 200)
  })

  CoreAPIV2::logOut(con$coreApi)
})
