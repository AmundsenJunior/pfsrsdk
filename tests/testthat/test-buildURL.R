#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{buildUrl} testing buildURL.

context("Tests for buildURL")

# Completed regression for 5.3.8 and 6.0.1

if (con$coreApi$tenant == "PLATFORM ADMIN") {
  odat <- "/odata/"
} else if (!is.null(con$coreApi$tenant) && is.null(con$coreApi$alias)) {
  odat <- paste0("/", CoreAPIV2::odataCleanName(con$coreApi$tenant), "/odata/")
} else if (!is.null(con$coreApi$alias)) {
  odat <- paste0("/", CoreAPIV2::odataCleanName(con$coreApi$alias), "/odata/")
} else {
  odat <- "/odata/"
}

test_that(paste("Test that buildURL responds correctly when passed a resource and query on:", env$auth), {
  resource <- "res"
  query <- "?test query"
  builtURL <- CoreAPIV2::buildUrl(con$coreApi, resource = resource, query = query)
  pattern <- paste0("^", con$coreApi$scheme, "\\W{3}", con$coreApi$host, ".\\d+.?\\w*.", odat, resource, ".?", query)
  expect_match(builtURL, pattern)
})

test_that(paste("Test that the build url handles the login special url on:", env$auth), {
  expect_equivalent(con$response$status_code, 200)
})

test_that(paste("Test that the build url handles the file special url on:", env$auth), {
  skip("Not tested here until the remediation of the file upload has been completed. See RSDK-80")
})

test_that(paste("Test that the build url handles the json special url on:", env$auth), {
  builtURL <- CoreAPIV2::buildUrl(con$coreApi, special = "json")
  pattern <- paste0("^", con$coreApi$scheme, "\\W{3}", con$coreApi$host, ".\\d+.?\\w*.sdk")
  expect_match(builtURL, pattern)
  jsonEndpoint <- CoreAPIV2::apiGET(coreApi = con$coreApi, data$testPocoName, "", special = "json")
  expect_equivalent(jsonEndpoint$response$status_code, 200)
})
