
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code{buildUrl} testing buildURL.


context("Tests for buildURL")

# Completed regression for 5.3.8 and 6.0.1

lapply(environments, function(x) {
  con <- Connect(x)
  if (!is.null(con$coreApi$account) && is.null(con$coreApi$TenantShortName)) {
    odat <- paste0("/", odataCleanName(con$coreApi$account), "/odata/")
  } else if (!is.null(con$coreApi$TenantShortName)) {
    odat <- paste0("/", odataCleanName(con$coreApi$TenantShortName), "/odata/")
  } else {
    odat <- "/odata/"
  }
  test_that(paste0("test that buildURL passes responds correctly when passed a resource and query on: ", x), {
    resource <- "res"
    query <- "?test query"
    builtURL <- buildUrl(con$coreApi, resource = resource, query = query)
    pattern <- paste0("^", con$coreApi$scheme, "\\W{3}", con$coreApi$coreUrl, ".\\d+", odat, resource, ".?", query)
    expect_match(builtURL, pattern)
  })

  test_that(paste0("Test that the build url handles the login special url on: ", x), {
    expect_equivalent(con$response$status_code, 200)
  })

  test_that(paste0("Test that the build url handles the file special url on: ", x), {
    ## TODO not tested here until the remediation of the file upload has been completed
  })

  test_that(paste0("Test that the build url handles the json special url on: ", x), {
    builtURL <- buildUrl(con$coreApi, special = "json")
    pattern <- paste0("^", con$coreApi$scheme, "\\W{3}", con$coreApi$coreUrl, ".\\d+.sdk")
    expect_match(builtURL, pattern)
    jsonEndpoint <- apiGET(coreApi = con$coreApi, POCO60NAME, "", special = "json")

    expect_equivalent(jsonEndpoint$response$status_code, 200)
  })

  CoreAPIV2::logOut(con$coreApi)
})
