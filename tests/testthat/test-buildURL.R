
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @auth Scott Russell scott.russell@thermofisher.com
#' @description \code{buildUrl} testing buildURL.


context("Tests for buildURL")

# Completed regression for 5.3.8 and 6.0.1

lapply(environments, function(x) {
  con <- Connect(x)
  
  if (con$coreApi$account == "PLATFORM ADMIN") {
    odat <- "/odata/"
  } else if (!is.null(con$coreApi$account) && is.null(con$coreApi$TenantShortName)) {
    odat <- paste0("/", CoreAPIV2::odataCleanName(con$coreApi$account), "/odata/")
  } else if (!is.null(con$coreApi$TenantShortName)) {
    odat <- paste0("/", CoreAPIV2::odataCleanName(con$coreApi$TenantShortName), "/odata/")
  } else {
    odat <- "/odata/"
  }
  
  test_that(paste("Test that buildURL responds correctly when passed a resource and query on:", x), {
    resource <- "res"
    query <- "?test query"
    builtURL <- CoreAPIV2::buildUrl(con$coreApi, resource = resource, query = query)
    pattern <- paste0("^", con$coreApi$scheme, "\\W{3}", con$coreApi$coreUrl, ".\\d+.?\\w*.", odat, resource, ".?", query)
    expect_match(builtURL, pattern)
  })

  test_that(paste("Test that the build url handles the login special url on:", x), {
    expect_equivalent(con$response$status_code, 200)
  })

  test_that(paste("Test that the build url handles the file special url on:", x), {
    ## TODO not tested here until the remediation of the file upload has been completed
  })

  test_that(paste("Test that the build url handles the json special url on:", x), {
    builtURL <- CoreAPIV2::buildUrl(con$coreApi, special = "json")
    pattern <- paste0("^", con$coreApi$scheme, "\\W{3}", con$coreApi$coreUrl, ".\\d+.?\\w*.sdk")
    expect_match(builtURL, pattern)
    
    jsonEndpoint <- CoreAPIV2::apiGET(coreApi = con$coreApi, POCO60NAME, "", special = "json")
    expect_equivalent(jsonEndpoint$response$status_code, 200)
  })

  CoreAPIV2::logOut(con$coreApi)
})
