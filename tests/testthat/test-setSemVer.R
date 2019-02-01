#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description Tests for setting PFS's Semanctic Version.
#'
context("Tests for setSemVer")

test_that(paste("test setSemVer() on:", env$auth), {
  
  
  #Tests if setSemVer is able to retrieve the PFS SemVer 
  result <- CoreAPIV2::setSemVer(con$coreApi)
  expect(length(result)>0)

  #Tests if the warning is returned when the server value in the Auth-*.json is NULL
  con$coreApi$semVer <- NULL
  expect_warning(CoreAPIV2::authBasic(con$coreApi),"SemVer variable")
})
