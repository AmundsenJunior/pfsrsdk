#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description Tests for setting PFS's Semanctic Version.
#'
context("Tests for setSemVer")

test_that(paste("test setSemVer() on:", env$auth), {
  
  result <- CoreAPIV2::setSemVer(con$coreApi)

  # #Tests if the warning is returned when the server value in the Auth-*.json is NULL
  # expect_error(result)


  #Tests if setSemVer is able to retrieve the PFS SemVer  
  expect(length(result)>0)
})
