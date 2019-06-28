#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getting PFS's Semantic Version.

context("Tests for getSemVer")

test_that(paste("test getSemVer on semantic version:", con$coreApi$semVer), {
  # Tests if getSemVer is able to retrieve the PFS SemVer
  result <- getSemVer(con$coreApi)
  expect_match(result, "[0-9]+\\.[0-9]+\\.[0-9]+")

  # Tests if the warning is returned when the server value in the Auth-*.json is NULL
  con$coreApi$semVer <- NULL
  expect_warning(authBasic(con$coreApi), "SemVer variable")
})
