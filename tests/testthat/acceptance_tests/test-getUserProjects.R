#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getUserProjects.

context("Tests for getUserProjects")

test_that(paste("test getUserProjects() returns successful on semantic version:", con$coreApi$semVer), {
  result <- getUserProjects(con$coreApi)

  expect_equal(result$response$status_code, 200)

  projectBarcodes <- unlist(lapply(
    result$entity,
    FUN = function(x)
      x$Barcode
  ))

  expect_gt(length(projectBarcodes), 0)
})
