#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code Tests for getEntityLocation
context("Tests for getEntityLocation")

# Completed regression for 5.3.8 and 6.0.1

cat(paste0("\n environments:\n", environments, "\n"))

lapply(environments, function(x) {
  con <- Connect(x)
  test_that(paste("test getEntityLocation for: ", x), {
    barcode <- CoreAPIV2::getEntityByName(con$coreApi, TESTPOCOTYPE, TESTPOCONAME, useVerbose = verbose)$entity[[1]]$Barcode
    loc <- CoreAPIV2::getEntityLocation(con$coreApi, TESTPOCOTYPE, barcode, useVerbose = FALSE)

    expect_equivalent(httr::status_code(loc$response), 200)
    expect_match(loc$entity[[1]]$Barcode, TESTPOCOLOC)
  })
  CoreAPIV2::logOut(con$coreApi)
})
