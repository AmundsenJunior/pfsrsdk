#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code Tests for updateEntityLocation
context("Tests for updateEntityLocation")

# Completed regression for 5.3.8 and 6.0.1

cat(paste0("\n environments:\n", environments, "\n"))

lapply(environments, function(x) {
  con <- Connect(x)
  test_that(paste("test updateEntityLocation for: ", x), {
    barcode <- CoreAPIV2::getEntityByName(con$coreApi, TESTPOCO, POCO60NAME, useVerbose = verbose)$entity[[1]]$Barcode
    loc <- CoreAPIV2::getEntityLocation(con$coreApi, TESTPOCO, barcode, useVerbose = FALSE)
    updatedLoc <- CoreAPIV2::updateEntityLocation(con$coreApi, TESTPOCO, barcode, loc$entity[[1]]$Barcode, useVerbose = FALSE)

    expect_equivalent(httr::status_code(updatedLoc$response), 200)
  })
  CoreAPIV2::logOut(con$coreApi)
})
