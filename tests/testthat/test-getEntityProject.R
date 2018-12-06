#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code Tests for getEntityProject
context("Tests for getEntityProject")

# Completed regression for 5.3.8 and 6.0.1

cat(paste0("\n environments:\n", environments, "\n"))

lapply(environments, function(x) {
  con <- Connect(x)
  test_that(paste("test getEntityProject for: ", x), {
    barcode <- CoreAPIV2::getEntityByName(con$coreApi, TESTPOCO, POCO60NAME, useVerbose = verbose)$entity[[1]]$Barcode
    pro <- CoreAPIV2::getEntityProject(con$coreApi, TESTPOCO, barcode, useVerbose = FALSE)

    expect_equivalent(httr::status_code(pro$response), 200)
  })
  CoreAPIV2::logOut(con$coreApi)
})
