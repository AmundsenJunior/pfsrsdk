#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getEntityProject
context("Tests for getEntityProject")

# Completed regression for 5.3.8 and 6.0.1

cat(paste0("\n environments:\n", environments, "\n"))

lapply(environments, function(x) {
  con <- Connect(x)
  
  test_that(paste("test getEntityProject for:", x), {
    barcode <- CoreAPIV2::getEntityByName(con$coreApi, TESTPOCOTYPE, TESTPOCONAME, useVerbose = verbose)$entity[[1]]$Barcode
    pro <- CoreAPIV2::getEntityProject(con$coreApi, TESTPOCOTYPE, barcode, useVerbose = FALSE)

    expect_equivalent(httr::status_code(pro$response), 200)
    expect_match(pro$entity[[1]]$Barcode, TESTPOCOPROJ)
  })
  
  CoreAPIV2::logOut(con$coreApi)
})
