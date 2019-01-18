#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code Tests for updateEntityProject
context("Tests for updateEntityProject")

# Completed regression for 5.3.8 and 6.0.1

cat(paste0("\n environments:\n", environments, "\n"))

lapply(environments, function(x) {
  con <- Connect(x)
  
  test_that(paste("test updateEntityProject for: ", x), {
    barcode <- CoreAPIV2::getEntityByName(con$coreApi, TESTPOCOUPDATETYPE, TESTPOCOUPDATENAME, useVerbose = verbose)$entity[[1]]$Barcode
    
    updateProj <- CoreAPIV2::updateEntityProject(con$coreApi, TESTPOCOUPDATETYPE, barcode, TESTPOCOUPDATEPROJ, useVerbose = FALSE)
    expect_equivalent(httr::status_code(updateProj$response), 200)
    
    proj <- CoreAPIV2::getEntityProject(con$coreApi, TESTPOCOUPDATETYPE, barcode, useVerbose = FALSE)
    expect_match(TESTPOCOUPDATEPROJ, proj$entity[[1]]$Barcode)
  })
  CoreAPIV2::logOut(con$coreApi)
})
