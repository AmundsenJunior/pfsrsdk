#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code Tests for updateEntityProject
context("Tests for updateEntityProject")

# Completed regression for 5.3.8 and 6.0.1

cat(paste0("\n environments:\n", environments, "\n"))

lapply(environments, function(x) {
  con <- Connect(x)
  
  test_that(paste("test updateEntityProject for: ", x), {
    barcode <- CoreAPIV2::getEntityByName(con$coreApi, TESTPOCO, POCO60NAME, useVerbose = verbose)$entity[[1]]$Barcode
    
    updateProj <- CoreAPIV2::updateEntityProject(con$coreApi, TESTPOCO, barcode, POCO60PROJ2, useVerbose = FALSE)
    expect_equivalent(httr::status_code(updateProj$response), 200)
    
    proj <- CoreAPIV2::getEntityProject(con$coreApi, TESTPOCO, barcode, useVerbose = FALSE)
    expect_match(POCO60PROJ2, proj$entity[[1]]$Barcode)
  })
  CoreAPIV2::logOut(con$coreApi)
})
