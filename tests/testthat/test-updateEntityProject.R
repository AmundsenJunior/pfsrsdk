#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for updateEntityProject
context("Tests for updateEntityProject")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test updateEntityProject for: ", env$auth), {
  barcode <- CoreAPIV2::getEntityByName(con$coreApi, data$testPocoUpdateType, data$testPocoUpdateName, useVerbose = verbose)$entity[[1]]$Barcode
    
  updateProj <- CoreAPIV2::updateEntityProject(con$coreApi, data$testPocoUpdateType, barcode, data$testPocoUpdateProj, useVerbose = verbose)
  expect_equivalent(httr::status_code(updateProj$response), 200)
    
  proj <- CoreAPIV2::getEntityProject(con$coreApi, data$testPocoUpdateType, barcode, useVerbose = verbose)
  expect_match(data$testPocoUpdateProj, proj$entity[[1]]$Barcode)
  
  CoreAPIV2::updateEntityProject(con$coreApi, data$testPocoUpdateType, barcode, data$testPocoProj, useVerbose = verbose)
})
