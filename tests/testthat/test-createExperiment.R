
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description Tests for Experiment creation.
#'
context("Tests for createExperiment")

lapply(environments, function(x) {
  con <- Connect(x)
  test_that(paste("test createExperiment() on: ", x), {
    
    expt <- CoreAPIV2::createExperiment(con$coreApi, EXPERIMENTTYPE, EXPERIMENTASSAYTYPE, EXPERIMENTASSAYBARCODE, PROTOCOLTYPE,
                                        EXPERIMENTPROTOCOLBARCODE,
                                        body = NULL, useVerbose = verbose)
    
    expect_that(httr::http_status(expt$response)$reason, equals("Created"))
    
  })
  
  CoreAPIV2::logOut(con$coreApi)
})
