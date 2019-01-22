
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description Tests for Experiment creation.
#'
context("Tests for createExperiment")

lapply(environments, function(x) {
  con <- Connect(x)
  test_that(paste("test createExperiment() on: ", x), {
    
    # create Experiment
    # type of experiment
    EXPERIMENTTYPE <- "BITTERNESS EXPERIMENT"
    # type of experiment protocol
    PROTOCOLTYPE <- "BITTERNESS PROTOCOL"
    # Barcode for the experiment protocol
    EXPERIMENTPROTOCOLBARCODE <- "BTNP1"
    # type of experiment assay
    EXPERIMENTASSAYTYPE <- "BITTERNESS ASSAY"
    # Barcode for the experiment assay to use
    EXPERIMENTASSAYBARCODE <- "BTNA1"
    
    expt <- CoreAPIV2::createExperiment(con$coreApi, EXPERIMENTTYPE, EXPERIMENTASSAYTYPE, EXPERIMENTASSAYBARCODE, PROTOCOLTYPE,
                                        EXPERIMENTPROTOCOLBARCODE,
                                        body = "", useVerbose = useVerbose)
    
    expect_that(httr::http_status(expt$response)$reason, equals("Created"))
    
  })
  
  CoreAPIV2::logOut(con$coreApi)
})
