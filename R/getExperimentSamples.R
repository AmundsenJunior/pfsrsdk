#' getExperimentSamples - Gets experiment sample barcodes from experiment identified by barcode.
#'
#' \code{getExperimentSamples}  Gets experiment samples from experiment identified by barcode.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentType experiment entity type to get
#' @param barcode barcode of entity to get
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http 
#' @return returns a list $entity contains entity information, $response contains the entire http response
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- getExperimentSamples(login$coreApi, "experimentType", "barcode")
#' experimentsampleBarcodes <- response$entity
#' logOut(login$coreApi)
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{ getExperimentSamples}  Gets experiment sample barcodes from experiment identified by experiment barcode.





getExperimentSamples <-
  function(coreApi,
             experimentType,
             barcode,
             useVerbose = FALSE) {

    # clean the name for ODATA
    resource <-
      paste0(odataCleanName(experimentType), "('", barcode, "')")

    query <- switch(EXPR = substr(coreApi$semVer, 1, 1),
      "2" = "?$expand=REV_EXPERIMENT_EXPERIMENT_SAMPLE",
      "3" = "?$expand=EXPERIMENT_SAMPLES",
      print("?$expand=EXPERIMENT_SAMPLES")
    )

    header <-
      c("Content-Type" = "application/json;odata.metadata=full", Accept = "application/json")



    response <-
      apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        useVerbose = useVerbose
      )

    ResponseContent <- httr::content(response$response, as = "parsed")


    list(entity = unlist((
      lapply(
        ResponseContent$value,
        FUN = function(x)
          x$Barcode
      )
    )), response = response)
  }
