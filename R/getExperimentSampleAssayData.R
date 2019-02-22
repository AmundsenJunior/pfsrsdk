#' getExperimentSampleAssayData - Gets assay data for an experiment sample.
#'
#' \code{getExperimentSampleAssayData }  Gets assay data for a experiment sample identified by barcode.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentAssayType assay type to get
#' @param experimentSampleBarcode experiment sample barcode of entity to get
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http 
#' @return returns a list $entity contains entity information, $response contains the entire http response
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' experiment <- getExperimentSampleAssayData(login$coreApi, "experimentAssayType", "experimentSampleBarcode")
#' logOut(login$coreApi)
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{ getExperimentSampleAssayData }  Gets experiment samples from experiment identified by experiment barcode.
#' Does not retieve files attached as data. Use getExperimentSampleAssayFileData to retrieve assay data that is a file.





getExperimentSampleAssayData <-
  function(coreApi,
             experimentAssayType,
             experimentSampleBarcode,
             useVerbose = FALSE) {
    # clean the name for ODATA

    resource <- odataCleanName("EXPERIMENT_SAMPLE")

    experimentAssayType <- odataCleanName(experimentAssayType)

    query <- paste0(
      "('",
      experimentSampleBarcode,
      "')/ASSAY_DATA/pfs.",
      experimentAssayType,
      "_DATA"
    )


    header <- c(Accept = "application/json")



    response <-
      apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        useVerbose = useVerbose
      )




    list(entity = response$content, response = response$response)
  }
