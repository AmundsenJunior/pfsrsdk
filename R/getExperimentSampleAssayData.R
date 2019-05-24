#' getExperimentSampleAssayData - Gets assay data for an experiment sample.
#'
#' \code{getExperimentSampleAssayData }  Gets assay data for a experiment sample identified by barcode.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentAssayType assay type to get
#' @param experimentSampleBarcode experiment sample barcode of entity to get
#' @param fullMetadata - get full metadata, default is FALSE
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the HTTP response content of entity information.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' experiment <- getExperimentSampleAssayData(login$coreApi, "experimentAssayType", "experimentSampleBarcode")
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{ getExperimentSampleAssayData }  Gets assay data for a experiment sample identified by barcode.

getExperimentSampleAssayData <-
  function(coreApi,
             experimentAssayType,
             experimentSampleBarcode,
             fullMetadata = FALSE,
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

    if (fullMetadata) {
      header <- c(Accept = "application/json;odata.metadata=full")
    } else {
      header <- c(Accept = "application/json;odata.metadata=minimal")
    }

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
