#' getExperimentSamplesWithSampleLots - Gets sample lots assigned to experiment samples of an experiment.
#' \code{getExperimentSamplesWithSampleLots}  Gets sample lots assigned to experiment samples of an experiment.
#' @param coreAPI coreApi object with valid jsessionid
#' @param experimentType entity type which has the attribute metadata
#' @param experimentBarcode barcode of entity to get
#' @param fullMetadata get full metadata, default is FALSE
#' @param ... additional arguments passed to \code{apiGET}
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} list with sample lot names for the corresponding experiment sample barcode from a specific experiment.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' experimentSampleLotNames <- getExperimentSamplesWithSampleLots(login$coreApi, "experimentType", "EXP123")
#' logOut(login$coreApi)
#' }
#' Specific data can be extracted, one example is to extract the sample lot names:
#' \dontrun{
#' sampleLotName <- sapply(
#'     response$content[[expansion]],
#'     FUN = function(x) {
#'         x$ENTITY$Name})
#' }
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Edgardo Gutierrez edgardo.gutierrez@thermofisher.com
#' @description \code{getExperimentSamplesWithSampleLots} - Gets sample lots assigned to experiment samples of an experiment.


getExperimentSamplesWithSampleLots <-
  function(coreApi,
             experimentType,
             experimentBarcode,
             fullMetadata = FALSE,
             ...) {

    # clean the name for ODATA
    experimentType <- odataCleanName(experimentType)
    
    resource <- paste0(experimentType, "('", experimentBarcode, "')")

    case(
      grepl("[0-2]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        expansion <- "REV_EXPERIMENT_EXPERIMENT_SAMPLE"
      },
      grepl("[3-9]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        expansion <- "EXPERIMENT_SAMPLES"
      }
    )

    query <-
      paste0(
        "?$expand=",
        expansion,
        "($expand=ENTITY)"
      )

    if (fullMetadata) {
      headers <- c(Accept = "application/json;odata.metadata=full")
    } else {
      headers <- NULL
    }

    response <-
      apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = headers,
        ...
      )

    list(entity = response$content, response = response$response)
  }
