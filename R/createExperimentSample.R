#' createExperimentSample - Create a new experiment sample from a sample lot.
#'
#' \code{createExperimentSample} Creates a new instance of an entity.
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentType experiment type to get as character string
#' @param experimentBarcode experiment barcode
#' @param sampleLotBarcode barcode of sample to add to experiment
#' @param body values for sample attributes as a  list of key-values pairs
#' @param fullMetadata get full metadata, default is FALSE
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the HTTP response content.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' item <- createExperimentSample(login$coreApi,
#'   experimentType,
#'   experimentBarcode,
#'   sampleLotBarcode,
#'   body = NULL,
#'   useVerbose = FALSE
#' )
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{createExperimentSample} Creates a new experiment sample from a sample lot.

createExperimentSample <-
  function(coreApi,
             experimentType,
             experimentBarcode,
             sampleLotBarcode,
             body = NULL,
             fullMetadata = FALSE,
             useVerbose = FALSE) {
    # clean the names for ODATA

    experimentType <- odataCleanName(experimentType)

    experimentSampleType <- paste0(experimentType, "_SAMPLE")

    exptRef <-
      list("EXPERIMENT@odata.bind" = paste0("/", experimentType, "('", experimentBarcode, "')"))
    # no lint start
    entityRef <-
      list("ENTITY@odata.bind" = paste0("/ENTITY('", sampleLotBarcode, "')"))
    # no lint end
    fullBody <-
      jsonlite::toJSON(c(body, exptRef, entityRef), auto_unbox = TRUE)

    if (fullMetadata) {
      headers <- c("Content-Type" = "application/json", "Accept" = "application/json;odata.metadata=full")
    } else {
      headers <- c("Content-Type" = "application/json", "Accept" = "application/json")
    }

    response <-
      apiPOST(
        coreApi,
        resource = experimentSampleType,
        body = fullBody,
        encode = "json",
        headers = headers,
        special = NULL,
        useVerbose = useVerbose
      )

    list(entity = response$content, response = response$response)
  }
