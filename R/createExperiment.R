#' createExperiment - Create a new instance of an experiment.
#'
#' \code{createExperiment} Creates a new instance of an entity.
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentType experiment type to get as character string
#' @param assayType assay type
#' @param assayBarcode assay barcode
#' @param protocolType protocol type
#' @param protocolBarcode protocol barcode
#' @param body values for experiment attributes and associations as a  list of key-values pairs
#' @param fullMetadata get full metadata, default is FALSE
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains entity information, $response contains the entire http response
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' experiment <- createExperiment(
#'   login$coreApi,
#'   "Experiment_Type",
#'   "Assaybarcode",
#'   "Protocolbarcode"
#' )
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{createExperiment} Creates a new experiment.

createExperiment <-
  function(coreApi,
             experimentType,
             assayType,
             assayBarcode,
             protocolType,
             protocolBarcode,
             body = NULL,
             fullMetadata = FALSE,
             useVerbose = FALSE) {
    # clean the names for ODATA

    experimentType <- odataCleanName(experimentType)
    assayType <- odataCleanName(assayType)
    protocolType <- odataCleanName(protocolType)

    assayRef <-
      list("EXPERIMENT_ASSAY@odata.bind" = paste0("/", assayType, "('", assayBarcode, "')"))

    protocolRef <-
      list(
        "EXPERIMENT_PROTOCOL@odata.bind" = paste0("/", protocolType, "('", protocolBarcode, "')")
      )

    fullBody <-
      jsonlite::toJSON(c(body, assayRef, protocolRef), auto_unbox = TRUE)

    if (fullMetadata) {
      headers <- c("Content-Type" = "application/json", "Accept" = "application/json;odata.metadata=full")
    } else {
      headers <- c("Content-Type" = "application/json", "Accept" = "application/json")
    }

    response <-
      apiPOST(
        coreApi,
        resource = experimentType,
        body = fullBody,
        encode = "json",
        headers = headers,
        special = NULL,
        useVerbose = useVerbose
      )



    list(entity = httr::content(response), response = response)
  }
