#' getExperimentSamples - Gets experiment sample barcodes from experiment identified by barcode.
#'
#' \code{getExperimentSamples}  Gets experiment samples from experiment identified by barcode.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentType experiment entity type to get
#' @param barcode barcode of entity to get
#' @param fullMetadata get full metadata, default is FALSE
#' @param ... additional arguments passed to \code{apiGET}
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the HTTP response content of samples information.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- getExperimentSamples(login$coreApi, "experimentType", "barcode")
#' experimentsampleBarcodes <- response$entity
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{ getExperimentSamples}  Gets experiment sample barcodes from experiment identified by experiment barcode.

getExperimentSamples <-
  function(coreApi,
             experimentType,
             barcode,
             fullMetadata = FALSE,
             ...) {

    # clean the name for ODATA
    resource <-
      paste0(odataCleanName(experimentType), "('", barcode, "')")

    case(
      grepl("[0-2]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        query <- "?$expand=REV_EXPERIMENT_EXPERIMENT_SAMPLE"
      },
      grepl("[3-9]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        query <- "?$expand=EXPERIMENT_SAMPLES"
      }
    )

    if (fullMetadata) {
      header <- c(Accept = "application/json;odata.metadata=full")
    } else {
      header <- NULL
    }

    response <-
      apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        ...
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
