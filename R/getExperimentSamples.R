#' getExperimentSamples - Gets experiment sample barcodes from experiment identified by barcode.
#'
#' \code{getExperimentSamples}  Gets experiment samples from experiment identified by barcode.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentType experiment entity type to get
#' @param barcode barcode of entity to get
#' @param fullMetadata get full metadata, default is FALSE
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
#' @author Craig Parman info@ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{ getExperimentSamples}  Gets experiment sample barcodes from experiment identified by experiment barcode.

getExperimentSamples <-
  function(coreApi,
             experimentType,
             barcode,
             fullMetadata = TRUE,
             useVerbose = FALSE) {

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
      header <- c("Content-Type" = "application/json", Accept = "application/json;odata.metadata=full")
    } else {
      header <- c("Content-Type" = "application/json", Accept = "application/json")
    }

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
