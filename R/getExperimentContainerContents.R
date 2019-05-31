#' getExperimentContainerContents -  Gets experiment container contents.
#'
#' \code{getExperimentContainerContents} Gets experiment container contents.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerBarcode Experiment container barcode
#' @param containerType Experiment container entity type
#' @param fullMetadata - Get full metadata, default is FALSE
#' @param ... additional arguments passed to \code{apiGET}
#' @export
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the HTTP response content of container information.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' cell <- getExperimentContainerContents(login$coreApi, "BTCR1", "EXPERIMENT_CONTAINTER")
#' logOut(login$coreApi)
#' }
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{getExperimentContainerContents} - Gets information about experiment container contents.

getExperimentContainerContents <-
  function(coreApi,
             containerBarcode,
             containerType = "EXPERIMENT_CONTAINER",
             fullMetadata = FALSE,
             ...) {

    # clean the name for ODATA
    resource <- odataCleanName(containerType)

    case(
      grepl("[0-2]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        expansion <- "?$expand=REV_IMPL_CONTAINER_CELL($expand=CONTENT($expand=IMPL_SAMPLE_LOT))"
      },
      grepl("[3-9]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        expansion <- "?$expand=CELLS($expand=CELL_CONTENTS($expand=SAMPLE_LOT))"
      }
    )

    query <-
      paste0(
        "('",
        containerBarcode,
        "')/CONTAINER",
        expansion
      )


    if (fullMetadata) {
      header <- c(Accept = "application/json;odata.metadata=full")
    } else {
      header <- c(Accept = "application/json;odata.metadata=minimal")
    }

    out <-
      apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        ...
      )

    list(entity = out$content, response = out$response)
  }
