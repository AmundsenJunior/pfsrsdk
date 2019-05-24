#' getContainerContents -  Gets container contents.
#'
#' \code{getContainerContents} Gets information about container contents.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerBarcode container barcode
#' @param containerType container entity type
#' @param fullMetadata return full metadata, default is FALSE
#' @param useVerbose  Use verbose communication for debugging
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
#' cell <- getContainerContents(login$coreApi, "VIA9", "VIAL")
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{getContainerContents} - Gets information about container cell contents.

getContainerContents <-
  function(coreApi,
             containerBarcode,
             containerType = "CONTAINER",
             fullMetadata = FALSE,
             useVerbose = FALSE) {

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
        "')",
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
        useVerbose = useVerbose
      )

    list(entity = out$content, response = out$response)
  }
