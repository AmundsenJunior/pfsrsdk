#' getWellContents -  Gets content information of a single container well.
#'
#' \code{getWellContents} Gets content information of a single container well.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerBarcode container barcode
#' @param containerWellNum number location of container's well
#' @param containerType entity type of container (default: "CONTAINER")
#' @param fullMetadata get full metadata, default is FALSE
#' @param useVerbose  Use verbose communication for debugging (default: FALSE)
#' @export
#' @return RETURN returns a list $entity contains well information, $response contains the entire http response
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' well <- getWellContents(login$coreApi, "VIA9", "1", "VIAL")
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{getWellContents} - Gets content information of a single container well.

getWellContents <-
  function(coreApi,
             containerBarcode,
             containerWellNum,
             containerType = "CONTAINER",
             fullMetadata = FALSE,
             useVerbose = FALSE) {
    containerType <- odataCleanName(containerType)
    containerWellNum <- as.numeric(containerWellNum)
    resource <- "CELL"

    cellId <- getContainerCellIds(coreApi, containerBarcode, containerType, useVerbose)$entity[containerWellNum]

    case(
      grepl("[0-2]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        query <- paste0("(", cellId, ")?$expand=CONTENT($expand=IMPL_SAMPLE_LOT)")
      },
      grepl("[3-9]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        query <- paste0("(", cellId, ")?$expand=CELL_CONTENTS($expand=SAMPLE_LOT)")
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

    response <-
      list(entity = response$content, response = response$response)
  }
