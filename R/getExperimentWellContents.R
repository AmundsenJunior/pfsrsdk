#' getExperimentWellContents -  Gets content information of a single container well in an experiment.
#'
#' \code{getExperimentWellContents} Gets content information of a single container well in an experiment.
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentContainerBarcode experiment container barcode
#' @param experimentContainerWellNum number location of experiment container's well
#' @param experimentContainerType entity type of experiment container (default: "EXPERIMENT_CONTAINER")
#' @param fullMetadata get full metadata, default is FALSE
#' @param ... additional arguments passed to \code{apiGET}
#' @export
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the HTTP response content of well information.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' well <- getExperimentWellContents(login$coreApi, "BTCR1", "1", "BITTERNESS_EXPERIEMENT_CONTAINER")
#' logOut(login$coreApi)
#' }
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{getExperimentWellContents} - Gets content information of a single container well in an experiment.

getExperimentWellContents <-
  function(coreApi,
             experimentContainerBarcode,
             experimentContainerWellNum,
             experimentContainerType = "EXPERIMENT_CONTAINER",
             fullMetadata = FALSE,
             ...) {
    experimentContainerType <- odataCleanName(experimentContainerType)
    experimentContainerWellNum <- as.numeric(experimentContainerWellNum)
    resource <- "CELL"

    cellId <- getExperimentContainerCellIds(coreApi, experimentContainerBarcode, experimentContainerType, useVerbose = TRUE)$entity[experimentContainerWellNum]

    case(
      grepl("[0-2]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        query <- paste0("(", cellId, ")?$expand=CONTENT($expand=IMPL_SAMPLE_LOT)")
      },
      grepl("[3-9]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        query <- paste0("(", cellId, ")?$expand=CELL_CONTENTS($expand=SAMPLE_LOT)")
      }
    )

    if (fullMetadata) {
      header <- c("Content-Type" = "application/json", "Accept" = "application/json;odata.metadata=full")
    } else {
      header <- c("Content-Type" = "application/json", "Accept" = "application/json")
    }

    response <-
      apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        ...
      )

    response <-
      list(entity = response$content, response = response$response)
  }
