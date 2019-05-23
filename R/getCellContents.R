#' getCellContents -  Gets information about a single container cell contents.
#'
#' \code{getCellContents} Gets information about container cell contents.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerCellId container cell number as a string
#' @param fullMetadata get full metadata, default is FALSE
#' @param ... additional arguments passed to \code{apiGET}
#' @export
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the HTTP response content of cell information.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' cell <- getCellContents(login$coreApi, "1234233", fullMetadata = TRUE)
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{getCellContents} -  Gets information about a single container cell contents.

getCellContents <-
  function(coreApi,
             containerCellId,
             fullMetadata = FALSE,
             ...) {

    # make sure containerCellNum is numeric
    containerCellId <- as.numeric(containerCellId)

    resource <- "CELL"

    case(
      grepl("[0-2]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        expansion <- "?$expand=CONTENT($expand=IMPL_SAMPLE_LOT)"
      },
      grepl("[3-9]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        expansion <- "?$expand=CELL_CONTENTS($expand=SAMPLE_LOT)"
      }
    )

    query <-
      paste0(
        "(",
        containerCellId,
        ")",
        expansion
      )

    if (fullMetadata) {
      header <- c("Accept" = "application/json;odata.metadata=full")
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

    response <-
      list(entity = response$content, response = response$response)
  }
