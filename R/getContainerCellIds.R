#' getContainerCellIds -  Gets cell ids for a container
#'
#' \code{getContainerCellIds} Gets information about container contents.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerBarcode container barcode
#' @param containerType container entity type (default: CONTAINER)
#' @param ... additional arguments passed to \code{apiGET}
#' @export
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is a vector of cell IDs.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' cellIDs <- getContainerCellIds(login$coreApi, "TE1", containerType = "384 WELL PLATE")$entity
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{getContainerCellIds} -  Gets cell ids for a container

getContainerCellIds <-
  function(coreApi,
             containerBarcode,
             containerType = "CONTAINER",
             ...) {

    # clean the name for ODATA
    resource <- odataCleanName(containerType)

    case(
      grepl("[0-2]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        expansion <- "REV_IMPL_CONTAINER_CELL"
      },
      grepl("[3-9]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        expansion <- "CELLS"
      }
    )

    query <-
      paste0(
        "('",
        containerBarcode,
        "')?$expand=",
        expansion
      )

    out <-
      apiGET(
        coreApi,
        resource = resource,
        query = query,
        ...
      )

    cells <-
      unlist(lapply(out$content[[expansion]], function(x)
        x$Id))

    list(entity = cells, response = out$response)
  }
