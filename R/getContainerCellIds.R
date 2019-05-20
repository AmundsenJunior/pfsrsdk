#' getContainerCellIds -  Gets cell ids for a container
#'
#' \code{getContainerCellIds} Gets information about container contents.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerBarcode container barcode
#' @param containerType container entity type (default: CONTAINER)
#' @param useVerbose  Use verbose communication for debugging (default: FALSE)
#' @export
#' @return RETURN returns $entity a array of cell IDs and  $response contains the entire http response
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
             useVerbose = FALSE) {

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

    header <- c(Accept = "application/json")

    out <-
      apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        useVerbose = useVerbose
      )

    cells <-
      unlist(lapply(out$content[[expansion]], function(x)
        x$Id))

    list(entity = cells, response = out$response)
  }
