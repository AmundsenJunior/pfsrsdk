#' getExperimentContainers - Gets experiment containers from experiment identified by barcode.
#'
#' \code{getExperimentContainers}  Gets experiment containers from experiment identified by barcode.
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentType experiment entity type to get
#' @param experimentBarcode barcode of experiment to query
#' @param ... additional arguments passed to \code{apiGET}
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is a list of container barcodes.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' exptContainerBarcodes <- getExperimentContainers(login$coreApi, "experimentType", "experimentBarcode")
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{getExperimentContainers}  Gets experiment contaniers from experiment identified by experiment barcode.




getExperimentContainers <-
  function(coreApi,
             experimentType,
             experimentBarcode,
             ...) {
    # clean the name for ODATA

    resource <- odataCleanName(experimentType)

    case(
      grepl("[0-2]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        association <- "REV_CONTAINER_EXPERIMENT_EXPERIMENT_CONTAINER"
      },
      grepl("[3-9]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        association <- "EXPERIMENT_CONTAINERS"
      }
    )

    response <-
      apiGET(
        coreApi,
        resource = resource,
        query = paste0("('", experimentBarcode, "')/", association),
        ...
      )


    list(entity = unlist((
      lapply(
        response$content,
        FUN = function(x)
          x$Barcode
      )
    )), response = response$response)
  }
