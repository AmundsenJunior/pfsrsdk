#' getEntityLocation - Get location for an entity by barcode from the Core LIMS using the ODATA API.
#'
#' \code{getEntityLocation} Get location for an entity  by barcode from the Core LIMS using the ODATA API.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type to get
#' @param barcode barcode of entity to get
#' @param fullMetadata - get full metadata, default is FALSE
#' @param ... additional arguments passed to \code{apiGET}
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the HTTP response content of entity location information.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' item <- getEntityLocation(login$coreApi, "entityType", "barcode")
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{getEntityLocation}  Get location for an entity by barcode from the Core LIMS using the ODATA API.

getEntityLocation <-
  function(coreApi,
             entityType,
             barcode,
             fullMetadata = FALSE,
             ...) {
    query <- paste0("('", barcode, "')/LOCATION")

    if (fullMetadata) {
      header <- c(Accept = "application/json;odata.metadata=full")
    } else {
      header <- NULL
    }

    out <-
      apiGET(
        coreApi,
        resource = entityType,
        query = query,
        headers = header,
        ...
      )

    list(entity = out$content, response = out$response)
  }
