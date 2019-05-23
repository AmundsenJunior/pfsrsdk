#' getEntityByName - Get an entity by name from the Core LIMS using the ODATA API.
#'
#' \code{getEntityByName} get an entity from the LIMS by barcode
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type to get
#' @param name name of entity to get
#' @param fullMetadata - get full metadata, default is FALSE
#' @param ... additional arguments passed to \code{apiGET}
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the HTTP response content.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' item <- getEntityByName(login$coreApi, "entityType", "name")
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{getEntityByName}  Get an entity by barcode from the Core LIMS using the ODATA API.

getEntityByName <-
  function(coreApi,
             entityType,
             name,
             fullMetadata = FALSE,
             ...) {
    query <- utils::URLencode(paste0("?$filter=Name eq '", name, "'"))

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
