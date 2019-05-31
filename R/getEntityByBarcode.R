#' getEntityByBarcode - Get an entity by barcode from PFS using the OData API.
#'
#' \code{getEntityByBarcode} Get an entity by barcode from PFS using the OData API.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type to get
#' @param barcode barcode of entity to get
#' @param fullMetadata - get full metadata, default is FALSE
#' @param ... additional arguments passed to \code{apiGET}
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the HTTP response content.
#'  \item{\code{response}} NULL by default (parameter \code{fullReturn} is TRUE).
#'   If \code{fullReturn} is FALSE, returns the entire HTTP response.
#' }
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' item <- getEntityByBarcode(login$coreApi, "entityType", "barcode")
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{getEntityByBarcode} Get an entity by barcode from PFS using the OData API.

getEntityByBarcode <-
  function(coreApi,
             entityType,
             barcode,
             fullMetadata = FALSE,
             ...) {
    query <- paste0("('", barcode, "')")

    if (fullMetadata) {
      header <- c(Accept = "application/json;odata.metadata=full")
    } else {
      header <- c(Accept = "application/json;odata.metadata=minimal")
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
