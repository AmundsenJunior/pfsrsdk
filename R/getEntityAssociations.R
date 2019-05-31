# ` getEntityAssociations - Get associations for a entity
#'
#' \code{getEntityAssociations} Get assoication for a context
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type to get
#' @param barcode barcode of entity to get
#' @param associationContext association context
#' @param fullMetadata - get full metadata, default is FALSE
#' @param ... additional arguments passed to \code{apiGET}
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the HTTP response content of entity associations.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' associations <- getEntityAssociations(login$coreAPI, "entityType", "barcode", "associationContext")
#' logOut(login$coreAPI)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{getEntityAssociations}  Get assoication for a entity

getEntityAssociations <-
  function(coreApi,
             entityType,
             barcode,
             associationContext,
             fullMetadata = FALSE,
             ...) {

    # this is the context for the association not the URL context
    associationContext <- odataCleanName(associationContext)
    query <- paste0("('", barcode, "')/", associationContext)

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
