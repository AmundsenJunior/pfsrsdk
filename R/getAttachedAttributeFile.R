#' getAttachedAttributeFile - Gets file attached to an attribute on a entity
#'
#' \code{getAttachedAttributeFile}  Gets file attached to an attribute on a entity
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type where sample is attached
#' @param barcode barcode of the entity
#' @param attribute name of the attribute
#' @param ... additional arguments passed to \code{apiGET}
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the binary-format HTTP response content.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- getAttachedAttributeFile(login$coreApi, entityType, barcode, attribute)
#' witeBin(response$entity, "filename.txt")
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{ getAttachedAttributeFile }  Gets file attached to an attribute on a entity.

getAttachedAttributeFile <-
  function(coreApi,
             entityType,
             barcode,
             attribute,
             ...) {
    # clean the name for ODATA

    resource <- odataCleanName(entityType)

    attribute <- odataCleanName(attribute)

    # no lint start
    query <- paste0(
      "('",
      barcode,
      "')/",
      attribute,
      "/$value"
    )
    # no lint end

    header <- c(Accept = "application/json;odata.metadata=full")

    response <-
      apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        useRaw = TRUE,
        ...
      )

    list(entity = response$content, response = response$response)
  }
