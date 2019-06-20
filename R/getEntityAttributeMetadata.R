#' getEntityAttributeMetadata - Gets attribute metadata for a particular entity type.
#' \code{getEntityAttributeMetadata} Gets attribute metadata for a particular entity type.
#' @param coreAPI coreApi object with valid jsessionid
#' @param entityType entity type which has the attribute metadata
#' @param fullMetadata get full metadata, default is FALSE
#' @param ... additional arguments passed to \code{apiGET}
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the HTTP response content of attribute metadata for the specified entity.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' experimentAssayType <- getEntityAttributeMetadata(login$coreApi, "BEER_SAMPLE", FALSE)
#' logOut(login$coreApi)
#' }
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Francisco Marin francisco.marin@thermofisher.com
#' @author Edgardo Gutierrez edgardo.gutierrez@thermofisher.com
#' @description \code{getEntityAttributeMetadata} - Gets attribute metadata for a particular entity type.

getEntityAttributeMetadata <-
  function(coreApi,
             entityType,
             fullMetadata = FALSE,
             ...) {
    case(
      grepl("[0-2]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        warning(
          paste("getEntityAttributeMetadata OData action not available in", coreApi$semVer),
          call. = FALSE
        )
      },
      grepl("[3-9]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        # clean the name for ODATA
        entityType <- odataCleanName(entityType)

        resource <- "TYPE_ATTRIBUTE"
        query <- paste0("?$filter=ENTITY_TYPE/Name%20eq%20'", entityType, "'&$expand=ENTITY_TYPE")

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

        list(entity = response$content, response = response$response)
      }
    )
  }
