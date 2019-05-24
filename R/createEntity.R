#' createEntity - Create a new instance of a entity. Used for standard object or "Plan Old Core Objects" (POCOs)
#'
#' \code{createEntity} Create a new instance of a entity. Used for standard object or "Plan Old Core Objects" (POCOs)
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type to get as character string
#' @param body values for attributes and associations as a  list of key-values pairs
#' @param locationId location ID for initial location as character string
#' @param projectIds project comma separated list of project IDs as character string
#' @param barcode User provided barcode as a character string
#' @param fullMetadata get full metadata, default is FALSE
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the HTTP response content.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' item <- createEntity(login$coreApi, "Entity_Type")
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{createEntity} Creates a new entity instance. Required inputs are url, jsessionId and entityType.

createEntity <- function(coreApi,
                         entityType,
                         body,
                         locationId = NULL,
                         projectIds = NULL,
                         barcode = NULL,
                         fullMetadata = FALSE,
                         useVerbose = FALSE) {

  # scrub unused fields from the body
  body <- body[!body %in% ""]

  if (fullMetadata) {
    headers <- c("Content-Type" = "application/json", "Accept" = "application/json;odata.metadata=full")
  } else {
    headers <- c("Content-Type" = "application/json", "Accept" = "application/json")
  }

  response <-
    apiPOST(
      coreApi,
      resource = entityType,
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      encode = "raw",
      headers = headers,
      special = NULL,
      useVerbose = useVerbose
    )

  list(entity = response$content, response = response$response)
}
