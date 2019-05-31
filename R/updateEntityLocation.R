# ` updateEntityLocation - Update entity location
#'
#' \code{updateEntityLocation}- Update entity location
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType type of entity to update
#' @param barcode barcode of entity to update
#' @param locationBarcode barcode of new location
#' @param ... additional arguments passed to \code{apiPOST}
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the HTTP response content of updated entity location information.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- updateEntityLocation(login$coreApi, "entityType", "barcode", "locationBarcode")
#' entity <- response$entity
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{updateEntityLocation} - Update entity location

updateEntityLocation <-
  function(coreApi,
             entityType,
             barcode,
             locationBarcode,
             ...) {

    # get new location ID
    id <-
      getEntityByBarcode(
        coreApi,
        "LOCATION",
        locationBarcode,
        fullMetadata = FALSE,
        useVerbose = TRUE
      )$entity$Id

    resource <- paste0(entityType, "('", barcode, "')/pfs.Entity.InventoryMove")
    body <- jsonlite::toJSON(list("locationId" = jsonlite::unbox(id)))
    header <- c("Content-Type" = "application/json;odata.metadata=minimal")

    # update location
    response <-
      apiPOST(
        coreApi,
        resource = resource,
        body = body,
        encode = "raw",
        headers = header,
        ...
      )

    list(entity = response$content, response = response$response)
  }
