# ` updateEntityLocation - Update entity location
#'
#' \code{updateEntityLocation}- Update entity location
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType type of entity to update
#' @param barcode barcode of entity to update
#' @param locationBarcode barcode of new location
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http
#' @return returns a list $entity contains entity information, $response contains the entire http response
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
#' @description \code{updateEntityLocation} - Update entity location

updateEntityLocation <-
  function(coreApi,
             entityType,
             barcode,
             locationBarcode,
             useVerbose = FALSE) {

    # get new location ID
    id <-
      getEntityByBarcode(
        coreApi,
        "LOCATION",
        locationBarcode,
        fullMetadata = FALSE,
        useVerbose = useVerbose
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
        useVerbose = useVerbose
      )

    list(entity = httr::content(response), response = response)
  }
