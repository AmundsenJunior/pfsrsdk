#' getEntityLocation - Get location for an entity by barcode from the Core LIMS using the ODATA API.
#'
#' \code{getEntityLocation} Get location for an entity  by barcode from the Core LIMS using the ODATA API.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type to get
#' @param barcode barcode of entity to get
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http
#' @return returns a list $entity contains location entity information, $response contains the entire http response
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
#' @description \code{getEntityLocation}  Get location for an entity by barcode from the Core LIMS using the ODATA API.




getEntityLocation <-
  function(coreApi, entityType, barcode, useVerbose = FALSE) {
    query <- paste0("('", barcode, "')/LOCATION")

    header <- c(Accept = "application/json;odata.metadata=minimal")

    out <-
      apiGET(
        coreApi,
        resource = entityType,
        query = query,
        headers = header,
        useVerbose = useVerbose
      )

    list(entity = out$content, response = out$response)
  }
