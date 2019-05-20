#' getEntityByBarcode - Get an entity by barcode from the Core LIMS using the ODATA API.
#'
#' \code{getEntityByBarcode} get an entity from the LIMS by barcode
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type to get
#' @param barcode barcode of entity to get
#' @param fullMetadata - get full metadata, default is FALSE
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http
#' @return returns a list $entity contains entity information, $response contains the entire http response
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
#' @description \code{getEntityByBarcode}  Get an entity by barcode from the Core LIMS using the ODATA API.



getEntityByBarcode <-
  function(coreApi,
             entityType,
             barcode,
             fullMetadata = FALSE,
             useVerbose = FALSE) {
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
        useVerbose = useVerbose
      )



    list(entity = out$content, response = out$response)
  }
