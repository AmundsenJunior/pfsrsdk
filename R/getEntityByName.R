#' getEntityByName - Get an entity by name from the Core LIMS using the ODATA API.
#'
#' \code{getEntityByName} get an entity from the LIMS by barcode
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type to get
#' @param name name of entity to get
#' @param fullMetadata - get full metadata
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http
#' @return returns a list $entity contains entity information, $response contains the entire http response
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
#' @description \code{getEntityByName}  Get an entity by barcode from the Core LIMS using the ODATA API.



getEntityByName <-
  function(coreApi,
             entityType,
             name,
             fullMetadata = TRUE,
             useVerbose = FALSE) {
    query <- utils::URLencode(paste0("?$filter=Name eq '", name, "'"))




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
