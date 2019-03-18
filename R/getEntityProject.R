#' getEntityProject - Get project(s) for an entity by barcode from the Core LIMS using the ODATA API.
#'
#' \code{getEntityLocation} Get project(s) for an entity by barcode from the Core LIMS using the ODATA API.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type to get
#' @param barcode barcode of entity to get
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http
#' @return returns a list $entity contains projects entity information, $response contains the entire http response
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' item <- getEntityLocation(login$coreApi, "entityType", "barcode")
#' logOut(login$coreApi)
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code{getEntityProject}  Get project(s) for an entity by barcode from the Core LIMS using the ODATA API.




getEntityProject <-
  function(coreApi, entityType, barcode, useVerbose = FALSE) {
    query <- paste0("('", barcode, "')/PROJECT")

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
