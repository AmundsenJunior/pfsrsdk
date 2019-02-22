# ` getEntityAssociations - Get associations for a entity
#'
#' \code{getEntityAssociations} Get assoication for a context
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type to get
#' @param barcode barcode of entity to get
#' @param associationContext association context
#' @param fullMetadata - get full metadata
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http 
#' @return returns a list $entity contains entity associations, $response contains the entire http response
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' associations <- getEntityAssociations(login$coreAPI, "entityType", "barcode", "associationContext")
#' logOut(login$coreAPI)
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code{getEntityAssociations}  Get assoication for a entity




getEntityAssociations <-
  function(coreApi,
             entityType,
             barcode,
             associationContext,
             fullMetadata = TRUE,
             useVerbose = FALSE) {

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
        useVerbose = useVerbose
      )



    list(entity = out$content, response = out$response)
  }
