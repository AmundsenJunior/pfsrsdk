# ` getEntityAssociations - Get associations for a entity
#'
#' \code{getEntityAssociations} Get assoication for a context
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type to get
#' @param barcode barcode of entity to get
#' @param context association context
#' @param fullMetadata - get full metadata
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http POST
#' @return returns a list $entity contains entity associations, $response contains the entire http response
#' @export
#' @examples
#' \dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' associations<-CoreAPIV2::getEntityAssociations(login$coreApi,"entityType","barcode",values)
#' CoreAPIV2::logOut(login$coreApi)
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @description \code{getEntityAssociations}  Get assoication for a entity




getEntityAssociations <-
  function(coreApi,
             entityType,
             barcode,
             context,
             fullMetadata = TRUE,
             useVerbose = FALSE) {

    #this is the context for the association not the URL context
    context <- CoreAPIV2::odataCleanName(context)
    query <- paste0("('", barcode, "')/",context)



    if (fullMetadata) {
      header <- c(Accept = "application/json;odata.metadata=full")
    } else {
      header <- c(Accept = "application/json;odata.metadata=minimal")
    }


    out <-
      CoreAPIV2::apiGET(
        coreApi,
        resource = entityType,
        query = query,
        headers = header,
        useVerbose = useVerbose
      )



    list(entity = out$content, response = out$response)
  }
