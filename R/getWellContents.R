#' getWellContents -  Gets information about a single container well contents.
#'
#' \code{getWellContents} Gets information about container well contents.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerBarcode container barcode
#' @param useVerbose  Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains cell information, $response contains the entire http response
#' @examples
#' \dontrun{
#' api<-CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' cell<-CoreAPIV2::getWellContents(login$coreApi,"VIA9","1",fullMetadata = TRUE)
#' CoreAPIV2::logOut(login$coreApi )
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @description \code{getWellContents} -  Gets information about a single container well contents.




getWellContents <-
  function(coreApi,
             containerBarcode,
             useVerbose = FALSE) {

    # make sure containerWellNum is numeric

    containerWellNum <- as.numeric(containerWellNum)

    resource <- "CELL"

    query <-
      paste0("EXPERIMENT_CONTAINER(", containerBarcode, ")/CONTAINER?$expand=REV_IMPL_CONTAINER_CELL($expand=CONTENT($expand=IMPL_SAMPLE_LOT))")


    header <-
      c("Content-Type" = "application/json;odata.metadata=full", Accept = "application/json")



    response <-
      CoreAPIV2::apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        useVerbose = useVerbose
      )



    response <-
      list(entity = response$content, response = response$response)
  }
