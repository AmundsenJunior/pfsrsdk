#' getExperimentContainers - Gets experiment containers from experiment identified by barcode.
#'
#' \code{getExperimentContainers}  Gets experiment containers from experiment identified by barcode.
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentType experiment entity type to get
#' @param experimentBarcode barcode of experiment to query
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http POST
#' @return returns a list $entity contains barcodes of the containers, $response contains the entire http response
#' @export
#' @examples
#' \dontrun{
#' api <- CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login <- CoreAPIV2::authBasic(api)
#' exptContainerBarcodes <- CoreAPIV2::getExperimentContainers(login$coreApi, "experimentType", "experimentBarcode")
#' CoreAPIV2:logOut(login$coreApi)
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{getExperimentContainers}  Gets experiment contaniers from experiment identified by experiment barcode.




getExperimentContainers <-
  function(coreApi,
             experimentType,
             experimentBarcode,
             useVerbose = FALSE) {
    # clean the name for ODATA

    resource <- CoreAPIV2::odataCleanName(experimentType)

    association <- switch(EXPR = substr(coreApi$semVer, 1, 1),
      "2" = "REV_CONTAINER_EXPERIMENT_EXPERIMENT_CONTAINER",
      print("EXPERIMENT_CONTAINERS")
    )


    header <-
      c("Content-Type" = "application/json;odata.metadata=full", Accept = "application/json")


    response <-
      CoreAPIV2::apiGET(
        coreApi,
        resource = resource,
        query = paste0("('", experimentBarcode, "')/", association),
        headers = header,
        useVerbose = useVerbose
      )


    list(entity = unlist((
      lapply(
        response$content,
        FUN = function(x)
          x$Barcode
      )
    )), response = response$response)
  }
