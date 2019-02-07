#' getExperimentContainerContents -  Gets experiment container contents.
#'
#' \code{getExperimentContainerContents} Gets experiment container contents.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerBarcode Experiment container barcode
#' @param containerType Experiment container entity type
#' @param fullMetadata Boolean on whether to return full metadata
#' @param useVerbose  Boolean on whether to use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains cell information, $response contains the entire http response
#' @examples
#' \dontrun{
#' api<-CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' cell<-CoreAPIV2::getExperimentContainerContents(login$coreApi,"BTCR1","EXPERIMENT_CONTAINTER")
#' CoreAPIV2::logOut(login$coreApi )
#' }
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{getExperimentContainerContents} - Gets information about experiment container contents.

getExperimentContainerContents <-
  function(coreApi,
             containerBarcode,
             containerType = "EXPERIMENT_CONTAINER",
             fullMetadata = TRUE,
             useVerbose = FALSE) {

    # clean the name for ODATA
    resource <- CoreAPIV2::odataCleanName(containerType)

    expansion <- switch(EXPR = substr(coreApi$semVer, 1, 1),
      "2" = "?$expand=REV_IMPL_CONTAINER_CELL($expand=CONTENT($expand=IMPL_SAMPLE_LOT))",
      "3" = "?$expand=CELLS($expand=CELL_CONTENTS($expand=SAMPLE_LOT))",
      print("?$expand=CELLS($expand=CELL_CONTENTS($expand=SAMPLE_LOT))")
    )

    query <-
      paste0(
        "('",
        containerBarcode,
        "')/CONTAINER",
        expansion
      )


    if (fullMetadata) {
      header <- c(Accept = "application/json;odata.metadata=full")
    } else {
      header <- c(Accept = "application/json;odata.metadata=minimal")
    }

    out <-
      CoreAPIV2::apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        useVerbose = useVerbose
      )

    list(entity = out$content, response = out$response)
  }
