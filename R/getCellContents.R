#' getCellContents -  Gets information about a single container cell contents.
#'
#' \code{getCellContents} Gets information about container cell contents.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerCellId container cell number as a string
#' @param useVerbose  Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains cell information, $response contains the entire http response
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' cell <- getCellContents(login$coreApi, "1234233", fullMetadata = TRUE)
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @description \code{getCellContents} -  Gets information about a single container cell contents.





getCellContents <-
  function(coreApi, containerCellId, useVerbose = FALSE) {
    # make sure containerCellNum is numeric
    containerCellId <- as.numeric(containerCellId)

    resource <- "CELL"

    expansion <- switch(EXPR = substr(coreApi$semVer, 1, 1),
      "2" = "?$expand=CONTENT($expand=IMPL_SAMPLE_LOT)",
      "3" = "?$expand=CELL_CONTENTS($expand=SAMPLE_LOT)",
      print("?$expand=CELL_CONTENTS($expand=SAMPLE_LOT")
    )

    query <-
      paste0(
        "(",
        containerCellId,
        ")",
        expansion
      )

    header <-
      c("Content-Type" = "application/json;odata.metadata=full", Accept = "application/json")

    response <-
      apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        useVerbose = useVerbose
      )

    response <-
      list(entity = response$content, response = response$response)
  }
