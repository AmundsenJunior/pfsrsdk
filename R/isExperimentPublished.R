#' isExperimentPublished gets a boolean indicating if an experiment is published.
#'
#' \code{isExperimentPublished} gets a boolean indicating if an experiment is published.
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentType experiment entity type
#' @param experimentBarcode barcode of the experiment.
#' @param ... additional arguments passed to \code{apiGET}
#' @export
#' @return List of length 2, containing \code{content} and \code{response} objects:
#' \itemize{
#'  \item{\code{content}} is the HTTP response content of experiment published status.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' update <- isExperimentPublished(login$coreApi, experimentType, experimentBarcode, useVerbose = TRUE)
#' logOut(login$coreApi)
#' }
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{isExperimentPublished} - gets a boolean indicating if an experiment is published.

isExperimentPublished <-
  function(coreApi,
             experimentType,
             experimentBarcode,
             ...) {
    # build request
    resource <-
      paste0(odataCleanName(experimentType), "('", experimentBarcode, "')", "/PUBLISHED")

    response <-
      apiGET(
        coreApi,
        resource = resource,
        query = NULL,
        ...
      )

    list(
      status = response$content, response = response$response
    )
  }
