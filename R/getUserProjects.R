#' getUserProjects - Gets metadata of projects accessible to the current user.
#'
#' \code{getUserProjects}  Gets metadata of projects accessible to the current user.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param ... additional arguments passed to \code{apiGET}
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} list with project's metadata.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- getUserProjects(login$coreApi)
#' logOut(login$coreApi)
#' }
#' Specific data can be extracted, one example is to extract the project barcodes:
#' \dontrun{
#' projectBarcodes <- unlist(lapply(
#'     response$content, 
#'     FUN = function(x)
#'         x$Barcode))
#' }
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{ getUserProjects}  Gets metadata of projects accessible to the current user.

getUserProjects <-
  function(coreApi,
             ...) {

    # Get username barcode
    username <- coreApi$username
    userBarcode <-
      apiGET(
        coreApi,
        resource = "EMPLOYEE",
        query = paste0("?$filter=CI_USERNAME%20eq%20'", username, "'"),
        useVerbose = TRUE
      )$content[[1]]$Barcode

    resource <- paste0("EMPLOYEE('", userBarcode, "')")

    query <- "?$expand=PROJECT"

    response <-
      apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = NULL,
        ...
      )

    list (entity = response$content$PROJECT, response = response$response)
  }
