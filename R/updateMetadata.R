# `updateMetadata - Updates cached metadata so metadata is up to date.
#'
#' \code{updateMetadata} g Updates cached metadata so metadata is up to date.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param ... additional arguments passed to \code{apiGET}
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the HTTP response content of XML-formatted OData metadata for all entities.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' metadata <- updateMetadata(login$coreApi, useverbose = TRUE)
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{updateMetadata}  Updates cached metadata so metadata is up to date.
#' Must be run after any configuration changes.

updateMetadata <- function(coreApi, ...) {
  resource <- "$metadata"
  query <- "?reload=1"

  header <- c(Accept = "application/xml")

  out <-
    apiGET(
      coreApi,
      resource = resource,
      query = query,
      headers = header,
      ...
    )

  list(entity = out$content, response = out$response)
}
