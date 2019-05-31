#' getAttachedAttributeFile - Gets file attached to an attribute on a entity
#'
#' \code{getAttachedAttributeFile}  Gets file attached to an attribute on a entity
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type where sample is attached
#' @param barcode barcode of the entity
#' @param attribute name of the attribute
#' @param ... additional arguments passed to \code{apiGET}
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the binary-format HTTP response content.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- getAttachedAttributeFile(login$coreApi, entityType, barcode, attribute)
#' witeBin(response$entity, "filename.txt")
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{ getAttachedAttributeFile }  Gets file attached to an attribute on a entity.

getAttachedAttributeFile <-
  function(coreApi,
             entityType,
             barcode,
             attribute,
             ...) {
    # clean the name for ODATA

    resource <- odataCleanName(entityType)

    attribute <- odataCleanName(attribute)

    # no lint start
    query <- paste0(
      "('",
      barcode,
      "')/",
      attribute,
      "/$value"
    )
    # no lint end

    header <- c(Accept = "application/json;odata.metadata=full")

    response <-
      apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        useRaw = TRUE,
        ...
      )

    list(entity = response$content, response = response$response)
  }





#' getAttachedFile - Gets file attached to an attribute on a entity
#'
#' \code{getAttachedFile}  Gets file attached to an attribute on a entity
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type where sample is attached
#' @param barcode barcode of the entity
#' @param attribute name of the attribute
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used
#' @return returns a list $entity contains file data, $response contains the entire http response
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- getAttachedFile(login$coreApi, entityType, barcode, attribute)
#' witeBin(response$entity, "filename.txt")
#' logOut(login$coreApi)
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @description \code{ getAttachedFile }  Gets file attached to an attribute on a entity.
#' The $entity slot contains the binary file data.

#'
#' @name getAttachedFile-deprecated
#' @seealso \code{\link{pfsrsdk-deprecated}}
#' @keywords internal
NULL

#' @rdname pfsrsdk-deprecated
#' @section \code{getAttachedFile}:
#' For \code{getAttachedFile}, use \code{\link{getAttachedAttributeFile}}.
#'
#' @export



getAttachedFile <-
  function(coreApi,
             entityType,
             barcode,
             attribute,
             useVerbose = FALSE) {
    # clean the name for ODATA

    resource <- odataCleanName(entityType)

    attribute <- odataCleanName(attribute)
    # no lint start
    query <- paste0(
      "('",
      barcode,
      "')/",
      attribute,
      "/$value"
    )
    # no lint end

    header <- c(Accept = "application/json")


    resource <- odataCleanName(resource)

    sdk_url <-
      buildUrl(
        coreApi,
        resource = resource,
        query = query,
        special = NULL,
        useVerbose = useVerbose
      )
    base_sdk_url <-
      sdk_url # need if we need to build url for additional chunks

    cookie <-
      c(
        JSESSIONID = coreApi$jsessionId,
        AWSELB = coreApi$awselb
      )

    # Get first response

    if (useVerbose) {
      response <-
        httr::with_verbose(httr::GET(
          sdk_url,
          httr::add_headers(header),
          httr::set_cookies(cookie)
        ))
    } else {
      response <- httr::GET(
        sdk_url,
        httr::add_headers(header),
        httr::set_cookies(cookie)
      )
    }





    bin <- httr::content(response, "raw")



    list(entity = bin, response = response)
  }
