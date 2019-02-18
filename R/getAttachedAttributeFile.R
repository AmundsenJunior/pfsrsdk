#' getAttachedAttributeFile - Gets file attached to an attribute on a entity
#'
#' \code{getAttachedAttributeFile}  Gets file attached to an attribute on a entity
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type where sample is attached
#' @param barcode barcode of the entity
#' @param attribute name of the attribute
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used
#' @return returns a list $entity contains file data, $response contains the entire http response
#' @export
#' @examples
#' \dontrun{
#' api <- CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login <- CoreAPIV2::authBasic(api)
#' response <- getAttachedAttributeFile(login$coreApi, entityType, barcode, attributeName)
#' witeBin(response$entity, "filename.txt")
#' CoreAPIV2:logOut(login$coreApi)
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code{ getAttachedAttributeFile }  Gets file attached to an attribute on a entity.
#' The $entity slot contains the binary file data.

getAttachedAttributeFile <-
  function(coreApi,
             entityType,
             barcode,
             attribute,
             useVerbose = FALSE) {
    # clean the name for ODATA

    resource <- CoreAPIV2::odataCleanName(entityType)

    attribute <- CoreAPIV2::odataCleanName(attribute)

    query <- paste0(
      "('",
      barcode,
      "')/",
      attribute,
      "/$value"
    )


    header <- c(Accept = "application/json")


    resource <- CoreAPIV2::odataCleanName(resource)

    sdk_url <-
      CoreAPIV2::buildUrl(
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
#' @export
#' @examples
#' \dontrun{
#' api <- CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login <- CoreAPIV2::authBasic(api)
#' response <- getAttachedFile(login$coreApi, entityType, barcode, attributeName)
#' witeBin(response$entity, "filename.txt")
#' CoreAPIV2:logOut(login$coreApi)
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @description \code{ getAttachedFile }  Gets file attached to an attribute on a entity.
#' The $entity slot contains the binary file data.

#'
#' @name getAttachedFile-deprecated
#' @seealso \code{\link{CoreAPIV2-deprecated}}
#' @keywords internal
NULL

#' @rdname CoreAPIV2-deprecated
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

    resource <- CoreAPIV2::odataCleanName(entityType)

    attribute <- CoreAPIV2::odataCleanName(attribute)

    query <- paste0(
      "('",
      barcode,
      "')/",
      attribute,
      "/$value"
    )


    header <- c(Accept = "application/json")


    resource <- CoreAPIV2::odataCleanName(resource)

    sdk_url <-
      CoreAPIV2::buildUrl(
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
