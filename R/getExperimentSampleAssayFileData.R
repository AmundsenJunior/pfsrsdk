#' getExperimentSamplesAssayFileData - Gets file attached as assay data.
#'
#' \code{getExperimentSamplesAssayFileData }  Gets file attached as assay data.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param assayType assay type to get
#' @param experimentSampleBarcode experiment sample barcode of entity to get
#' @param attributeName Name of the attibute that containts the file data
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http
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
#' response <- getExperimentSamplesAssayFileData(login$coreApi, "assayType", "experimentSampleBarcode", "CI_FILE")
#' writeBin(response$entity, "myfile.png")
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{ getExperimentSamplesAssayFileData }  Gets file attached as assay data.
#' Use getExperimentSamplesAssayData for non-file data.

getExperimentSamplesAssayFileData <-
  function(coreApi,
             assayType,
             experimentSampleBarcode,
             attributeName,
             useVerbose = FALSE) {
    # clean the name for ODATA
    resource <- paste0(odataCleanName(assayType), "_DATA")

    # no lint start
    query <- paste0(
      "('",
      experimentSampleBarcode,
      "')/",
      attributeCleanName(attributeName),
      "/$value"
    )
    # no lint end


    headers <- c(Accept = "image/png")
    resource <- odataCleanName(resource)

    sdk_url <-
      buildUrl(
        coreApi,
        resource = resource,
        query = query,
        special = NULL
      )

    cookie <-
      c(
        JSESSIONID = coreApi$jsessionId,
        AWSELB = coreApi$awselb
      )

    if (useVerbose) {
      response <-
        httr::with_verbose(
          httr::GET(sdk_url, httr::add_headers(headers)),
          httr::set_cookies(cookie)
        )
    } else {
      response <- httr::GET(
        sdk_url,
        httr::add_headers(headers),
        httr::set_cookies(cookie)
      )
    }

    # check for general HTTP error in response
    if (httr::http_error(response)) {
      stop({
        print("API call failed")
        print(httr::http_status(response))
      },
      call. = FALSE
      )
    }

    # not sure what is going to happen if file is returned chunked

    bin <- httr::content(response, "raw")

    list(entity = bin, response = response)
  }
