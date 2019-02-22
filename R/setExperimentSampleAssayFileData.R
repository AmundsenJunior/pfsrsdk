#' setExperimentSampleAssayFileData - Puts file attached as assay data into an experiment sample.
#'
#' \code{setExperimentSampleAssayFileData } Puts file attached as assay data into an experiment sample.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param assayType assay type that contains sample
#' @param experimentSampleBarcode experiment sample barcode of entity to get
#' @param attributeName  Name of the attribute that containts the file data
#' @param filePath path to file to upload
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http calls
#' @return returns a list $entity contains binary object that in the file, $response contains
#' the entire http response
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- setExperimentSampleAssayFileData(
#'   login$coreApi,
#'   "TURBIDITY ASSAY",
#'   "TBES1",
#'   "CI_EXTRA_DATA",
#'   "/path/to/data.file"
#' )
#' logOut(login$coreApi)
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{ setExperimentSampleAssayFileData } Puts file attached as assay data into an experiment sample.

setExperimentSampleAssayFileData <-
  function(coreApi,
             assayType,
             experimentSampleBarcode,
             attributeName,
             filePath,
             useVerbose = FALSE) {
    if (!file.exists(filePath)) {
      stop({
        print("Unable to find file on local OS")
        print(filePath)
      },
      call. = FALSE
      )
    }

    resource <- paste0(odataCleanName(assayType), "_DATA")

    query <- paste0(
      "('",
      experimentSampleBarcode,
      "')/",
      attributeCleanName(attributeName)
    )

    metadata <- getEntityMetadata(coreApi, resource)
    valueFlag <- ifelse(match("Edm.Stream", metadata$attributes$types[match(attributeName, metadata$attributes$names)]) == 1, TRUE, FALSE)
    body <- httr::upload_file(filePath)
    header <- c("If-Match" = "*")

    response <-
      apiPUT(
        coreApi,
        resource = resource,
        query = query,
        body = body,
        encode = "raw",
        headers = header,
        special = NULL,
        useVerbose = useVerbose,
        valueFlag = valueFlag
      )

    list(
      entity = if (response$status_code == 204) NULL else httr::content(response),
      response = response
    )
  }





#' setExperimentSamplesAssayFileData - puts file attached as assay data in an experiment.
#'
#' \code{setExperimentSamplesAssayFileData }  puts file attached as assay data.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param assayType assay type that contains sample
#' @param experimentSampleBarcode experiment sample barcode of entity to get
#' @param attributeName  Name of the attribute that containts the file data
#' @param filePath path to file to upload
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http calls
#' @return returns a list $entity contains binary object that in the file, $response contains
#' the entire http response
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- setExperimentSamplesAssayFileData(
#'   login$coreApi,
#'   "assayType",
#'   "barcode",
#'   "filePath",
#'   "CI_FILE"
#' )
#' logOut(login$coreApi)
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @description \code{ setExperimentSamplesAssayFileData } Puts file attached as assay data
#'  in an experiment
#'
#' @name setExperimentSamplesAssayFileData-deprecated
#' @seealso \code{\link{pfsrsdk-deprecated}}
#' @keywords internal
NULL

#' @rdname pfsrsdk-deprecated
#' @section \code{setExperimentSamplesAssayFileData}:
#' For \code{setExperimentSamplesAssayFileData}, use \code{\link{setExperimentSampleAssayFileData}}.
#'
#' @export

setExperimentSamplesAssayFileData <-
  function(coreApi,
             assayType,
             experimentSampleBarcode,
             attributeName,
             filePath,
             useVerbose = FALSE) {
    .Deprecated(new = "setExperimentSampleAssayFileData")

    if (!file.exists(filePath)) {
      stop({
        print("Unable to find file on local OS")
        print(filePath)
      },
      call. = FALSE
      )
    }

    resource <- paste0(ODATAcleanName(assayType), "_DATA")
    resource <- ODATAcleanName(resource)
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

    body <- httr::upload_file(filePath, type = "image/png")

    sdk_url <-
      buildUrl(
        coreApi,
        resource = resource,
        query = query,
        special = NULL,
        useVerbose = useVerbose
      )

    cookie <-
      c(
        JSESSIONID = coreApi$jsessionId,
        AWSELB = coreApi$awselb
      )

    if (useVerbose) {
      response <-
        httr::with_verbose(httr::PUT(
          sdk_url,
          body = body,
          httr::add_headers(headers),
          httr::set_cookies(cookie)
        ))
    } else {
      response <- httr::PUT(sdk_url,
        body = body,
        httr::add_headers(headers),
        httr::set_cookies(cookie)
      )
    }

    if (httr::http_error(response)) {
      stop({
        print("API call failed")
        print(httr::http_status(response))
      },
      call. = FALSE
      )
    }

    list(entity = "", response = response)
  }
