#' setExperimentSampleAssayFileData - Puts file attached as assay data into an experiment sample.
#'
#' \code{setExperimentSampleAssayFileData} Puts file attached as assay data into an experiment sample.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param assayType assay type that contains sample
#' @param experimentSampleBarcode experiment sample barcode of entity to get
#' @param attributeName  Name of the attribute that containts the file data
#' @param filePath path to file to upload
#' @param ... additional arguments passed to \code{apiPUT}
#' @return List of length 2, containing \code{content} and \code{response} objects:
#' \itemize{
#'  \item{\code{content}} is the HTTP response content. If the HTTP response status code is 204, returns NULL.
#'  \item{\code{response}} is the entire HTTP response.
#' }
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
#' @author Craig Parman info@ngsanalytics.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{setExperimentSampleAssayFileData} Puts file attached as assay data into an experiment sample.

setExperimentSampleAssayFileData <-
  function(coreApi,
             assayType,
             experimentSampleBarcode,
             attributeName,
             filePath,
             ...) {
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
        valueFlag = valueFlag,
        ...
      )

    list(
      entity = if (response$response$status_code == 204) NULL else response$content,
      response = response$response
    )
  }
