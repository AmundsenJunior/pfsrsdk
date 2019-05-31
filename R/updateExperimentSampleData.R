#' updateExperimentSampleData - Update experiment sample assay data.
#'
#' \code{updateExperimentSampleData} Update experiment sample assay data.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentAssayType entity type to get as character string
#' @param experimentSampleBarcode User provided barcode as a character string
#' @param experimentAssayUpdateAttrList assay attributes as a list of key-values pairs
#' @param ... additional arguments passed to \code{apiPUT}
#' @export
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the HTTP response content of updated sample data.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- updateExperimentSampleData(login$coreApi,
#'   experimentAssayType = "BITTERNESS_ASSAY",
#'   experimentSampleBarcode = "BTES3", experimentAssayUpdateAttrList = list(CI_BITTERNESS_IBU = 9.7, CI_ACCEPT = FALSE)
#' )
#' updatedEntity <- response$entity
#' logOut(login$coreApi)
#' response <- authBasic(coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{updateExperimentSampleData} Update experiment sample assay data.

updateExperimentSampleData <-
  function(coreApi,
             experimentAssayType,
             experimentSampleBarcode,
             experimentAssayUpdateAttrList,
             ...) {
    # Clean Names of assay
    experimentAssayType <- odataCleanName(experimentAssayType)

    # Clean Names of attributes
    for (i in 1:length(names(experimentAssayUpdateAttrList)))
    {
      names(experimentAssayUpdateAttrList)[i] <-
        attributeCleanName(names(experimentAssayUpdateAttrList)[i])
    }
    body <- jsonlite::unbox(experimentAssayUpdateAttrList)

    resource <- paste0(experimentAssayType, "_DATA")
    query <- paste0("('", experimentSampleBarcode, "')")
    header <- c("Content-Type" = "application/json", "If-Match" = "*")

    response <-
      apiPUT(
        coreApi,
        resource = resource,
        query = query,
        body = body,
        encode = "raw",
        headers = header,
        ...
      )

    list(entity = response$content, response = response$response)
  }
