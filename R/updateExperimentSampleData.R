#' updateExperimentSampleData - Update experiment sample assay data.
#'
#' \code{updateExperimentSampleData} Update experiment sample assay data.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentAssayType entity type to get as character string
#' @param experimentSampleBarcode User provided barcode as a character string
#' @param experimentAssayUpdateAttrList assay attributes as a list of key-values pairs
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains entity information,
#'        $response contains the entire http response
#' @examples
#' \dontrun{
#' api <- CoreAPI("PATH TO JSON FILE")
#' login <- CoreAPIV2::authBasic(api)
#' response <- CoreAPIV2::updateExperimentSampleData(login$coreApi,
#'   experimentAssayType = "BITTERNESS_ASSAY",
#'   experimentSampleBarcode = "BTES3", experimentAssayUpdateAttrList = list(CI_BITTERNESS_IBU = 9.7, CI_ACCEPT = FALSE)
#' )
#' updatedEntity <- response$entity
#' CoreAPIV2::logOut(login$coreApi)
#' response <- CoreAPI::authBasic(coreApi)
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{updateExperimentSampleData} Update experiment sample assay data.



updateExperimentSampleData <-
  function(coreApi,
             experimentAssayType,
             experimentSampleBarcode,
             experimentAssayUpdateAttrList,
             useVerbose = FALSE) {
    # Clean Names of assay

    experimentAssayType <- CoreAPIV2::odataCleanName(experimentAssayType)


    # Clean Names of attributes


    for (i in 1:length(names(experimentAssayUpdateAttrList)))
    {
      names(experimentAssayUpdateAttrList)[i] <-
        CoreAPIV2::attributeCleanName(names(experimentAssayUpdateAttrList)[i])
    }



    body <- experimentAssayUpdateAttrList # needs to be unboxed

    resource <- paste0(experimentAssayType, "_DATA")
    query <- paste0("('", experimentSampleBarcode, "')")

    header <- c("Content-Type" = "application/json", "If-Match" = "*")

    response <-
      CoreAPIV2::apiPUT(
        coreApi,
        resource = resource,
        query = query,
        body = body,
        encode = "raw",
        headers = header,
        useVerbose = useVerbose
      )


    list(entity = httr::content(response), response = response)
  }
