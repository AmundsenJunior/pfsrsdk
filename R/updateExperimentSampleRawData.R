#' updateExperimentSampleRawData - Update experiment sample raw data.
#'
#' \code{updateExperimentSampleRawData} Update experiment sample raw data.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentContainerBarcode User provided barcode as a character string
#' @param rawDataCellNum cell (well) number of container
#' @param rawDataValues assay attributes as a list of key-values pairs
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains entity information,
#'        $response contains the entire http response
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- updateExperimentSampleRawData(login$coreApi,
#'   experimentContainerBarcode = "BTCR1", rawDataCellNum = 1,
#'   rawDataValues = list(DATA_VALUE = 100, CI_ACCEPT = FALSE)
#' )
#' 
#' updatedEntity <- response$entity
#' logOut(login$coreApi)
#' response <- authBasic(coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{updateExperimentSampleRawData} Update experiment sample assay raw data.



updateExperimentSampleRawData <-
  function(coreApi,
             experimentContainerBarcode,
             rawDataCellNum,
             rawDataValues,
             useVerbose = FALSE) {

    # get the current values

    resource <- "RAW_DATA"


    query <- paste0(
      "?$filter=CI_CELL%20eq%20",
      as.integer(rawDataCellNum),
      "%20and%20EXPERIMENT_CONTAINER/Name%20eq%20'",
      experimentContainerBarcode,
      "'"
    )


    header <- c(Accept = "application/json")




    response <-
      apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        useVerbose = useVerbose
      )


    body <- response$content[[1]]


    for (i in 1:length(rawDataValues))
    {
      eval(parse(
        text =
          paste0(
            "body$", names(rawDataValues)[i],
            "<-", rawDataValues[i]
          )
      ))
    }



    resource <- paste0("RAW_DATA")
    query <- paste0("(", body$Id, ")")

    header <- c("Content-Type" = "application/json", "If-Match" = "*")

    response <-
      apiPUT(
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
