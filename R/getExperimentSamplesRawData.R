#' getExperimentSamplesRawData - Gets raw data for an experiment container.
#'
#' \code{getExperimentSamplesRawData }  Gets raw data for a experiment container identified by barcode.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentContainerBarcode experiment sample container of entity to get
#' @param fullMetadata - get full metadata, default is FALSE
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http call
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is a data frame with derived experiment sample barcodes, concentration, and assay raw data.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- getExperimentSamplesRawData(login$coreApi,
#'   "experimentContainerBarcode",
#'   useVerbose = FALSE
#' )
#' rawdata <- response$entity
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{ getExperimentSamplesRawData }   Gets raw data for an experiment container identified by barcode.



getExperimentSamplesRawData <-
  function(coreApi,
             experimentContainerBarcode,
             fullMetadata = FALSE,
             useVerbose = FALSE) {
    resource <- "RAW_DATA"


    query <- paste0(
      "?$filter=EXPERIMENT_CONTAINER/Barcode%20eq%20'",
      experimentContainerBarcode,
      "'"
    )


    if (fullMetadata) {
      header <- c(Accept = "application/json;odata.metadata=full")
    } else {
      header <- c(Accept = "application/json;odata.metadata=minimal")
    }


    response <-
      apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        useVerbose = useVerbose
      )


    dataValues <- lapply(response$content, unlist)

    dataValues <- lapply(dataValues, function(x) {
      names(x) <- NULL
      return(x)
    })

    dataValues <- t(matrix(unlist(dataValues), ncol = length(response$content), nrow = length(response$content[[1]])))

    colnames(dataValues) <- names(response$content[[1]])

    dataValues <- as.data.frame(dataValues)



    list(entity = dataValues, response = response$response)
  }
