#' getExperimentSamplesRawData - Gets raw data for an experiment container.
#'
#' \code{getExperimentSamplesRawData }  Gets raw data for a experiment container identified by barcode.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentContainerBarcode experiment sample container of entity to get
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http call
#' @return returns a list $entity contains data frame with derived experiment sample barcodes concentration,
#'         and assay raw data. $response contains the entire http response
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
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{ getExperimentSamplesRawData }   Gets raw data for a experiment container identified by barcode.



getExperimentSamplesRawData <-
  function(coreApi,
             experimentContainerBarcode,
             useVerbose = FALSE) {
    resource <- "RAW_DATA"


    query <- paste0(
      "?$filter=EXPERIMENT_CONTAINER/Barcode%20eq%20'",
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
