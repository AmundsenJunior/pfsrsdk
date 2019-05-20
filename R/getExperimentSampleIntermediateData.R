#' getExperimentSampleIntermediateData - Gets intermediate data for an experiment sample.
#'
#' \code{getExperimentSampleIntermediateData}  Gets intermediate data for an experiment sample identified by barcode.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentType experiment type for sample
#' @param experimentAssayType assay type for sample
#' @param experimentSampleBarcode experiment sample barcode of entity to get
#' @param intermediateDataName assay  intermediate data name to retrive as configured in the assay.
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http call
#' @return returns a list $entity contains data frame with derived experiment sample barcodes concentration,
#'         and assay raw data. $response contains the entire http response
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- getExperimentSampleIntermediateData(
#'   login$coreApi,
#'   "experimentType",
#'   "experimentAssayType",
#'   "intermediateDataName",
#'   "experimentSampleBarcode"
#' )
#' rawdata <- response$entity
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{getExperimentSampleIntermediateData}   Gets intermediate data for an experiment sample identified by barcode.



getExperimentSampleIntermediateData <-
  function(coreApi,
             experimentType,
             experimentAssayType,
             intermediateDataName,
             experimentSampleBarcode,
             useVerbose = FALSE) {
    # clean the name for ODATA
    experimentType <- odataCleanName(experimentType)
    experimentAssayType <- odataCleanName(experimentAssayType)
    intermediateDataName <- attributeCleanName(intermediateDataName)
    resource <- paste0(experimentType, "_SAMPLE")


    query <- paste0(
      "('",
      experimentSampleBarcode,
      "')?$expand=ASSAY_DATA/pfs.ASSAY_DATA,DERIVED_FROM",
      "($expand=INTERMEDIATE_ASSAY_DATA/pfs.INTERMEDIATE_",
      experimentAssayType,
      "_DATA)"
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

    derivedSamples <- response$content$DERIVED_FROM

    barcodes <- unlist(lapply(derivedSamples, function(x)
      x$Barcode))

    accept <- unlist(lapply(derivedSamples, function(x)
      x$CI_ACCEPT))

    concentration <-
      unlist(lapply(derivedSamples, function(x) {
        if (is.null(x$CI_CONC_NM)) {
          return("")
        } else {
          return(x$CI_CONC_NM)
        }
      }))

    concUnit <-
      unlist(lapply(derivedSamples, function(x) {
        if (is.null(x$CI_CONC_UNIT)) {
          return("")
        } else {
          return(x$CI_CONC_UNIT)
        }
      }))

    time <-
      unlist(lapply(derivedSamples, function(x) {
        if (is.null(x$CI_TIME)) {
          return("")
        } else {
          return(x$CI_TIME)
        }
      }))

    cell <-
      unlist(lapply(derivedSamples, function(x) {
        if (is.null(x$CI_CELL)) {
          return("")
        } else {
          return(x$CI_CELL)
        }
      }))

    id <-
      unlist(lapply(derivedSamples, function(x) {
        return(x$Id)
      }))

    # TODO - Allow users to put more than one intermediateDataName, currently only one can be
    # passed and it has to be written in UPPERCASE in PFS, unless line "intermediateDataName <- attributeCleanName(intermediateDataName)"
    # is commented out.

    dataValues <-
      unlist(lapply(derivedSamples, function(x) {
        if (is.null(eval(parse(
          text =
            paste0("x$INTERMEDIATE_ASSAY_DATA$'", intermediateDataName, "'")
        )))
        ) {
          return("")
        } else {
          return(eval(parse(
            text =
              paste0("x$INTERMEDIATE_ASSAY_DATA$'", intermediateDataName, "'")
          )))
        }
      }))

    entity <-
      data.frame(
        barcodes = barcodes,
        id = id,
        concentration = concentration,
        concUnit = concUnit,
        time = time,
        cell = cell,
        accept = accept
      )

    entity <- entity[order(entity$concentration), ]

    ######## can have different intermediate data must build columns

    eval(parse(text = paste0("entity$'", intermediateDataName, "'<-dataValues")))

    list(entity = entity, response = response$response)
  }


#' getExperimentSamplesIntermediateData - Gets intermediate data for an experiment sample.
#'
#' \code{getExperimentSamplesIntermediateData}  Gets intermediate data for a experiment sample identified by barcode.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentType experiment type for sample
#' @param experimentAssayType assay type for sample
#' @param experimentSampleBarcode experiment sample barcode of entity to get
#' @param intermediateDataName assay  intermediate data name to retrive as configured in the assay.
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http call
#' @return returns a list $entity contains data frame with derived experiment sample barcodes concentration,
#'         and assay raw data. $response contains the entire http response
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- getExperimentSamplesIntermediateData(
#'   login$coreApi,
#'   "experimentType",
#'   "experimentAssayType",
#'   "intermediateDataName",
#'   "experimentSampleBarcode"
#' )
#' rawdata <- response$entity
#' logOut(login$coreApi)
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{getExperimentSamplesIntermediateData}   Gets raw data for a experiment sample identified by barcode.
#'
#' @name getExperimentSamplesIntermediateData-deprecated
#' @seealso \code{\link{pfsrsdk-deprecated}}
#' @keywords internal
NULL

#' @rdname pfsrsdk-deprecated
#' @section \code{getExperimentSamplesIntermediateData}:
#' For \code{getExperimentSamplesIntermediateData}, use \code{\link{getExperimentSampleIntermediateData}}.
#'
#' @export


getExperimentSamplesIntermediateData <-
  function(coreApi,
             experimentType,
             experimentAssayType,
             intermediateDataName,
             experimentSampleBarcode,
             useVerbose = FALSE) {
    .Deprecated(new = "getExperimentSampleIntermediateData")

    # clean the name for ODATA
    experimentType <- odataCleanName(experimentType)
    experimentAssayType <- odataCleanName(experimentAssayType)
    intermediateDataName <- attributeCleanName(intermediateDataName)
    resource <- paste0(experimentType, "_SAMPLE")


    query <- paste0(
      "('",
      experimentSampleBarcode,
      "')?$expand=ASSAY_DATA/pfs.ASSAY_DATA,DERIVED_FROM",
      "($expand=INTERMEDIATE_ASSAY_DATA/pfs.INTERMEDIATE_",
      experimentAssayType,
      "_DATA)"
    )


    header <- c(Accept = "application/json; odata.metadata=minimal")


    response <-
      apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        useVerbose = useVerbose
      )

    derivedSamples <- response$content$DERIVED_FROM

    barcodes <- unlist(lapply(derivedSamples, function(x)
      x$Barcode))

    accept <- unlist(lapply(derivedSamples, function(x)
      x$CI_ACCEPT))

    concentration <-
      unlist(lapply(derivedSamples, function(x) {
        if (is.null(x$CI_CONC_NM)) {
          return("")
        } else {
          return(x$CI_CONC_NM)
        }
      }))

    concUnit <-
      unlist(lapply(derivedSamples, function(x) {
        if (is.null(x$CI_CONC_UNIT)) {
          return("")
        } else {
          return(x$CI_CONC_UNIT)
        }
      }))

    time <-
      unlist(lapply(derivedSamples, function(x) {
        if (is.null(x$CI_TIME)) {
          return("")
        } else {
          return(x$CI_TIME)
        }
      }))

    cell <-
      unlist(lapply(derivedSamples, function(x) {
        if (is.null(x$CI_CELL)) {
          return("")
        } else {
          return(x$CI_CELL)
        }
      }))

    id <-
      unlist(lapply(derivedSamples, function(x) {
        return(x$Id)
      }))

    dataValues <-
      unlist(lapply(derivedSamples, function(x) {
        if (is.null(eval(parse(
          text =
            paste0("x$INTERMEDIATE_ASSAY_DATA$'", intermediateDataName, "'")
        )))
        ) {
          return("")
        } else {
          return(eval(parse(
            text =
              paste0("x$INTERMEDIATE_ASSAY_DATA$'", intermediateDataName, "'")
          )))
        }
      }))

    entity <-
      data.frame(
        barcodes = barcodes,
        id = id,
        concentration = concentration,
        concUnit = concUnit,
        time = time,
        cell = cell,
        accept = accept
      )

    entity <- entity[order(entity$concentration), ]

    ######## can have different intermediate data must build columns

    eval(parse(text = paste0("entity$'", intermediateDataName, "'<-dataValues")))

    list(entity = entity, response = response$response)
  }
