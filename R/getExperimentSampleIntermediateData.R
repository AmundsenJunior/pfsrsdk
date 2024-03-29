#' getExperimentSampleIntermediateData - Gets intermediate data for an experiment sample.
#'
#' \code{getExperimentSampleIntermediateData}  Gets intermediate data for an experiment sample identified by barcode.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentType experiment type for sample
#' @param experimentAssayType assay type for sample
#' @param experimentSampleBarcode experiment sample barcode of entity to get
#' @param intermediateDataName assay  intermediate data name to retrive as configured in the assay.
#' @param ... additional arguments passed to \code{apiGET}
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is a data frame with derived experiment sample barcodes, concentration, and assay intermediate data.
#'  \item{\code{response}} is the entire HTTP response.
#' }
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
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{getExperimentSampleIntermediateData}   Gets intermediate data for an experiment sample identified by barcode.

getExperimentSampleIntermediateData <-
  function(coreApi,
             experimentType,
             experimentAssayType,
             intermediateDataName,
             experimentSampleBarcode,
             ...) {
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

    response <-
      apiGET(
        coreApi,
        resource = resource,
        query = query,

        ...
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
