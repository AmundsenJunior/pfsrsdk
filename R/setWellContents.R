#' setWellContents -  Puts a sample lot in a container well.
#'
#' \code{setWellContents} Puts a sample lot in a container well.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerType container type
#' @param containerBarcode barcode of a container that IS NOT assigned to an experiment.
#' @param containerWellNum container well number. (In PFS 5.3.8 (semVer 2.7.1) if multi-wells are used, well A1 has to be filled or filled first in order to setWellContents in other cells.)
#' @param sampleLotType sample lot type
#' @param sampleLotBarcode barcode of lot to add to well
#' @param amount amount to add (numeric)
#' @param amountUnit units
#' @param concentration (numeric)
#' @param concentrationUnit concentration units
#' @param ... additional arguments passed to \code{apiPOST}
#' @export
#' @return List of length 2, containing \code{content} and \code{response} objects:
#' \itemize{
#'  \item{\code{content}} is the HTTP response content of updated well information.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' well <- setWellContents(login$coreAPI,
#'   containerType = "CONTAINER", containerBarcode = "96W101", containerWellNum = "1",
#'   sampleLotType = "BEER_SAMPLE_LOT", sampleLotBarcode = "BS1000-1", amount = 1, amountUnit = "ml", concentration = 1,
#'   concentrationUnit = "nM", useVerbose = FALSE
#' )
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{setWellContents} - Puts a cell lot in a container well.

setWellContents <-
  function(coreApi,
             containerType,
             containerBarcode,
             containerWellNum,
             sampleLotType,
             sampleLotBarcode,
             amount,
             amountUnit,
             concentration,
             concentrationUnit,
             ...) {
    # clean the name for ODATA
    containerType <- odataCleanName(containerType)

    containerWellNum <- as.numeric(containerWellNum)
    amount <- as.numeric(amount)
    concentration <- as.numeric(concentration)

    if ((grepl("[0-2]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) & (!(amount %% 1 == 0) | !(concentration %% 1 == 0)))) {
      stop(paste0("Amount: ", amount, " and Concentration: ", concentration, " values have to be of type numeric with no decimal places."))
    }

    # first get the cellID for the well
    cellID <-
      getContainerCellIds(coreApi, containerBarcode, containerType, useVerbose = FALSE)$entity[containerWellNum]

    # get ID for lot number
    lotID <-
      getEntityByBarcode(
        coreApi,
        entityType = sampleLotType,
        barcode = sampleLotBarcode,
        fullMetadata = FALSE,
        useVerbose = TRUE
      )$entity$Id

    body <- list()

    cells <-
      list(c(
        list(
          cellId = jsonlite::unbox(cellID),
          amount = jsonlite::unbox(amount),
          amountUnit = jsonlite::unbox(amountUnit),

          contents = list(c(
            list(
              lotId = jsonlite::unbox(lotID),
              concentration = jsonlite::unbox(concentration),
              concentrationUnit = jsonlite::unbox(concentrationUnit)
            )
          ))
        )
      ))

    body[["cells"]] <- cells

    query <-
      paste0(
        "CONTAINER('",
        containerBarcode,
        "')/pfs.Container.SetCellContents"
      )

    response <-
      apiPOST(
        coreApi,
        resource = query,
        body = body,
        encode = "json",
        ...
      )

    list(entity = response$content, response = response$response)
  }
