#' updateCellContents -  Updates amount and concentrations of existing cell.
#'
#' \code{updateCellContents} Updates amount and concentrations of existing cell.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerType container entity type
#' @param containerBarcode container barcode
#' @param containerCellNum container cell number
#' @param sampleLotBarcode barcode of sample lot to add to cell
#' @param amount amount to add (numeric)
#' @param amountUnit units
#' @param concentration (numeric)
#' @param concentrationUnit concentration units
#' @param useVerbose use verbose communications for debugging
#' @return RETURN returns a list $entity contains updated container information,
#'   $response contains the entire http response
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- updateCellContents(
#'   login$coreApi,
#'   "96 WELL PLATE",
#'   "96W78",
#'   "1",
#'   "SMSA7-1",
#'   "10.1",
#'   "mL",
#'   "0.5",
#'   "nM"
#' )
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{updateCellContents} - Updates amount and concentrations of existing cell.
#' @name updateCellContents-deprecated
#' @seealso \code{\link{pfsrsdk-deprecated}}
#' @keywords internal
NULL

#' @rdname pfsrsdk-deprecated
#' @section \code{updateCellContents}:
#' For \code{updateCellContents}, use \code{\link{setCellContents}}.
#'
#' @export

updateCellContents <-
  function(coreApi,
             containerType,
             containerBarcode,
             containerCellNum,
             sampleLotBarcode,
             amount,
             amountUnit,
             concentration,
             concentrationUnit,
             useVerbose = FALSE) {
    sdkCmd <- jsonlite::unbox("update-cell")

    data <- list()
    data[["amount"]] <- jsonlite::unbox(amount)
    data[["amountUnit"]] <- jsonlite::unbox(amountUnit)
    data[["concentration"]] <- jsonlite::unbox(concentration)
    data[["concentrationUnit"]] <- jsonlite::unbox(concentrationUnit)

    data[["cellRefs"]] <-
      list(c(list(
        cellNum = jsonlite::unbox(containerCellNum),
        containerRef = list(barcode = jsonlite::unbox(containerBarcode))
      )))
    data[["lotRef"]] <-
      list(barcode = jsonlite::unbox(sampleLotBarcode))

    responseOptions <-
      c(
        "CONTEXT_GET",
        "MESSAGE_LEVEL_WARN",
        "INCLUDE_CONTAINER_CELL_CONTENTS"
      )
    logicOptions <- list()
    typeParam <- jsonlite::unbox(containerType)

    request <-
      list(
        request = list(
          sdkCmd = sdkCmd,
          data = data,
          typeParam = typeParam,
          responseOptions = responseOptions,
          logicOptions = logicOptions
        )
      )

    headers <- c(
      "Content-Type" = "application/json",
      Accept = "*/*",
      Cookie = paste0("AWSELB=", coreApi$awselb)
    )

    response <-
      apiPOST(
        coreApi,
        resource = NULL,
        body = jsonlite::toJSON(request),
        encode = "raw",
        headers = headers,
        special = "json",
        useVerbose = useVerbose
      )

    list(
      entity = httr::content(response)$response$data,
      response = response
    )
  }
