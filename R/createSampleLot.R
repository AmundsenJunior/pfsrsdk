#' createSampleLot - Creates a lot of a sample.
#'
#' \code{createSampleLot} Creates a sample lot of a sample. Assumes the sample lot entity is in the form samplename_lot
#' @param coreApi coreApi object with valid jsessionid
#' @param sampleType sample type to create the lot of
#' @param sampleBarcode parent sample barcode
#' @param body attributes as list of key-values pairs (optional)
#' @param fullMetadata get full metadata, default is FALSE
#' @param ... additional arguments passed to \code{apiPOST}
#' @export
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the HTTP response content.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' lot <- createSampleLot(login$coreApi, sampleType, sampleBarcode, )
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{createSampleLot} Creates a new sample lot using the parent sample barcode

createSampleLot <-
  function(coreApi,
             sampleType,
             sampleBarcode,
             body = NULL,
             fullMetadata = TRUE,
             ...) {
    # clean the name for ODATA
    sampleType <- odataCleanName(sampleType)

    lotName <- paste0(sampleType, "_LOT")

    case(
      grepl("[0-2]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        dataBind <- "IMPL_LOT_SAMPLE@odata.bind"
      },
      grepl("[3-9]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        dataBind <- "SAMPLE@odata.bind"
      }
    )

    lotRef <- list(dataBind = paste0("/", sampleType, "('", sampleBarcode, "')"))

    names(lotRef) <- dataBind

    fullBody <- jsonlite::toJSON(c(body, lotRef), auto_unbox = TRUE)

    if (fullMetadata) {
      headers <- c("Content-Type" = "application/json", "Accept" = "application/json;odata.metadata=full")
    } else {
      headers <- c("Content-Type" = "application/json")
    }

    response <-
      apiPOST(
        coreApi,
        resource = lotName,
        body = fullBody,
        encode = "json",
        headers = headers,
        special = NULL,
        ...
      )

    list(entity = response$content, response = response$response)
  }
