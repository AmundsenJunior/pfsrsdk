#' createSampleLot - Creates a lot of a sample.
#'
#' \code{createSampleLot} Creates a sample lot of a sample. Assumes the sample lot entity is in the form samplename_lot
#' @param coreApi coreApi object with valid jsessionid
#' @param sampleType sample type to create the lot of
#' @param sampleBarcode parent sample barcode
#' @param body attributes as list of key-values pairs (optional)
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains entity information, $response contains the entire http response
#' @examples
#' \dontrun{
#' api <- CoreAPIV2("PATH TO JSON FILE")
#' login <- CoreAPIV2::authBasic(api)
#' lot <- CoreAPIV2::createSampleLot(login$coreApi, "Sample_Name")
#' logOut(login$coreApi)
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Adam Wheeler, adam.j.wheeler@accenture.com
#' @description \code{createSampleLot} Creates a new sample lot using the parent sample barcode





createSampleLot <-
  function(coreApi,
             sampleType,
             sampleBarcode,
             body = NULL,
             useVerbose = FALSE) {
    # clean the name for ODATA

    sampleType <- CoreAPIV2::odataCleanName(sampleType)

    lotName <- paste0(sampleType, "_LOT")

    dataBind <- switch(EXPR = substr(coreApi$semVer, 1, 1),
      "2" = "IMPL_LOT_SAMPLE@odata.bind",
      "3" = "SAMPLE@odata.bind",
      print("SAMPLE@odata.bind")
    )

    switch(EXPR = coreApi$semVer,
      "2.7.1" = NULL,
      "3.0.3" = NULL,
      print(warning("This PFS version has not been tested. Please contact support if any errors arise."))
    )

    lotRef <- list(dataBind = paste0("/", sampleType, "('", sampleBarcode, "')"))

    names(lotRef) <- dataBind

    fullBody <- jsonlite::toJSON(c(body, lotRef), auto_unbox = TRUE)

    headers <-
      c("Content-Type" = "application/json;odata.metadata=full", accept = "application/json")

    response <-
      CoreAPIV2::apiPOST(
        coreApi,
        resource = lotName,
        body = fullBody,
        encode = "json",
        headers = headers,
        special = NULL,
        useVerbose = useVerbose
      )

    list(entity = httr::content(response), response = response)
  }
