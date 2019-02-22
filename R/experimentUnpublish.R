#' experimentUnpublish Unpublishes an experiment.
#'
#' \code{experimentUnpublish} Unpublishes an experiment.
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentType experiment entity type
#' @param experimentBarcode barcode of the experiment
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains updated experiment information, $response contains the entire http response
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' update <- experimentUnpublish(login$coreApi, experimentType, experimentBarcode, useVerbose = TRUE)
#' logOut(login$coreApi)
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @description \code{experimentUnpublish} - Unpublishes an experiment.






experimentUnpublish <-
  function(coreApi,
             experimentType,
             experimentBarcode,
             useVerbose = FALSE) {
    # build request

    sdkCmd <- jsonlite::unbox("experiment-unpublish")

    data <- list()



    data[["entityRef"]] <-
      list(barcode = jsonlite::unbox(experimentBarcode))



    responseOptions <- c("CONTEXT_GET", "MESSAGE_LEVEL_WARN")
    logicOptions <- c("EXECUTE_TRIGGERS")
    typeParam <- jsonlite::unbox(experimentType)



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
