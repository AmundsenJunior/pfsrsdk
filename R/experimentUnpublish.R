#' experimentUnpublish Unpublishes an experiment.
#'
#' \code{experimentUnpublish} Unpublishes an experiment.
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentType experiment entity type
#' @param experimentBarcode barcode of the experiment (Experiments that require a signature cannot be published through the API).
#' @param ... additional arguments passed to \code{apiPOST}
#' @export
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the HTTP response content.
#'   For PFS version 5.x.x the \code{entity} value comes from \code{response$content$response$data}
#'   For PFS version 6.x.x the \code{entity} value comes from \code{response$content}
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' update <- experimentUnpublish(login$coreApi, experimentType, experimentBarcode, useVerbose = TRUE)
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{experimentUnpublish} - Unpublishes an experiment.

experimentUnpublish <-
  function(coreApi,
             experimentType,
             experimentBarcode,
             ...) {
    # build request

    case(
      grepl("[0-2]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
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

        resource <- NULL

        body <- jsonlite::toJSON(request)

        encode <- "raw"

        headers <- c(
          "Content-Type" = "application/json",
          Accept = "*/*",
          Cookie = paste0("AWSELB=", coreApi$awselb)
        )

        special <- "json"

        response <-
          apiPOST(
            coreApi,
            resource = resource,
            body = body,
            encode = encode,
            headers = headers,
            special = special,
            ...
          )

        list(entity = response$content$response$data, response = response$response)
      },
      grepl("[3-9]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        resource <-
          paste0(odataCleanName(experimentType), "('", experimentBarcode, "')", "/Experiment.Unpublish")

        body <- jsonlite::toJSON({})

        encode <- "json"

        headers <- c("Content-Type" = "application/json")

        special <- NULL

        response <-
          apiPOST(
            coreApi,
            resource = resource,
            body = body,
            encode = encode,
            headers = headers,
            special = special,
            ...
          )

        list(entity = response$content, response = response$response)
      }
    )
  }
