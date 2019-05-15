#' experimentPublish Publishes an experiment.
#'
#' \code{experimentPublish} Publishes an experiment.
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentType experiment entity type
#' @param experimentBarcode barcode of the experiment (Experiments that require a signature cannot be published through the API).
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains updated experiment information, $response contains the entire http response
#' For PFS version 5.x.x the output is generate as list(entity = httr::content(response)$response$data, response = response)
#' For PFS version 6.x.x the output is generate as list(entity = httr::content(response), response = response)
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' update <- experimentPublish(login$coreApi, experimentType, experimentBarcode, useVerbose = TRUE)
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{experimentPublish} - Publishes an experiment.



experimentPublish <-
  function(coreApi,
             experimentType,
             experimentBarcode,
             useVerbose = FALSE) {
    # build request

    case(
      grepl("[0-2]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        sdkCmd <- jsonlite::unbox("experiment-publish")

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
            useVerbose = useVerbose
          )

        list(
          entity = httr::content(response)$response$data,
          response = response
        )
      },
      grepl("[3-9]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        resource <-
          paste0(odataCleanName(experimentType), "('", experimentBarcode, "')", "/Experiment.Publish")

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
            useVerbose = useVerbose
          )

        list(
          entity = httr::content(response),
          response = response
        )
      }
    )
  }
