#' apiPOST - Do a POST to the Core ODATA REST API.
#'
#' \code{apiPOST}  Do a POST to the Core ODATA REST API.
#' @param coreApi coreApi object with valid jsessionid
#' @param resource entity type for POST
#' @param body body for request
#' @param encode encode type must be "multipart", "form", "json", "raw"
#' @param headers  headers to be added to get.
#' @param special  passed to buildUrl for special sdk endpoints
#' @param useVerbose  Use verbose communication for debugging
#' @export
#' @return Returns the entire http response
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- apiPOST(login$coreApi, "SAMPLE", body, "json", special = NULL, useVerbose = FALSE)
#' message <- httr::content(response)
#' error <- httr::http_error(response)
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Francisco Marin francisco.marin@thermofisher.com
#' @description \code{apiPOST} - Base call to Core ODATA REST API.




apiPOST <-
  function(coreApi,
             resource = NULL,
             body = NULL,
             encode,
             headers = NULL,
             special = NULL,
             useVerbose = FALSE) {
    # clean the resource name for ODATA

    resource <- odataCleanName(resource)


    # Check that encode parameter is proper

    if (!(encode %in% c("multipart", "form", "json", "raw"))) {
      stop({
        print("encode parameter not recognized")
        print(httr::http_status(response))
      },
      call. = FALSE
      )
    }



    sdk_url <-
      buildUrl(
        coreApi,
        resource = resource,
        special = special,
        useVerbose = useVerbose
      )

    cookie <-
      c(
        JSESSIONID = coreApi$jsessionId,
        AWSELB = coreApi$awselb
      )

    response <-
      invisible(
        httr::POST(
          sdk_url,
          resource = resource,
          body = body,
          encode = encode,
          httr::add_headers(headers),
          httr::set_cookies(cookie),
          httr::verbose(
            data_out = useVerbose,
            data_in = useVerbose,
            info = useVerbose,
            ssl = useVerbose
          )
        )
      )

    # check for general HTTP error in response

    if (httr::http_error(response)) {
      warning("API call failed", call. = FALSE)
      warning(httr::http_status(response), call. = FALSE)
      responseData <- httr::content(response)
      headers <- httr::headers(response)

      if (grepl("application/json", headers[["Content-Type"]], fixed = TRUE) &&
        !is.null(responseData$error)) {
        statusCode <- httr::status_code(response)
        warning(paste0(
          "Status Code: ", statusCode,
          ", Error: ", responseData$error$message
        ),
        call. = FALSE
        )

        if (!is.null(responseData$error$details)) {
          warning(paste0("Additional Details: ", responseData$error$details$message))
        }
      }
    }

    return(response)
  }
