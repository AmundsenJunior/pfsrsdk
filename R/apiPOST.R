#' apiPOST - Do a POST to the PFS API.
#'
#' \code{apiPOST}  Do a POST to the PFS API.
#' @param coreApi coreApi object with valid jsessionid
#' @param resource entity type for POST
#' @param body body for request
#' @param encode encode type must be "multipart", "form", "json", "raw"
#' @param headers  headers to be added to \code{httr::POST}.
#' @param special  passed to buildUrl for special sdk endpoints
#' @param useVerbose  Use verbose communication for debugging
#' @param fullReturn Return the entire response object, or just the response content (default TRUE)
#' @export
#' @return List of length 2, containing \code{content} and \code{response} objects:
#' \itemize{
#'  \item{\code{content}} is the HTTP response content.
#'  \item{\code{response}} is the entire HTTP response. NULL if \code{fullReturn} is FALSE.
#' }
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- apiPOST(login$coreApi, "SAMPLE", body, "json", special = NULL, useVerbose = FALSE)
#' content <- response$content
#' error <- response$error$message
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Francisco Marin francisco.marin@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{apiPOST} - Do a POST to the PFS API.

apiPOST <-
  function(coreApi,
             resource = NULL,
             body = NULL,
             encode,
             headers = NULL,
             special = NULL,
             useVerbose = FALSE,
             fullReturn = TRUE) {
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
        special = special
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

    if (fullReturn) {
      out <- list(content = httr::content(response), response = response)
    } else {
      out <- list(content = httr::content(response), response = NULL)
    }

    return(out)
  }
