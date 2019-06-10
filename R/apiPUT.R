#' apiPUT - Do a PUT to the Core ODATA REST API.
#'
#' \code{apiPUT}  Do a PUT to the Core ODATA REST API.
#' @param coreApi coreApi object with valid jsessionid
#' @param resource entity type for PUT
#' @param query query string
#' @param body body for request
#' @param encode encoding to use for request option are "multipart", "form", "json", "raw"
#' @param headers  headers to be added to get.
#' @param special  passed to buildUrl for special sdk endpoints
#' @param useVerbose  Use verbose communication for debugging
#' @param unbox use autounbox when doing lait yo json conversion
#' @param valueFlag Tells the PUT if there needs to be a /$value added to the end.
#' @export
#' @return Returns the entire http response
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- apiPUT(login$coreApi,
#'   "SAMPLE",
#'   body,
#'   "json",
#'   special = NULL,
#'   useVerbose = FALSE,
#'   unbox = TRUE
#' )
#' content <- httr::content(response)
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Francisco Marin francisco.marin@thermofisher.com
#' @description \code{apiPUT} - Base PUT call to Core ODATA REST API.

apiPUT <-
  function(coreApi,
             resource = NULL,
             query = NULL,
             body = NULL,
             encode,
             headers = NULL,
             special = NULL,
             useVerbose = FALSE,
             unbox = TRUE,
             valueFlag = FALSE) {
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

    sdk_url <- paste0(
      buildUrl(
        coreApi,
        resource = resource,
        query = query,
        special = special,
        useVerbose = useVerbose
        # nolint
      ), ifelse(valueFlag, "/$value", "")
    )

    cookie <-
      c(
        JSESSIONID = coreApi$jsessionId,
        AWSELB = coreApi$awselb
      )

    # check to see if the put request is for a file
    if (!"form_file" %in% class(body)) {
      body <- jsonlite::toJSON(body, auto_unbox = unbox, null = "null")
    }

    response <-
      httr::PUT(
        url = sdk_url,
        body = body,
        httr::set_cookies(cookie),
        encode = encode,
        httr::add_headers(headers),
        httr::verbose(
          data_out = useVerbose,
          data_in = useVerbose,
          info = useVerbose,
          ssl = useVerbose
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
