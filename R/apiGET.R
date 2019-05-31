#' apiGET - Do a GET from the PFS API.
#'
#' \code{apiGET} Do a GET from the PFS API.
#' @param coreApi coreApi object with valid jsessionid
#' @param resource entity type to get
#' @param query query string
#' @param headers  headers to be added to \code{httr::GET}.
#' @param special passed to buildUrl for special sdk endpoints
#' @param useVerbose Use verbose communication for debugging (default FALSE)
#' @param useRaw Return raw response content (default FALSE)
#' @param fullReturn Return the entire response object, or just the response content (default TRUE)
#' @export
#' @return List of length 2, containing \code{content} and \code{response} objects:
#' \itemize{
#'  \item{\code{content}} is the HTTP response content (concatenated if a chunked response).
#'  \item{\code{response}} is the entire HTTP response. If chuncked, \code{response} object contains
#' the full HTTP response of the last HTTP GET call. NULL if \code{fullReturn} is FALSE.
#' }
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- apiGET(login$coreApi, "json", resource, query)
#' content <- response$content
#' error <- response$error$message
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Francisco Marin francisco.marin@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{apiGET} - Do a GET from the PFS API.


apiGET <-
  function(coreApi,
             resource,
             query,
             headers = NULL,
             special = NULL,
             useVerbose = FALSE,
             useRaw = FALSE,
             fullReturn = TRUE) {
    # clean the resource name for ODATA
    resource <- odataCleanName(resource)

    sdk_url <-
      buildUrl(
        coreApi,
        resource = resource,
        query = query,
        special = special
      )
    base_sdk_url <-
      sdk_url # need if we need to build url for additional chunks

    cookie <-
      c(
        JSESSIONID = coreApi$jsessionId,
        AWSELB = coreApi$awselb
      )

    # Get first response

    if (useVerbose) {
      response <-
        httr::with_verbose(httr::GET(
          sdk_url,
          httr::add_headers(headers),
          httr::set_cookies(cookie)
        ))
    } else {
      response <- httr::GET(
        sdk_url,
        httr::add_headers(headers),
        httr::set_cookies(cookie)
      )
    }

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

    # determine if this should be raw data
    if (useRaw) {
      content <- httr::content(response, "raw")
    } else {
      # determine if this is a chunked response
      if (!is.null(httr::headers(response)$"transfer-encoding")) {
        chunked <-
          (httr::headers(response)$"transfer-encoding" == "chunked")
      } else {
        chunked <- FALSE
      }

      chunked <-
        !is.null(httr::content(response)$`@odata.nextLink`) ### added to account for chunked header when not really chunked

      # two methods for chunked and not chunked
      # it appears sometimes we get a content$value and sometimes we get just content

      if (!chunked) {
        # not chunked response

        if (is.null(httr::content(response)[["value"]])) {
          content <- httr::content(response)
        } else {
          content <- httr::content(response)$value
        }
      } else {
        # chunked response
        more_content <- TRUE # flag for more chunks
        content <- httr::content(response)$value


        while (more_content) {
          # build url for next chunk
          sdk_url <- httr::content(response)$`@odata.nextLink`
          # get next data chunk

          if (useVerbose) {
            response <-
              httr::with_verbose(
                httr::GET(
                  sdk_url,
                  httr::add_headers(headers),
                  httr::set_cookies(cookie)
                )
              )
          } else {
            response <- httr::GET(
              sdk_url,
              httr::add_headers(headers),
              httr::set_cookies(cookie)
            )
          }

          # add content
          content <- c(content, httr::content(response)$value)

          # Is there more content ?
          more_content <-
            !is.null(httr::content(response)$`@odata.nextLink`)
        }
      }
    }

    if (fullReturn) {
      out <- list(content = content, response = response)
    } else {
      out <- list(content = content, response = NULL)
    }

    return(out)
  }
