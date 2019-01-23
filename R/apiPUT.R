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
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' response <-CoreAPIV2::apiPUT(login$coreApi,
#'     "SAMPLE",
#'     body,
#'     "json",
#'     special=NULL,
#'     useVerbose=FALSE,
#'     unbox = TRUE)
#' content <- httr::coontent(response)
#' CoreAPIV2::logOut(login$coreApi )
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Adam Wheeler, adam.j.wheeler@accenture.com
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
    resource <- CoreAPIV2::odataCleanName(resource)

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
      CoreAPIV2::buildUrl(
        coreApi,
        resource = resource,
        query = query,
        special = special,
        useVerbose = useVerbose
      ), ifelse(valueFlag, "/$value", "")
    )

    cookie <-
      c(
        JSESSIONID = coreApi$jsessionId,
        AWSELB = coreApi$awselb
      )

    # check to see if the put request is for a file
    if (class(body) != "form_file") {
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
      stop({
        print("API call failed")
        print(httr::http_status(response))
        print(httr::content(response, as = "text"))
      },
      call. = FALSE
      )
    }
    return(response)
  }
