#' buildUrl - build URL for call to Core ODATA API.
#'
#' \code{buildUrl}  build URL for call to Core ODATA API.
#' @param coreApi coreApi object with valid jsessionid
#' @param resource resource path (required except for special requests)
#' @param query and additional property options (optional)
#' @param special flag for special sdk endpoints
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return RETURN Core REST URL
#' @examples
#' \dontrun{
#' api<-CoreAPIV2("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' URL <-CoreAPIV2::buildUrl(api,"Sample","('PS1')")
#' logOut(login$coreApi )
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{buildUrl} build URL for call to Core REST API.





buildUrl <-
  function(coreApi,
             resource = NULL,
             query = NULL,
             special = NULL,
             useVerbose = FALSE) {

    # Concat account and odata
    if (coreApi$account == "PLATFORM ADMIN") {
      odat <- "/odata/"
    } else if (!is.null(coreApi$account) && is.null(coreApi$alias)) {
      odat <- paste0("/", odataCleanName(coreApi$account), "/odata/")
    } else if (!is.null(coreApi$alias)) {
      odat <- paste0("/", odataCleanName(coreApi$alias), "/odata/")
    } else {
      odat <- "/odata/"
    }

    # Add URL context to path
    if (!is.null(coreApi$context)) {
      ctx <- paste0("/", coreApi$context)
    } else {
      ctx <- ""
    }


    if (is.null(special)) {
      sdk_url <-
        paste0(
          coreApi$scheme,
          "://",
          coreApi$coreUrl,
          ":",
          coreApi$port,
          ctx,
          odat,
          resource,
          query
        )
    } else {
      switch(
        special,
        login = sdk_url <-
          paste0(
            coreApi$scheme,
            "://",
            coreApi$coreUrl,
            ":",
            coreApi$port,
            ctx,
            "/odatalogin"
          ),
        json = sdk_url <-
          paste0(
            coreApi$scheme,
            "://",
            coreApi$coreUrl,
            ":",
            coreApi$port,
            ctx,
            "/sdk"
          )
      )
    }

    return(sdk_url)
  }
