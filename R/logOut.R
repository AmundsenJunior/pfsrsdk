#' logOut -Log user out of the LIMS.
#'
#' \code{logOut} logs user out of the Core API
#'
#' @param coreApi coreApi object returned during log in
#' @param ... additional arguments passed to \code{apiPOST}
#' @return returns list with $success = "Success" when sucessful, $response contains the entire http response
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' response <- authBasic(api)
#' logOut <- logOut(response$coreApi, useVerbose = TRUE)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @description \code{logOut} logs out of the current session.



### Log out

logOut <- function(coreApi, ...) {
  request <-
    list(
      request = list(
        sdkCmd = jsonlite::unbox("sdk-logout"),
        typeParam = jsonlite::unbox("*"),
        data = NULL
      ),
      responseOptions = list()
    )

  response <-
    apiPOST(
      coreApi = coreApi,
      body = request,
      encode = "json",
      special = "login",
      ...
    )

  list(
    success = httr::http_status(response$response)$category,
    response = response$response
  )
}
