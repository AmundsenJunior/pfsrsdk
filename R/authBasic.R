#' authBasic - Authenticates against the ODATA REST API using basic authentication.
#'
#' \code{authBasic} Authenticates against the ODATA REST API using basic authentication.
#'
#' @param coreApi object of class coreApi that contains username, password,  baseURL and
#' tenant. tenant is required if user has access to multiple tenants.
#' @param useVerbose - Use verbose settings for HTTP commands
#' @return returns a list with two oblects. coreApi which returns the passed coreApi object with  jsessionid,
#'            awselb and employeeid populated, $response contains the entire http response
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' response <- authBasic(api)
#' login <- response$core$Api
#' error <- httr::http_error(response$response)
#' logOut(response$coreApi, useVerbose = TRUE)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Francisco Marin francisco.marin@thermofisher.com
#' @description \code{authBasic} Logs in and returns a fully populated coreApi object in $coreAPI.



authBasic <- function(coreApi, useVerbose = FALSE) {
  if (is.null(coreApi$tenant)) {
    request <-
      list(request = list(
        data = list(
          lims_userName = jsonlite::unbox(coreApi$username),
          lims_password = jsonlite::unbox(coreApi$password)
        ),
        typeParam = jsonlite::unbox("*"),
        sdkCmd = jsonlite::unbox("sdk-login")
      ))
  } else {
    accountObject <-
      list(
        "entityID" = jsonlite::unbox(""),
        "barcode" = jsonlite::unbox(""),
        "name" = jsonlite::unbox(coreApi$tenant)
      )

    request <-
      list(request = list(
        data = list(
          lims_userName = jsonlite::unbox(coreApi$username),
          lims_password = jsonlite::unbox(coreApi$password),
          accountRef = accountObject
        ),
        typeParam = jsonlite::unbox("*"),
        sdkCmd = jsonlite::unbox("sdk-login")
      ))
  }


  response <-
    apiPOST(
      coreApi,
      body = request,
      encode = "json",
      useVerbose = useVerbose,
      special = "login"
    )

  if (httr::http_error(response)) {
    # The error details are in the response object. The apiPOST function will generate
    # warnings with information of what was wrong.
    warning("Please review details of the authentication error in the response.")

    return(response)
  }

  getSession <- function(response) {
    jsessionid <- httr::content(response)$response$data$jsessionid
    awselb <-
      httr::cookies(response)[which(httr::cookies(response)[, 6] == "AWSELB"), 7]
    if (length(awselb) == 0) {
      awselb <- NULL
    }
    employeeId <- httr::content(response)$response$data$employeeId
    serviceRoot <- httr::content(response)$response$data$serviceRoot

    list(
      jsessionid = jsessionid,
      awselb = awselb,
      employeeId = employeeId,
      serviceRoot = serviceRoot
    )
  }

  session <-
    tryCatch(
      getSession(response),
      error = function(e) {
        list("jsessionid" = NULL, "employeeId" = NULL, "serviceRoot" = NULL)
      }
    )



  if (!is.null(session$jsessionid)) {
    coreApi$jsessionId <- session$jsessionid
  }
  if (!is.null(session$awselb)) {
    coreApi$awselb <- session$awselb
  }
  if (!is.null(session$employeeId)) {
    coreApi$employeeId <- session$employeeId
  }
  if (!is.null(session$serviceRoot)) {
    coreApi$serviceRoot <- session$serviceRoot
  }

  if (is.null(coreApi$semVer)) {
    coreApi$semVer <- getSemVer(coreApi)
    warning(paste("SemVer variable in JSON connection string should be set to", coreApi$semVer))
  }

  if (!any(coreApi$semVer %in% eval(parse(text = getOption("pfs.testedVersions"))))) {
    warning(getOption("pfs.untestedVersionMessage"))
    options("pfs.tested" = FALSE)
  } else {
    options("pfs.tested" = TRUE)
  }
  list(coreApi = coreApi, response = response)
}
