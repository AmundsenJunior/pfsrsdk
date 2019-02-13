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
#' api <- CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' response <- CoreAPIV2::authBasic(api)
#' login <- response$core$Api
#' error <- httr::http_error(response$response)
#' CoreAPIV2::logOut(response$coreApi, useVerbose = TRUE)
#' }
#' @author Craig Parman ngsAnalytics, ngsanaltics.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Adam Wheeler, adam.j.wheeler@accenture.com
#' @author Natasha Mora natasha.mora@thermofisher.com
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
    CoreAPIV2::apiPOST(
      coreApi,
      body = request,
      encode = "json",
      useVerbose = useVerbose,
      special = "login"
    )
  
  
  
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
  
  if (httr::http_error(response)) {
    stop({
      print("API call failed")
      print(httr::http_status(response))
    },
    call. = FALSE
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
  
  if (is.null(coreApi$semVer)){
    coreApi$semVer <- getSemVer(coreApi)
    warning(paste('SemVer variable in JSON connection string should be set to', coreApi$semVer))
  }
  
  if (!any(coreApi$semVer %in% getOption("pfs.testedVersions"))) {
    warning(getOption("pfs.untestedVersionMessage"))
    options("pfs.tested" = FALSE)
  } else {
    options("pfs.tested" = TRUE)
  }
  list(coreApi = coreApi, response = response)
}
