#' coreAPI - Creates a object of class coreAPI that contains user and connection information.
#'
#' \code{coreAPI} Creates a object of class coreAPI that contains user and connection information.
#' @param CoreAccountInfo file with tenant information in json format.
#' @return Object of class coreAPI
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("/home/environment.json")
#' }
#' @details{ Creates a object of class list that contains connection information.
#'          Requires a json formatted datastring.
#'         \code{#'Creates a object of class list that contains connection information}
#'         \code{coreAPI("path to json")}.
#' }
#' The json must include the fields shown below.
#'     \code{
#' {
#'   "values": [
#'     {
#'       "key": "tenant",
#'       "value": "FULLNAMEOFTENANT"
#'     },
#'     {
#'       "key": "alias",
#'       "value": "SHORTNAMEINURL"
#'     },
#'     {
#'       "key": "scheme",
#'       "value": "https"
#'     },
#'     {
#'       "key": "host",
#'       "value": "HOSTNAME"
#'     },
#'     {
#'       "key": "port",
#'       "value": "443"
#'     },
#'     {
#'       "key": "context",
#'       "value": ""
#'     },
#'     {
#'       "key": "api_username",
#'       "value": "USER"
#'     },
#'     {
#'       "key": "api_password",
#'       "value": "PASSWORD"
#'     },
#'     {
#'       "key": "semver",
#'       "value": ""
#'     }
#'     ]
#' }
#'     }
#'
#'  The tenant value may be set to "" if the user only has access to one tenant.
#' As an alternative the environment json object from Postman can be used directly.
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Adam Wheeler, adam.j.wheeler@accenture.com
#' @author Natasha Mora natasha.mora@thermofisher.com
coreAPI <- function(CoreAccountInfo) {
  accountinfo <- jsonlite::fromJSON(CoreAccountInfo)$values
  structure(
    list(
      username = getAccountInfoValue(accountinfo, "api_username"),
      password = getAccountInfoValue(accountinfo, "api_password"),
      tenant = getAccountInfoValue(accountinfo, "tenant"),
      alias = getAccountInfoValue(accountinfo, "alias"),
      context = getAccountInfoValue(accountinfo, "context"),
      host = getAccountInfoValue(accountinfo, "host"),
      port = getAccountInfoValue(accountinfo, "port"),
      scheme = getAccountInfoValue(accountinfo, "scheme"),
      jsessionId = NULL,
      awselb = NULL,
      employeeId = NULL,
      serviceRoot = NULL,
      semVer = getAccountInfoValue(accountinfo, "semver")
    ),
    class = "coreAPI"
  )
}

#' getAccountInfoValue - read a configuration value from a data frame or environment variable
#' @param accountinfo data frame of configuration values
#' @param key name of data frame key that might match an environment variable
#' @return value from the data frame or environment variable
#' @author Scott Russell scott.russell@thermofisher.com
getAccountInfoValue <- function(accountinfo, key) {
  value <- accountinfo$value[accountinfo$key == key]

  if (stringi::stri_isempty(value)) {
    value <- NULL

    envVar <- stringr::str_to_upper(key)
    if (!stringi::stri_isempty(Sys.getenv(envVar))) {
      value <- Sys.getenv(envVar)
    }
  }

  return(value)
}
