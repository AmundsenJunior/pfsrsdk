#' coreAPI Creates a object of class coreAPI that contains user and connection information.
#' @param CoreAccountInfo file with tenant information in json format.
#' @return Object of class coreAPI
#' @export
#' @examples
#' \dontrun{
#' api<-CoreAPIV2::coreAPI("/home/environment.json")
#' }
#' @details{ Creates a object of class coreAPI that contains username,
#'          password, host, tenant, port.
#'          It has slots for tenant, jsessionId, AWSELB, and host.
#'          Requires a json file that is a POSTMAN environment file.
#'         \code{#'Creates a object of class coreAPI that contains tenant information}
#'         \code{coreAPI("path to json")}.
#' }
#' The json must include the fields shown below.
#'     \code{
#' {
#'   "values": [
#'     {
#'       "key": "tenant",
#'       "value": "FULLNAMEOFTENANT",
#'       "type": "text",
#'       "enabled": true
#'     },    
#'     {
#'       "key": "alias",
#'       "value": "SHORTNAMEINURL",
#'       "type": "text",
#'       "enabled": true
#'     },
#'     {
#'       "key": "scheme",
#'       "value": "https",
#'       "type": "text",
#'       "name": "scheme",
#'       "enabled": true,
#'       "hovered": false
#'     },
#'     {
#'       "key": "host",
#'       "value": "HOSTNAME",
#'       "type": "text",
#'       "name": "host",
#'       "enabled": true,
#'       "hovered": false
#'     },
#'     {
#'       "key": "port",
#'       "value": "443",
#'       "type": "text",
#'       "enabled": true
#'     },
#'     {
#'       "key": "context",
#'       "value": "",
#'       "type": "text",
#'       "enabled": true
#'     },
#'     {
#'       "key": "username",
#'       "value": "USER",
#'       "type": "text",
#'       "name": "nameadmin",
#'       "enabled": true,
#'       "hovered": false
#'     },
#'     {
#'       "key": "password",
#'       "value": "PASSWORD",
#'       "type": "text",
#'       "name": "passwordadmin",
#'       "enabled": true,
#'       "hovered": false
#'     },
#'     {
#'       "key": "semver",
#'       "value": "",
#'       "type": "text",
#'       "enabled": true
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
      username = getAccountInfoValue(accountinfo, "username"),
      password = getAccountInfoValue(accountinfo, "password"),
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
    )

    ,
    class = "coreAPI"
  )
}

getAccountInfoValue <- function(accountinfo, key) {
  value <- accountinfo$value[accountinfo$key == key]
  
  if(stringi::stri_isempty(value)) {
    value <- NULL
  }
  
  envVar <- stringr::str_to_upper(key)
  if(!stringi::stri_isempty(Sys.getenv(envVar))) {
    value <- Sys.getenv(envVar)
  }
  
  return(value)
}
