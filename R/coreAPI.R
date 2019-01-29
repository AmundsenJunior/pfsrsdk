#' coreAPI Creates a object of class coreAPI that contains user and connection information.
#' @param CoreAccountInfo file with tenant information in json format.
#' @return Object of class coreAPI
#' @export
#' @examples
#' \dontrun{
#' api<-CoreAPIV2::coreAPI("/home/environment.json")
#' }
#' @details{ Creates a object of class coreAPI that contains username,
#'          password, base url, tenant, port.
#'          It has slots for tenant, jsessionId, AWSELB, and base URL.
#'          Requires a json file that is a POSTMAN environment file.
#'         \code{#'Creates a object of class coreAPI that contains tenant information}
#'         \code{coreAPI("path to json")}.
#' }
#' The json must include the fields shown below.
#'     \code{
#'      {
#'    "values": [
#'  {
#'    "key": "tenant",
#'    "value": "R-Integration_Baseline"
#'  },
#'  {
#'    "key": "alias",
#'    "value": "bp2",
#'    "type": "text",
#'    "enabled": true
#'  },
#'  {
#'    "key": "scheme",
#'    "value": "https"
#'  },
#'  {
#'    "key": "host",
#'    "value": "lims.ccc.cloud"
#'  },
#'  {
#'    "key": "context",
#'    "value": ""
#'  },
#'  {
#'    "key": "username",
#'   "value": "yyyy"
#'  },
#'  {
#'    "key": "password",
#'    "value": "xxxxx"
#'  },
#'  {
#'    "key": "port",
#'    "value": "443"
#'  }
#'  ]
#' }
#'     }
#'
#'  The tenant value may be set to "" if the user only has access to one tenant.
#' As an alternative the environment json object from Postman can be used directly.
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Scott Russell scott.russell@thermofisher.com
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
      serviceRoot = NULL
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
  
  return(value)
}
