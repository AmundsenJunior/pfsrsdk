#' loadXMLconfigFile- Loads an XML config file
#'
#' \code{xmlFilePathe}  Loads an XML config file
#' @param coreApi coreApi object with valid jsessionid
#' @param xmlFilePath Path to XML file
#' @param appName name of PFS app to enable
#' @export
#' @return RETURN returns the entire http response
#' @examples
#' \dontrun{
#' api<-CoreAPIV2::coreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' loadXMLconfigFile(coreApi,"PathtoFile/test.xml")
#' CoreAPIV2::logOut(login$coreApi )
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{xmlFilePath} Loads an XML config file.

loadXmlConfigFile <- function(coreApi, xmlFilePath = NULL, appName = NULL) {
  cookie <-
    c(
      JSESSIONID = coreApi$jsessionId,
      AWSELB = coreApi$awselb
    )

  header <- c("Content-Type" = "multipart/form-data")

  url <-
    paste0(
      coreApi$scheme,
      "://", coreApi$host,
      ":", coreApi$port,
      "/",
      ifelse(!stringi::stri_isempty(coreApi$context), paste0(coreApi$context, "/"), ""),
      "corelims"
    )

  body <- list(
    cmd = "limsXmlConfigLoad",
    entityType = "LIMS",
    forceConfig = "yes",
    xmlConfigType = "apps",
    if (!is.null(xmlFilePath)) file <- httr::upload_file(path = xmlFilePath, type = "application/xml")
  )
  if (!is.null(appName)) body[[appName]] <- "on"

  xmlpost <-
    httr::POST(
      url = url,
      body = body,
      httr::add_headers(header),
      httr::set_cookies(cookie)
    )
}
