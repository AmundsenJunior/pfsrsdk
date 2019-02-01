#' setSemVer - Retrieves the PFS SemVer and creates a warning with the right value if null.
#'
#' \code{setSemVer} Retrieves the PFS SemVer and creates a warning with the right value if null.
#' @param coreApi coreApi object with valid jsessionid
#' @export
#' @return RETURN returns semVer containing the semVer of the PFS system.
#' @examples
#' \dontrun{
#' api<-CoreAPIV2("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' semver<-CoreAPIV2::setSemVer(login$coreApi)
#' logOut(login$coreApi )
#' }
#' @author Adam Wheeler, adam.j.wheeler@accenture.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{setSemVer} Retrieves the PFS SemVer and creates a warning with the right value if null.


setSemVer <- function(coreApi){
  resource <- CoreAPIV2::odataCleanName("LIMS('LM1')/CORE_VERSION_NUMBER")
  header <- c('Content-Type' = "application/json;odata.metadata=full", Accept = "application/json")
  response <- CoreAPIV2::apiGET(coreApi = coreApi,
                                resource = resource,
                                query = NULL,
                                headers = header,
                                useVerbose = T)
  semVer <- response$content
  
  semVer
}
