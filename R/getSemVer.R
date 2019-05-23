#' getSemVer - Retrieves the PFS SemVer and creates a warning with the right value if null.
#'
#' \code{getSemVer} Retrieves the PFS SemVer and creates a warning with the right value if null.
#' @param coreApi coreApi object with valid jsessionid
#' @return RETURN returns semVer containing the semVer of the PFS system.
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' semver <- getSemVer(login$coreApi)
#' logOut(login$coreApi)
#' }
#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{getSemVer} Retrieves the PFS SemVer and creates a warning with the right value if null.


getSemVer <- function(coreApi) {
  resource <- odataCleanName("LIMS('LM1')/CORE_VERSION_NUMBER")

  response <- apiGET(
    coreApi = coreApi,
    resource = resource,
    query = NULL,
    useVerbose = T
  )

  semVer <- response$content

  semVer
}
