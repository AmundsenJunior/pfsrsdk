#' odataCleanName - converts names to ODATA compliant version. Used to clean names in ODATA calls.
#'
#' \code{odataCleanName} Clean a name for ODATA.
#' @param name  string to clean
#' @param refType Reference to the type of object being passed.
#' @export
#' @section Handling \code{refType} parameter:
#' As of now the \code{refType} parameter defaults to the Value of "odataObject"
#' This will ensure that the leading underscode is placed in front of numbers
#' at the beginning of the odata string to comply with Odata Standards. However,
#' This function can be used for other objects as well to replace spaces and
#' hyphens with underscores.
#'
#' @return Returns name in ODATA compliant form
#' @examples
#' \dontrun{
#' new_name <- odataCleanName("384 Well Plate")
#' # returns "_384_WELL_PLATE"
#' new_name <- odataCleanName("384 Well Plate", "tenant")
#' # returns "384_WELL_PLATE"
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{odataCleanName} - converts names to ODATA compliant version. Used to clean names in ODATA calls.

odataCleanName <- function(name, refType = "odataObject") {
  if (refType == "odataObject") name <- gsub("(^[1-9])", "_\\1", name)

  if (refType == "tenant") {
    name <- gsub(" ", "_", name)
  } else {
    name <- gsub(" |-", "_", name)
  }
}


#' ODATAcleanName - converts names to ODATA compliant version. Used to clean names in ODATA calls.
#'
#' \code{ODATAcleanName} Clean a name for ODATA.
#' @param name  string to clean
#' @return Returns name in ODATA compliant form
#' @examples
#' \dontrun{
#' new_name <- ODATAcleanName("384 Well Plate")
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code{ODATAcleanName} - converts names to ODATA compliant version. Used to clean names in ODATA calls.
#'
#' @name pfsrsdk-deprecated
#' @seealso \code{\link{pfsrsdk-deprecated}}
#' @keywords internal
NULL

#' @rdname pfsrsdk-deprecated
#' @section \code{ODATAcleanName}:
#' For \code{ODATAcleanName}, use \code{\link{odataCleanName}}.
#'
#' @export
ODATAcleanName <- function(name) {
  .Deprecated(new = "odataCleanName")

  name <- gsub("(^[1-9])", "_\\1", name)
  name <- gsub(" ", "_", name)

  name
}
