#' odataCleanName - converts names to ODATA compliant version. Used to clean names in ODATA calls.
#'
#' \code{odataCleanName} Clean a name for ODATA.
#' @param name  string to clean
#' @param refType Reference to the type of object being passed (default: odataObject).
#' @export
#' @section Using \code{refType} parameter:
#' \code{refType} accepts two values, the default "odataObject" and "tenant".
#' "odataObject" ensures that a leading underscore is placed in front of numbers
#' at the beginning of the string to comply with OData Standards. This function
#' can be used for other objects as well to replace spaces and hyphens with 
#' underscores. When \code{refType} is set to "tenant", underscores are
#' substituted for spaces, but leaves hyphens intact.
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
