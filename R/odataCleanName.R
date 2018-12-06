#' odataCleanName - converts names to ODATA compliant version. Used to clean names in ODATA calls.
#'
#' \code{odataCleanName} Clean a name for ODATA.
#' @param name  string to clean
#' @export
#' @return Returns name in ODATA compliant form
#' @examples
#' \dontrun{
#' new_name <-CoreAPIV2::odataCleanName("384 Well Plate")
#' new_name
#' _384_WELL_PLATE
#'  }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code{odataCleanName} - converts names to ODATA compliant version. Used to clean names in ODATA calls.



odataCleanName <- function(name) {
  name <- gsub("(^[1-9])", "_\\1", name)

  name <- gsub(" ", "_", name)


  name
}


#' ODATAcleanName - converts names to ODATA compliant version. Used to clean names in ODATA calls.
#'
#' \code{ODATAcleanName} Clean a name for ODATA.
#' @title ODATA clean Name
#' @param name  string to clean
#' @return Returns name in ODATA compliant form
#' @examples
#' \dontrun{
#' new_name <-CoreAPIV2::ODATAcleanName("384 Well Plate")
#' new_name
#' _384_WELL_PLATE
#'  }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code{ODATAcleanName} - converts names to ODATA compliant version. Used to clean names in ODATA calls.
#' 
#' @name ODATAcleanName-deprecated
#' @seealso \code{\link{CoreAPIV2-deprecated}}
#' @keywords internal
NULL

#' @rdname CoreAPIV2-deprecated
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
