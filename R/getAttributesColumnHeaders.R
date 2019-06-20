#' getAttributesColumnHeaders - Gets the attribute column header for all or specified attributes in an entityType.
#' \code{getAttributesColumnHeaders} Gets the attribute column header for all or specified attributes in an entityType.
#' @param coreAPI coreApi object with valid jsessionid
#' @param attributeList list of attribute names (usually obtained through getEntityMetadata) to limit the column header names. Default = NULL.
#' @param entityType entity type which has the desired attribute column headers assigned
#' @param ... additional arguments passed to \code{apiGET}
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is a character element with associated column header names.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' experimentAssayType <- getAttributesColumnHeaders(login$coreApi, "CI_TEMPERATURE", "BEER")
#' logOut(login$coreApi)
#' }
#' @author Edgardo Gutierrez edgardo.gutierrez@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Francisco Marin francisco.marin@thermofisher.com
#' @description \code{getAttributesColumnHeaders} - Gets the attribute column header for all or specified attributes in an entityType.

getAttributesColumnHeaders <-
  function(coreApi,
             attributeList = NULL,
             entityType,
             ...) {
    case(
      grepl("[0-2]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        warning(
          paste("getAttributesColumnHeaders OData action not available in", coreApi$semVer),
          call. = FALSE
        )
      },
      grepl("[3-9]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        # clean the name for ODATA
        entityType <- odataCleanName(entityType)

        attrMetadata <- getEntityAttributeMetadata(coreApi, entityType, fullMetadata = FALSE, useVerbose = TRUE)

        colHeaders <-
          lapply(
            attrMetadata$entity,
            FUN = function(x)
              x$ColumnHeader
          )

        escapedName <-
          lapply(
            attrMetadata$entity,
            FUN = function(x)
              x$EscapedName
          )

        returnHeaders <- lapply(seq(length(colHeaders)), function(headerIndex) {
          result <- NULL
          if (is.null(colHeaders[[headerIndex]])) {
            result <- escapedName[headerIndex]
          } else {
            result <- colHeaders[headerIndex]
          }
          result
        })

        returnHeaders <- toupper(unlist(returnHeaders))

        if (is.null(attributeList)) {
          list(entity = returnHeaders, response = attrMetadata$response)
        } else if (all(!is.na(attributeList)) & nchar(attributeList[[1]]) > 0) {
          attributes <- returnHeaders[returnHeaders %in% attributeList]

          list(entity = attributes, response = attrMetadata$response)
        } else {
          warning(paste("Invalid list of attributes:", attributeList))

          list(entity = NULL, response = attrMetadata$response)
        }
      }
    )
  }
