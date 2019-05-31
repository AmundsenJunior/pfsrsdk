# ` updateEntityAttributes - Update entity attributes
#'
#' \code{updateEntityAttributes} Update entity assoications.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type to get
#' @param barcode barcode of entity to get
#' @param updateValues vaules to update as list of value pairs
#' @param ... additional arguments passed to \code{apiPUT}
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the HTTP response content of updated entity information.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' updateValues <- list(SOURCE_LAB = "My Lab", REQUESTOR = "you")
#' response <- updateEntityAttributes(login$coreApi, "entityType", "barcode", updateValues)
#' updatedEntity <- response$entity
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{updateEntityAttributes}  Update entity attributes.

updateEntityAttributes <-
  function(coreApi,
             entityType,
             barcode,
             updateValues,
             ...) {
    query <- paste0("('", barcode, "')")

    # Get entityType
    entity <-
      getEntityByBarcode(coreApi,
        entityType,
        barcode,
        fullMetadata = FALSE,
        useVerbose = TRUE
      )
    old_values <- entity$entity

    # check to see if all values to update are in the entity
    # replace values
    if (table(names(updateValues) %in% names(old_values))["TRUE"] != length(names(updateValues))) {
      stop({
        print("Names of values to update don't match entity names ")
        print(names(updateValues))
        print(names(old_values))
      },
      call. = FALSE
      )
    }

    namesToUpdate <- names(updateValues)

    for (i in 1:length(namesToUpdate))
    {
      old_values[[namesToUpdate[i]]] <- updateValues[[i]]
    }

    body <- old_values
    query <- paste0("('", barcode, "')")
    header <- c("Content-Type" = "application/json", "If-Match" = "*")

    # update record
    response <-
      apiPUT(
        coreApi,
        resource = entityType,
        query = query,
        body = body,
        encode = "raw",
        headers = header,
        ...
      )

    list(entity = response$content, response = response$response)
  }
