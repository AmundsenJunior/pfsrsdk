# ` updateEntityAssociations - Update entity assoications
#'
#' \code{updateEntityAssociations} Update entity assoications
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type to get
#' @param barcode barcode of entity to get
#' @param updateValues values to update as list of associations contex and entity type pair and barcode
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} is the HTTP response content of udpated entity associations.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' updateValues <- list(SAMPLE_ENZYME = c("ENZYME", "ENZ2"))
#' response <- updateEntityAssociations(login$coreApi, "entityType", "barcode", updateValues)
#' udatedEntity <- response$entity
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{updateEntityAssociations}  Update entity associations.

updateEntityAssociations <-
  function(coreApi,
             entityType,
             barcode,
             updateValues,
             useVerbose = FALSE) {
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
    newAssociations <- list()
    namesToUpdate <- names(updateValues)

    for (i in 1:length(namesToUpdate))
    {
      name <- paste0(namesToUpdate[i], "@odata.bind")

      value <-
        paste0("/", updateValues[[namesToUpdate[i]]][1], "('", updateValues[[namesToUpdate[i]]][2], "')")

      newAssociations[[name]] <- value
    }

    old_values <- c(old_values, newAssociations)

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
        useVerbose = useVerbose
      )

    list(entity = response$content, response = response$response)
  }
