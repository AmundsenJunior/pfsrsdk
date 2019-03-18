# ` updateEntityProject - Update entity project association
#'
#' \code{updateEntityProject}- Update entity project association
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type to get
#' @param barcode barcode of entity to get
#' @param projectBarcodes one or more project barcodes to associate to the entity
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http
#' @return returns a list $entity contains entity information, $response contains the entire http response
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- updateEntityProject(login$coreApi, "entityType", "barcode", "locationBarcode")
#' entity <- response$entity
#' logOut(login$coreApi)
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code{updateEntityProject} - Update entity project associations.  Does not perserve current associations.




updateEntityProject <-
  function(coreApi,
             entityType,
             barcode,
             projectBarcodes,
             useVerbose = FALSE) {
    query <- paste0("('", barcode, "')")

    # Get entityType

    entity <-
      getEntityByBarcode(coreApi,
        entityType,
        barcode,
        fullMetadata = FALSE,
        useVerbose = useVerbose
      )


    old_values <-
      lapply(entity$entity, function(x)
        if ((length(x) <= 1)) {
          jsonlite::unbox(x)
        } else {
          x
        })

    # no lint start

    for (i in 1:length(projectBarcodes))
    {
      projectBarcodes[i] <-
        paste0("/PROJECT", "('", projectBarcodes[i], "')")
    }
    # no lint end
    old_values[["PROJECT@odata.binding"]] <- projectBarcodes


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
        useVerbose = useVerbose,
        unbox = FALSE
      )



    list(entity = httr::content(response), response = response)
  }
