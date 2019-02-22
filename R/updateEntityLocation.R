# ` updateEntityLocation - Update entity location
#'
#' \code{updateEntityLocation}- Update entity location
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type to get
#' @param barcode barcode of entity to get
#' @param locationBarcode loaction barcode
#' @param useVerbose TRUE or FALSE to indicate if verbose options should be used in http 
#' @return returns a list $entity contains entity information, $response contains the entire http response
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' response <- updateEntityLocation(login$coreApi, "entityType", "barcode", "locationBarcode")
#' entity <- response$entity
#' logOut(login$coreApi)
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code{updateEntityLocation} - Update entity location

updateEntityLocation <-
  function(coreApi,
             entityType,
             barcode,
             locationBarcode,
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


    old_values <- entity$entity



    # no lint start
    old_values[["LOCATION@odata.bind"]] <-
      paste0("/LOCATION", "('", locationBarcode, "')")
    # no lint end

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




    list(entity = httr::content(response), response = response)
  }
