#' attachFile - Attaches a file to an entity or file attribute.
#'
#' \code{attachFile}  Attaches a file to an entity or file attribute.
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType PFS entity type
#' @param barcode User provided barcode as a character string
#' @param filePath path to the file to attach
#' @param targetAttributeName - if included the name if the attribute to attach the file to.  Must be in all caps.
#' @param ... additional arguments passed to \code{apiPUT}
#' @export
#' @return List of length 2, containing \code{content} and \code{response} objects:
#' \itemize{
#'  \item{\code{content}} is the HTTP response content. If \code{targetAttributeName}
#'  is not empty, \code{content} will return as NULL.
#'  \item{\code{response}} is the entire HTTP response. If \code{fullReturn} is FALSE
#'  will return as NULL.
#' }
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' modifiedItem <- attachFile(login$coreApi,
#'   entitytype,
#'   barcode,
#'   filePath,
#'   targetAttributeName = "",
#'   useVerbose = FALSE
#' )
#' logOut(login$coreApi)
#' }
#' @author Craig Parman info@ngsanalytics.com
#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{attachFile} Attaches a file to entity identified by barcode.
#' Note: This function uses the JSON API to post a file to an entity and
#' Odata to post to an attribute.

attachFile <-
  function(coreApi, entityType,
             barcode,
             filePath,
             targetAttributeName = "",
             ...) {
    if (!file.exists(filePath)) {
      stop({
        print("Unable to find file on local OS")
        print(filePath)
      },
      call. = FALSE
      )
    }

    if (targetAttributeName != "") {
      # Check if the metadata reports this as a stream
      met <- getEntityMetadata(coreApi, entityType)
      valueFlag <- ifelse(match(
        "Edm.Stream",
        met$attributes$types[match(
          targetAttributeName,
          met$attributes$names
        )]
      ) == 1,
      TRUE,
      FALSE
      )

      body <- httr::upload_file(filePath)
      query <- paste0("('", barcode, "')/", targetAttributeName)
      header <- c("If-Match" = "*")

      response <-
        apiPUT(
          coreApi,
          resource = entityType,
          query = query,
          body = body,
          encode = "raw",
          headers = header,
          special = NULL,
          valueFlag = valueFlag,
          ...
        )
    } else { # Use the JSON SDK to post a file to an entity.
      # TODO replace this section of code to use the ODATA functionality when available.

      sdkCmd <- jsonlite::unbox("file-attach")
      fileName <- basename(filePath)
      data <- list(
        targetEntityBarcode = jsonlite::unbox(barcode),
        targetEntityId = jsonlite::unbox(""),
        name = jsonlite::unbox(fileName),
        fileContentTypeOverride = jsonlite::unbox("")
      )

      responseOptions <- c("CONTEXT_GET", "MESSAGE_LEVEL_WARN")
      logicOptions <- "EXECUTE_TRIGGERS"
      typeParam <- jsonlite::unbox("FILE")

      request <-
        list(
          request = list(
            sdkCmd = sdkCmd,
            data = data,
            typeParam = typeParam,
            responseOptions = responseOptions,
            logicOptions = logicOptions
          )
        )

      body <- list(
        json = jsonlite::toJSON(request),
        fileData = httr::upload_file(filePath)
      )

      cookie <-
        c(
          JSESSIONID = coreApi$jsessionId,
          AWSELB = coreApi$awselb
        )

      response <-
        httr::POST(
          url = buildUrl(coreApi, special = "json"),
          body = body,
          httr::verbose(data_out = FALSE),
          httr::add_headers("Content-Type" = "multipart/form-data"),
          httr::set_cookies(cookie)
        )

      # check for general HTTP error in response
      if (httr::http_error(response)) {
        stop({
          print("json API file-attach call failed")
          print(httr::http_status(response))
        },
        call. = FALSE
        )
      }
    }

    if (targetAttributeName != "") {
      list(
        entity = NULL,
        response = response$response
      )
    } else {
      list(
        entity = httr::content(response),
        response = response
      )
    }
  }
