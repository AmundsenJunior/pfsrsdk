#' createExperimentContainer- Creates a new experiment container by adding an exiting container to an experiment.
#'
#' \code{createExperimentContainer} Creates a new experiment container by adding an exiting container to an experiment.
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentType experiment type to get as character string
#' @param experimentBarcode experiment barcode of an unpublished experiment
#' @param containerBarcode barcode of container to add to experiment
#' @param body values for sample attributes as a  list of key-values pairs (not user in this json version)
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains entity information, $response contains the entire http response
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' item <- createExperimentContainer(
#'   login$coreApi, "Experiment_Type",
#'   "ExperimentBarCode", "Containerbarcode"
#' )
#' logOut(login$coreApi)
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{createExperimentContainer}Creates a new experiment container by adding an exiting container
#' to an experiment.

createExperimentContainer <-
  function(coreApi,
             experimentType,
             experimentBarcode,
             containerBarcode,
             body = NULL,
             useVerbose = FALSE) {
    experimentType <- odataCleanName(experimentType)
    headers <-
      c("Content-Type" = "application/json;odata.metadata=full")
    # no lint start
    case(
      grepl("[0-2]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        body <- list(
          "CONTAINER_EXPERIMENT@odata.bind" = paste0("/", experimentType, "('", experimentBarcode, "')"),
          "CONTAINER@odata.bind" = paste0("/CONTAINER('", containerBarcode, "')")
        )
      },
      grepl("[3-9]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        body <- list(
          "EXPERIMENT@odata.bind" = paste0("/", experimentType, "('", experimentBarcode, "')"),
          "CONTAINER@odata.bind" = paste0("/CONTAINER('", containerBarcode, "')")
        )
      }
    )
    # no lint end
    resource <- paste0(experimentType, "_CONTAINER")

    response <-
      apiPOST(
        coreApi,
        resource = resource,
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        encode = "raw",
        headers = headers,
        special = NULL,
        useVerbose = useVerbose
      )



    list(entity = httr::content(response), response = response)
  }
