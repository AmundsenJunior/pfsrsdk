#' getExperimentContainersAndExperimentSamples - Gets the experiment container, container type and experiment samples of an experiment.
#' \code{getExperimentContainersAndExperimentSamples}  Gets the experiment container, container type and experiment samples of an experiment.
#' @param coreAPI coreApi object with valid jsessionid
#' @param experimentType entity type which has the attribute metadata
#' @param experimentBarcode barcode of entity to get
#' @param includeExperimentSamples get experiment sample associated to containers of an experiment, default FALSE
#' @param fullMetadata get full metadata, default is FALSE
#' @param ... additional arguments passed to \code{apiGET}
#' @return List of length 2, containing \code{entity} and \code{response} objects:
#' \itemize{
#'  \item{\code{entity}} list with experiment container, container type and experiment samples of an experiment.
#'  \item{\code{response}} is the entire HTTP response.
#' }
#' @export
#' @examples
#' \dontrun{
#' api <- coreAPI("PATH TO JSON FILE")
#' login <- authBasic(api)
#' experimentSampleLotNames <- getExperimentContainersAndExperimentSamples(login$coreApi, "experimentType", "EXP123", FALSE)
#' logOut(login$coreApi)
#' }
#' Specific data can be extracted, one example is to extract the container barcodes:
#' \dontrun{
#' containerBarcode <- sapply(
#'     result$entity[[contExpansion]], 
#'     #contExpansion is "REV_CONTAINER_EXPERIMENT_EXPERIMENT_CONTAINER" or "EXPERIMENT_CONTAINERS" depending on the PFS version.
#'     FUN = function(x) {
#'         x$Barcode})
#' }
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code{getExperimentContainersAndExperimentSamples} - Gets the experiment container, container type and experiment samples of an experiment.

getExperimentContainersAndExperimentSamples <-
  function(coreApi,
           experimentType,
           experimentBarcode,
           includeExperimentSamples = TRUE,
           fullMetadata = FALSE,
           ...) {

    # clean the name for ODATA
    experimentType <- odataCleanName(experimentType)

    resource <- paste0(experimentType, "('", experimentBarcode, "')")

    case(
      grepl("[0-2]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        exptContainerExpansion <- "REV_CONTAINER_EXPERIMENT_EXPERIMENT_CONTAINER"
        exptSampleExpansion <- "REV_EXPERIMENT_CONTAINER_EXPERIMENT_SAMPLE"
      },
      grepl("[3-9]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        exptContainerExpansion <- "EXPERIMENT_CONTAINERS"
        exptSampleExpansion <- "EXPERIMENT_SAMPLES"
      }
    )

    query <- paste0("?$expand=",exptContainerExpansion,"($expand=CONTAINER",
                        ifelse(includeExperimentSamples,paste0(",",exptSampleExpansion),""),
                        ")")

    if (fullMetadata) {
      headers <- c("Accept" = "application/json;odata.metadata=full")
    } else {
      headers <- NULL
    }

    response <-
      apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = headers,
        ...
      )

    list(entity = response$content, response = response$response)
  }
