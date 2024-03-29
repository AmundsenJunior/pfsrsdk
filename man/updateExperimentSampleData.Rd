% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/updateExperimentSampleData.R
\name{updateExperimentSampleData}
\alias{updateExperimentSampleData}
\title{updateExperimentSampleData - Update experiment sample assay data.}
\usage{
updateExperimentSampleData(coreApi, experimentAssayType,
  experimentSampleBarcode, experimentAssayUpdateAttrList, ...)
}
\arguments{
\item{coreApi}{coreApi object with valid jsessionid}

\item{experimentAssayType}{entity type to get as character string}

\item{experimentSampleBarcode}{User provided barcode as a character string}

\item{experimentAssayUpdateAttrList}{assay attributes as a list of key-values pairs}

\item{...}{additional arguments passed to \code{apiPUT}}
}
\value{
List of length 2, containing \code{entity} and \code{response} objects:
\itemize{
 \item{\code{entity}} is the HTTP response content of updated sample data.
 \item{\code{response}} is the entire HTTP response.
}
}
\description{
\code{updateExperimentSampleData} Update experiment sample assay data.
}
\details{
\code{updateExperimentSampleData} Update experiment sample assay data.
}
\examples{
\dontrun{
api <- coreAPI("PATH TO JSON FILE")
login <- authBasic(api)
response <- updateExperimentSampleData(login$coreApi,
  experimentAssayType = "BITTERNESS_ASSAY",
  experimentSampleBarcode = "BTES3", experimentAssayUpdateAttrList = list(CI_BITTERNESS_IBU = 9.7, CI_ACCEPT = FALSE)
)
updatedEntity <- response$entity
logOut(login$coreApi)
response <- authBasic(coreApi)
}
}
\author{
Craig Parman info@ngsanalytics.com

Natasha Mora natasha.mora@thermofisher.com

Scott Russell scott.russell@thermofisher.com
}
