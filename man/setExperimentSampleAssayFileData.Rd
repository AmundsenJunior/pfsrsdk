% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setExperimentSampleAssayFileData.R
\name{setExperimentSampleAssayFileData}
\alias{setExperimentSampleAssayFileData}
\title{setExperimentSampleAssayFileData - Puts file attached as assay data into an experiment sample.}
\usage{
setExperimentSampleAssayFileData(coreApi, assayType,
  experimentSampleBarcode, attributeName, filePath, ...)
}
\arguments{
\item{coreApi}{coreApi object with valid jsessionid}

\item{assayType}{assay type that contains sample}

\item{experimentSampleBarcode}{experiment sample barcode of entity to get}

\item{attributeName}{Name of the attribute that containts the file data}

\item{filePath}{path to file to upload}

\item{...}{additional arguments passed to \code{apiPUT}}
}
\value{
List of length 2, containing \code{content} and \code{response} objects:
\itemize{
 \item{\code{content}} is the HTTP response content. If the HTTP response status code is 204, returns NULL.
 \item{\code{response}} is the entire HTTP response.
}
}
\description{
\code{setExperimentSampleAssayFileData} Puts file attached as assay data into an experiment sample.
}
\details{
\code{setExperimentSampleAssayFileData} Puts file attached as assay data into an experiment sample.
}
\examples{
\dontrun{
api <- coreAPI("PATH TO JSON FILE")
login <- authBasic(api)
response <- setExperimentSampleAssayFileData(
  login$coreApi,
  "TURBIDITY ASSAY",
  "TBES1",
  "CI_EXTRA_DATA",
  "/path/to/data.file"
)
logOut(login$coreApi)
}
}
\author{
Craig Parman info@ngsanalytics.com

Scott Russell scott.russell@thermofisher.com
}
