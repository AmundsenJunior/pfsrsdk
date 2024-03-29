% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createExperimentContainer.R
\name{createExperimentContainer}
\alias{createExperimentContainer}
\title{createExperimentContainer- Creates a new experiment container by adding an exiting container to an experiment.}
\usage{
createExperimentContainer(coreApi, experimentType, experimentBarcode,
  containerBarcode, body = NULL, fullMetadata = FALSE, ...)
}
\arguments{
\item{coreApi}{coreApi object with valid jsessionid}

\item{experimentType}{experiment type to get as character string}

\item{experimentBarcode}{experiment barcode of an unpublished experiment}

\item{containerBarcode}{barcode of container to add to experiment}

\item{body}{values for sample attributes as a  list of key-values pairs (not user in this json version)}

\item{fullMetadata}{get full metadata, default is FALSE}

\item{...}{additional arguments passed to \code{apiPOST}}
}
\value{
List of length 2, containing \code{entity} and \code{response} objects:
\itemize{
 \item{\code{entity}} is the HTTP response content.
 \item{\code{response}} is the entire HTTP response.
}
}
\description{
\code{createExperimentContainer}Creates a new experiment container by adding an existing container
to an experiment.
}
\details{
\code{createExperimentContainer} Creates a new experiment container by adding an existing container to an experiment.
}
\examples{
\dontrun{
api <- coreAPI("PATH TO JSON FILE")
login <- authBasic(api)
item <- createExperimentContainer(
  login$coreApi, "Experiment_Type",
  "ExperimentBarCode", "Containerbarcode"
)
logOut(login$coreApi)
}
}
\author{
Craig Parman info@ngsanalytics.com

Natasha Mora natasha.mora@thermofisher.com

Scott Russell scott.russell@thermofisher.com
}
