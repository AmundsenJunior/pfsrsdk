% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getUserProjects.R
\name{getUserProjects}
\alias{getUserProjects}
\title{getUserProjects - Gets metadata of projects accessible to the current user.}
\usage{
getUserProjects(coreApi, ...)
}
\arguments{
\item{coreApi}{coreApi object with valid jsessionid}

\item{...}{additional arguments passed to \code{apiGET}}
}
\value{
List of length 2, containing \code{entity} and \code{response} objects:
\itemize{
 \item{\code{entity}} list with project's metadata.
 \item{\code{response}} is the entire HTTP response.
}
}
\description{
\code{ getUserProjects}  Gets metadata of projects accessible to the current user.
}
\details{
\code{getUserProjects}  Gets metadata of projects accessible to the current user.
}
\examples{
\dontrun{
api <- coreAPI("PATH TO JSON FILE")
login <- authBasic(api)
response <- getUserProjects(login$coreApi)
logOut(login$coreApi)
}
Specific data can be extracted, one example is to extract the project barcodes:
\dontrun{
projectBarcodes <- unlist(lapply(
    response$content, 
    FUN = function(x)
        x$Barcode))
}
}
\author{
Natasha Mora natasha.mora@thermofisher.com
}
