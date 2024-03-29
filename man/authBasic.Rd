% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/authBasic.R
\name{authBasic}
\alias{authBasic}
\title{authBasic - Authenticates against the ODATA REST API using basic authentication.}
\usage{
authBasic(coreApi, ...)
}
\arguments{
\item{coreApi}{object of class coreApi that contains username, password,  baseURL and
tenant. tenant is required if user has access to multiple tenants.}

\item{...}{additional arguments passed to \code{apiPOST}}
}
\value{
List of length 2, containing \code{coreApi} and \code{response} objects:
\itemize{
 \item{\code{content}} is the passed \code{coreApi} object with \code{jsessionid}, \code{awselb} and \code{employeeid} populated.
 \item{\code{response}} is the entire HTTP response.
}
}
\description{
\code{authBasic} Logs in and returns a fully populated coreApi object in $coreAPI.
}
\details{
\code{authBasic} Authenticates against the ODATA REST API using basic authentication.
}
\examples{
\dontrun{
api <- coreAPI("PATH TO JSON FILE")
response <- authBasic(api)
login <- response$core$Api
error <- httr::http_error(response$response)
logOut(response$coreApi, useVerbose = TRUE)
}
}
\author{
Craig Parman info@ngsanalytics.com

Scott Russell scott.russell@thermofisher.com

Adam Wheeler adam.wheeler@thermofisher.com

Natasha Mora natasha.mora@thermofisher.com

Francisco Marin francisco.marin@thermofisher.com
}
