% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAttributesColumnHeaders.R
\name{getAttributesColumnHeaders}
\alias{getAttributesColumnHeaders}
\title{getAttributesColumnHeaders - Gets the attribute column header for all or specified attributes in an entityType.
\code{getAttributesColumnHeaders} Gets the attribute column header for all or specified attributes in an entityType.}
\usage{
getAttributesColumnHeaders(coreApi, attributeList = NULL, entityType,
  ...)
}
\arguments{
\item{attributeList}{list of attribute names (usually obtained through getEntityMetadata) to limit the column header names. Default = NULL.}

\item{entityType}{entity type which has the desired attribute column headers assigned}

\item{...}{additional arguments passed to \code{apiGET}}

\item{coreAPI}{coreApi object with valid jsessionid}
}
\value{
List of length 2, containing \code{entity} and \code{response} objects:
\itemize{
 \item{\code{entity}} is a character element with associated column header names.
 \item{\code{response}} is the entire HTTP response.
}
}
\description{
\code{getAttributesColumnHeaders} - Gets the attribute column header for all or specified attributes in an entityType.
}
\examples{
\dontrun{
api <- coreAPI("PATH TO JSON FILE")
login <- authBasic(api)
experimentAssayType <- getAttributesColumnHeaders(login$coreApi, "CI_TEMPERATURE", "BEER")
logOut(login$coreApi)
}
}
\author{
Edgardo Gutierrez edgardo.gutierrez@thermofisher.com

Natasha Mora natasha.mora@thermofisher.com

Francisco Marin francisco.marin@thermofisher.com
}
