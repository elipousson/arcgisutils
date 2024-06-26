% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-geometry.R
\name{determine_esri_geo_type}
\alias{determine_esri_geo_type}
\title{Determine Esri Geometry type}
\usage{
determine_esri_geo_type(x, call = rlang::caller_env())
}
\arguments{
\item{x}{an object of class \code{data.frame}, \code{sf}, \code{sfc}, or \code{sfg}.}

\item{call}{The execution environment of a currently running
function, e.g. \code{call = caller_env()}. The corresponding function
call is retrieved and mentioned in error messages as the source
of the error.

You only need to supply \code{call} when throwing a condition from a
helper function which wouldn't be relevant to mention in the
message.

Can also be \code{NULL} or a \link[rlang:topic-defuse]{defused function call} to
respectively not display any call or hard-code a code to display.

For more information about error calls, see \ifelse{html}{\link[rlang:topic-error-call]{Including function calls in error messages}}{\link[rlang:topic-error-call]{Including function calls in error messages}}.}
}
\value{
returns a character scalar of the corresponding Esri geometry type
}
\description{
Takes an \code{sf} or \code{sfc} object and returns the appropriate Esri geometry type.
}
\details{
\subsection{Geometry type mapping}{
\itemize{
\item \code{POINT}:  \code{esriGeometryPoint}
\item \code{MULTIPOINT}:  \code{esriGeometryMultipoint}
\item \code{LINESTRING}:  \code{esriGeometryPolyline}
\item \code{MULTILINESTRING}:  \code{esriGeometryPolyline}
\item \code{POLYGON}:  \code{esriGeometryPolygon}
\item \code{MULTIPOLYGON}:  \code{esriGeometryPolygon}
}
}
}
\examples{

determine_esri_geo_type(sf::st_point(c(0, 0)))
}
