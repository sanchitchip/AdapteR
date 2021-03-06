% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FLCastFunctions.R
\name{as.FLVector}
\alias{as.FLVector}
\title{casting to FLVector}
\usage{
as.FLVector(object, ...)
}
\arguments{
\item{object}{matrix, vector, data frame, sparseMatrix, FLMatrix which
needs to be casted to FLVector}

\item{...}{additional arguments like size}

\item{connection}{ODBC/JDBC connection object}

\item{size}{number of elements in resulting FLVector.
size input is not applicable only in case of FLMatrix}
}
\value{
FLVector object after casting.
}
\description{
Converts input \code{obj} to FLVector object
}

