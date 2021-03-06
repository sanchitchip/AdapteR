% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FLStringFunctions.R
\name{FLParseXML}
\alias{FLParseXML}
\title{Parse XML files}
\usage{
FLParseXML(object)
}
\arguments{
\item{object}{FLVector of characters}
}
\value{
dataframe with parsed XML
}
\description{
Parse XML files stored in-database
as elements of FLVector.
}
\details{
The DB Lytix function called is FLParseXMLUdt.An XML parser is a utility that goes through 
text documents containing XML trees and allows the information in the 
hierarchy to be extracted to replicate the tree structure in a columnar layout.
}
\section{Constraints}{

row vectors are not supported currently.
}
\examples{
wtd <- FLTable("tblXMLTest","GroupID")
flv <- wtd[,"pXML"]
resultdataframe <- FLParseXML(flv)
}
\seealso{
\code{\link[XML]{xmlParse}}
}

