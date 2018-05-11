\name{fReadTimeBerkeley}
\alias{fReadTimeBerkeley}
\title{fReadTimeBerkeley}
\description{Reads time columns (year, month, day, hour) from column in ISODate integer format}
\usage{fReadTimeBerkeley(data, fileName, ncPkg = requireNetCDFPackage(), 
    callingFunction = "", colTime = "TIMESTAMP_END", 
    ...)}
\arguments{
  \item{data}{Data frame}
  \item{fileName}{NetCDF file name as a string}
  \item{ncPkg}{scalar string of package name to be used}
  \item{callingFunction}{Name (string) of function called from}
  \item{colTime}{the column name (string) holding time with
format described in details}
  \item{\dots}{further arguments to var.get.nc or ncvar_get, such as
start and count}
}
\details{In the Berkeley-Release of the fluxnet data, the time is stored as an integer
with base10-digits representing YYYYMMddhhmm}


\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl]}



\seealso{\code{\link{fReadTimeSeveralCols}}

\code{\link{fLoadFluxNCIntoDataframe}}}

