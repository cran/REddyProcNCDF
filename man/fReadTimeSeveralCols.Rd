\name{fReadTimeSeveralCols}
\alias{fReadTimeSeveralCols}
\title{fReadTimeSeveralCols}
\description{Constructing time from columns 'year',...,'hour'}
\usage{fReadTimeSeveralCols(data, fileName, ncPkg = requireNetCDFPackage(), 
    callingFunction = "", colYear = "year", 
    colMonth = "month", colDay = "day", colHour = "hour", 
    defaultHour = 0, ...)}
\arguments{
  \item{data}{Data frame}
  \item{fileName}{NetCDF file name as a string}
  \item{ncPkg}{scalar string of package name to be used}
  \item{callingFunction}{Name (string) of function called from}
  \item{colYear}{Name (string) of variable holding the year}
  \item{colMonth}{Name (string) of variable holding the month}
  \item{colDay}{Name (string) of variable holding the day}
  \item{colHour}{Name (string) of variable holding the hour}
  \item{defaultHour}{(numeric) default that is used when colHour = NA
, when only days are specified}
  \item{\dots}{further arguments to var.get.nc or ncvar_get
, such as start and count}
}
\details{Time may be stored in different formats, and \code{\link{fLoadFluxNCIntoDataframe}}
is parameterized by a argument \code{fReadTime}.
The following functions are provided to construct time from different formats:
These functions help with the preparation of your data for the analysis:
\itemize{
\item{ from columns 'year',...,'hour': fReadTimeSeveralCols (this function) }
\item{ from column in ISODate integer format: \code{\link{fReadTimeBerkeley}} }
}}


\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl]}



\seealso{\code{\link{fLoadFluxNCIntoDataframe}}}

