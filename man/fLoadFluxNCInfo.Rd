\name{fLoadFluxNCInfo}
\alias{fLoadFluxNCInfo}
\title{Get site information from BGI NetCDF files}
\description{Load site information attributes such as latitude, longitude and others
from BGI NetCDF files}
\usage{fLoadFluxNCInfo(fileName, ncPkg = requireNetCDFPackage(), 
    callingFunction = "")}
\arguments{
  \item{fileName}{NetCDF file name as a string}
  \item{ncPkg}{scalar string of package name to be used}
  \item{callingFunction}{Name (string) of function called from}
}
\details{Description of attribute list:\describe{
\item{ID}{SiteID}
\item{DIMS}{Number of data rows}
\item{LON}{Longitude}
\item{LAT}{Latitude}
\item{TZ}{Time zone}
\item{ELEV}{Elevation}
\item{IGBP}{IGBP class}
}}
\value{Attibute list}

\author{AMM, TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl]}



\seealso{\code{\link{fLoadFluxNCIntoDataframe}}}

