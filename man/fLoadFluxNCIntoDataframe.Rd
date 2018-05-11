\name{fLoadFluxNCIntoDataframe}
\alias{fLoadFluxNCIntoDataframe}
\title{Load data from a NetCDF file}
\description{Load specified variables and time stamp information from NetCDF file in
Fluxnet BGI format.
The time stamp information needs to be provided as variables
'year', 'month', 'day', 'hour'.}
\usage{fLoadFluxNCIntoDataframe(varNames, fileName, 
    ncPkg = requireNetCDFPackage(), fReadTime = fReadTimeSeveralCols, 
    ...)}
\arguments{
  \item{varNames}{string vector of variables to be read in}
  \item{fileName}{File name as a string}
  \item{ncPkg}{scalar string of package name to be used}
  \item{fReadTime}{function that reads time columns
It must append columns year (from 0AD), month, day, and hour (fractional)
See \code{\link{fReadTimeSeveralCols}}}
  \item{\dots}{further arguments to var.get.nc or ncvar_get, such as start and count}
}

\value{Data frame with data from nc file.}

\author{AMM, KS, TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl]}




\examples{\donttest{
examplePath <- system.file(
file.path('examples','Example_DE-Tha.1996.1998.hourly_selVars.nc')
, package = "REddyProcNCDF")
EddyNCData <- fLoadFluxNCIntoDataframe(c('NEE', 'Rg', 'NEE_f'), examplePath)
}}
