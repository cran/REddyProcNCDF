\name{fAddNCFVar}
\alias{fAddNCFVar}
\title{fAddNCFVar}
\description{Add variable from NetCDF file to data frame}
\usage{fAddNCFVar(data, varNames, fileName, ncPkg = requireNetCDFPackage(), 
    callingFunction = "", varRenames = varNames, 
    ...)}
\arguments{
  \item{data}{Data frame}
  \item{varNames}{Variable name or names (vector of strings)}
  \item{fileName}{NetCDF file name as a string}
  \item{ncPkg}{scalar string of package name to be used
to be tried to used in this order}
  \item{callingFunction}{Name (string) of function called from}
  \item{varRenames}{Name (string) of the variable in data, offer renaming}
  \item{\dots}{further arguments to var.get.nc or ncvar_get
, such as start and count}
}

\value{Data frame with new nc variable added.}

\author{AMM, KS, TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl]}



\seealso{\code{\link{fLoadFluxNCIntoDataframe}}}

