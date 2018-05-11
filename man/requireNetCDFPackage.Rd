\name{requireNetCDFPackage}
\alias{requireNetCDFPackage}
\title{requireNetCDFPackage}
\description{require namespace of given NetCDF package}
\usage{requireNetCDFPackage(packageNames = c("RNetCDF", 
    "ncdf4"))}
\arguments{
  \item{packageNames}{string vector: Name of R NetCDF packages
to be tried to used in this order}
}
\details{currently 'RNetCDF' and 'ncdf4' are supported
Loading package namespace is tried in the order of occurrence in packageNames}
\value{The package name whose namespace has been loaded}

\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl]}





