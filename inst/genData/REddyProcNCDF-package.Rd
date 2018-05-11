\name{REddyProcNCDF-package}
\alias{REddyProcNCDF-package}
\alias{REddyProcNCDF}
\title{Reading data from NetCDF files for REddyProc}
\description{
This package enhances REddyProc a package for
standard and extensible Eddy-Covariance data post-processing includes
  uStar-filtering, gap-filling, and flux-partitioning.
A general description and an online tool based on this REddyProc can be found
here: \url{https://www.bgc-jena.mpg.de/bgi/index.php/Services/REddyProcWeb}.

This package adds functionality to read data from netCDF files.
}

\details{
Reading data from NetCDF files was formerly part of REddyProc, but has
been factored to this own package to decrease the number of dependencies
in REddyProc.

The main functions
\itemize{
\item{ Reading standard data from a NetCDF file: \code{\link{fLoadFluxNCIntoDataframe}} }
\item{ Reading a specific variable from a NetCDF file: \code{\link{fAddNCFVar}} }
}%itemize

The package works with alternative backend-packages that are installed.
For the default is the first entry of argument \code{packageNames}
in \code{\link{requireNetCDFPackage}}.
If the preference order for a specific package is changed, provide argument
use: \code{prefNcPkg = requireNetCDFPackage(myPreferenceNameVector)} and
subsequently specify argument \code{ncPkg = prefNcPkg} to other functions.

Time may be stored in different formats, and \code{\link{fLoadFluxNCIntoDataframe}}
is parameterized by a argument \code{fReadTime}.
The following functions are provided to construct time from different formats:
These functions help with the preparation of your data for the analysis:
\itemize{
\item{ from columnns 'year',...,'hour': \code{\link{fReadTimeSeveralCols}} }
\item{ from column in ISODate integer format: \code{\link{fReadTimeBerkeley}} }
}%itemize

Further functionality.
\itemize{
\item{ Get site information from BGI NetCDF files: \code{\link{fLoadFluxNCInfo}} }
}%itemize

}%details


\author{ Department for Biogeochemical Integration at MPI-BGC, Jena, Germany }

\keyword{package}
