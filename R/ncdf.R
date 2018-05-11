#' @export
fLoadFluxNCIntoDataframe <- function(
  ##title<<
  ## Load data from a NetCDF file
  ##description<<
  ## Load specified variables and time stamp information from NetCDF file in
  ## Fluxnet BGI format.
  ## The time stamp information needs to be provided as variables
  ## 'year', 'month', 'day', 'hour'.
  varNames         ##<< string vector of variables to be read in
  , fileName        ##<< File name as a string
  , ncPkg = requireNetCDFPackage()    ##<< scalar string of package name to be used
  , fReadTime = fReadTimeSeveralCols	##<< function that reads time columns
    ## It must append columns year (from 0AD), month, day, and hour (fractional)
    ## See \code{\link{fReadTimeSeveralCols}}
  , ...		##<< further arguments to var.get.nc or ncvar_get, such as start and count
) {
  ##author<<AMM, KS, TW
  # Check for R NetCDF packages
  ncPkg <- requireNetCDFPackage(ncPkg)
  # Read in time variables
  data <- fReadTime(
    NULL, fileName, ncPkg = ncPkg, 'fLoadFluxNCIntoDataframe', ...)
  # Convert time format to POSIX
  # !Attention: Use YMDH time format because julday and hour time stamps
  # inconsistent at end of year
  data <- fConvertTimeToPosix(
    data, 'YMDH', Year.s = 'year', Month.s = 'month'
    , Day.s = 'day', Hour.s = 'hour')
  # Read in variables from a given list of needed variables
  # capture both stdErr and stdOut from fAddNCVar, so that messages can be suppressed
  msgFromfAddNCFVar <- capture.output(capture.output(
		  data <- fAddNCFVar(
		    data, setdiff(varNames, colnames(data)), fileName, ncPkg = ncPkg
		    , 'fLoadFluxNCIntoDataframe', ...)
		  , type = c("message")))
  message(msgFromfAddNCFVar)
  message('Loaded BGI Fluxnet NC file: ', fileName, ' with the following headers:')
  message(' *** ', paste(colnames(data), '(', as.character(lapply(
    data, attr, which = 'units')), ')', collapse = ' ', sep = ''))
  data
  ##value<<
  ## Data frame with data from nc file.
  ##examples<<
  ## \donttest{
  ## examplePath <- system.file(
  ##  file.path('examples','Example_DE-Tha.1996.1998.hourly_selVars.nc')
  ##  , package = "REddyProcNCDF")
  ## EddyNCData <- fLoadFluxNCIntoDataframe(c('NEE', 'Rg', 'NEE_f'), examplePath)
  ## }
}

.tmp.f <- function(){
  fileName <- 'Example_DE-Tha.1996.1998.hourly_selVars.nc'
  Dir.s <- '../REddyProc/examples';
  varNames <- c('NEE', 'Rg', 'rH', 'Tair', 'NEE_f')
  ncPkg <- 'ncdf4'
  str(tmp <- fLoadFluxNCIntoDataframe(
    c('NEE', 'Rg'), 'Example_DE-Tha.1996.1998.hourly_selVars.nc'
    ,  count = c(1L, 1L, 4000L) ))
}

#' @export
requireNetCDFPackage <- function(
  ### require namespace of given NetCDF package
  packageNames = c('RNetCDF','ncdf4')  ##<< string vector: Name of R NetCDF packages
  ## to be tried to used in this order
) {
  ##details<< currently 'RNetCDF' and 'ncdf4' are supported
  ## Loading package namespace is tried in the order of occurrence in packageNames
  choices <- c('RNetCDF','ncdf4')
  for (pkg in packageNames) {
    if ((pkg %in% choices) && suppressWarnings(requireNamespace(pkg)) )
      ##value<< The package name whose namespace has been loaded
      return(pkg)
  }
  stop('None of the Ncdf-packages (', paste(packageNames, collapse = ",")
       , ') could not be loaded')
}

.tmp.f <- function(){
  requireNetCDFPackage("bla")
  requireNetCDFPackage("ncdf4")
  requireNetCDFPackage(c("ncdf4","RNetCDF"))
  requireNetCDFPackage(c("RNetCDF","ncdf4"))
}


#' @export
fReadTimeSeveralCols <- function(
		### Constructing time from columns 'year',...,'hour'
		data                ##<< Data frame
		, fileName          ##<< NetCDF file name as a string
		, ncPkg = requireNetCDFPackage()    ##<< scalar string of package name to be used
		, callingFunction = '' ##<< Name (string) of function called from
		, colYear = 'year'	  ##<< Name (string) of variable holding the year
		, colMonth = 'month'	##<< Name (string) of variable holding the month
		, colDay = 'day'		  ##<< Name (string) of variable holding the day
		, colHour = 'hour'	  ##<< Name (string) of variable holding the hour
		, defaultHour = 0	  	##<< (numeric) default that is used when colHour = NA
		  ## , when only days are specified
		, ...				          ##<< further arguments to var.get.nc or ncvar_get
		  ## , such as start and count
) {
  ##details<<
  ## Time may be stored in different formats, and \code{\link{fLoadFluxNCIntoDataframe}}
  ## is parameterized by a argument \code{fReadTime}.
  ## The following functions are provided to construct time from different formats:
  ##   These functions help with the preparation of your data for the analysis:
  ##   \itemize{
  ##     \item{ from columns 'year',...,'hour': fReadTimeSeveralCols (this function) }
  ##     \item{ from column in ISODate integer format: \code{\link{fReadTimeBerkeley}} }
  ##   }
  ##seealso<< \code{\link{fLoadFluxNCIntoDataframe}}
  if (!length(colHour) ) {
		data <- fAddNCFVar(
		  data, c(colYear, colMonth, colDay), fileName,  ncPkg = ncPkg
		  , callingFunction, varRenames = c("year", "month", "day"), ...)
		data$hour <- defaultHour
	} else {
		data <- fAddNCFVar(
		  data, c(colYear, colMonth, colDay, colHour), fileName,  ncPkg = ncPkg
		  , callingFunction, varRenames = c("year", "month", "day", "hour"), ...)
	}
	data
}

#' @export
fReadTimeBerkeley <- function(
		### Reads time columns (year, month, day, hour) from column in ISODate integer format
		data                ##<< Data frame
		, fileName           ##<< NetCDF file name as a string
		, ncPkg = requireNetCDFPackage()    ##<< scalar string of package name to be used
		, callingFunction = ''    ##<< Name (string) of function called from
		, colTime = 'TIMESTAMP_END'	##<< the column name (string) holding time with
		  ## format described in details
		, ...					##<< further arguments to var.get.nc or ncvar_get, such as
		  ## start and count
) {
	##details<<
	## In the Berkeley-Release of the fluxnet data, the time is stored as an integer
	## with base10-digits representing YYYYMMddhhmm
	#
	##seealso<< \code{\link{fReadTimeSeveralCols}}
  ##seealso<< \code{\link{fLoadFluxNCIntoDataframe}}
	data <- fAddNCFVar(data, colTime, fileName,  ncPkg = ncPkg
	                     , callingFunction, ...)
	timeStampChar <- as.character(data[[colTime]])
	data <- cbind(data, data.frame(
					year = as.integer(substr(timeStampChar, 1, 4))
					, month = as.integer(substr(timeStampChar, 5, 6))
					, day = as.integer(substr(timeStampChar, 7, 8))
					, hour = as.integer(substr(timeStampChar, 9, 10)) +
						as.integer(substr(timeStampChar, 11, 12)) / 60
	))
	#str(data)
	data
}

.tmp.f <- function() {
	# testing if baseDate is stored in nc-file
	# http://stackoverflow.com/questions/18819112/use-r-to-extract-time-series-from-netcdf-data
	InputNCF.s <- REddyProc:::fSetFile(fileName,  T, 'fAddNCFVar')
	NCFile.C <- open.nc(InputNCF.s)
	baseDate <- att.get.nc(NCFile.C, "NC_GLOBAL", "base_date")
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fAddNCFVar <- function(
  ##description<<
  ## Add variable from NetCDF file to data frame
  data                ##<< Data frame
  , varNames                ##<< Variable name or names (vector of strings)
  , fileName           ##<< NetCDF file name as a string
  , ncPkg = requireNetCDFPackage()    ##<< scalar string of package name to be used
  ## to be tried to used in this order
  , callingFunction = ''    ##<< Name (string) of function called from
  , varRenames = varNames		 ##<< Name (string) of the variable in data, offer renaming
  , ...					##<< further arguments to var.get.nc or ncvar_get
    ## , such as start and count
) {
  ##author<< AMM, KS, TW
  ##seealso<< \code{\link{fLoadFluxNCIntoDataframe}}
  if (length(varRenames) != length(varNames) ) stop(
    "varRenames must have the same length as varNames")
  InputNCF.s <- fSetFile(fileName, T, 'fAddNCFVar')
  if (ncPkg == 'RNetCDF') {
	  fOpen <- RNetCDF::open.nc
	  fReadVar <- RNetCDF::var.get.nc
	  fClose <- RNetCDF::close.nc
	  #fInqVar <- var.inq.nc
	  fGetAtt <- RNetCDF::att.get.nc
  } else if (ncPkg == 'ncdf4') {
	  fOpen <- ncdf4::nc_open
	  fReadVar <- ncdf4::ncvar_get
	  fClose <- ncdf4::nc_close
	  fGetAtt <- function(...) { ncdf4::ncatt_get(...)$value }
  } else {
    stop('specified netCDF package is not supported: ', ncPkg)
  }
  #
	NCFile.C <- fOpen(InputNCF.s)	# NCFile.C <- nc_open(InputNCF.s)
	tmpFilename <- tempfile(); tmpFile <- file(tmpFilename, open = "wt")
	tryCatch({
				newCols <- lapply(seq_along(varNames), function(i) {
							newCol <- try(as.vector(fReadVar(NCFile.C, varNames[i], ...)) )
							if (length(newCol) && !inherits(newCol, "try-error")) {
								attr(newCol, 'varnames') <- varRenames[i]
								# to prevent error message, that appears even with
								# try(, silent = TRUE) on non-existing attribute
								sink(tmpFile, type = "message")
								try(attr(newCol, 'units') <- fGetAtt(
								  NCFile.C, varNames[i], 'units'), silent = TRUE)
								sink(NULL, type = "message")
								newCol
							} else {
								warning("could not read variable ", varNames[i]
								        , " from netCdf-File ", fileName)
								return(NULL)
							}
							#attr(data[[1]], 'units')
						})
				names(newCols) <- varRenames
				newCols.F <- as.data.frame(newCols[sapply(newCols, length) != 0L])
				# Use c() instead of cbind() to be able to bind dataframe data
				# even if empty
				# twutz160121: c gives a list instead of error when nRows differ
				# between data and newCol.F (e.g. with different count argument to
				# fReadVar), therefore better use cbind
				if (nrow(newCols.F) )
					data <- if (length(data)) cbind(data, newCols.F) else newCols.F
			},
			finally = {
				fClose(NCFile.C)
				close(tmpFile)
				unlink(tmpFilename)
			}
	)
  data
  ##value<<
  ## Data frame with new nc variable added.
}

.tmp.f <- function(){
  data <- NULL; varNames <- 'NEE'
  fileName <- 'Example_DE-Tha.1996.1998.hourly_selVars.nc'
  Dir.s <- 'inst / examples'
  ncPkg <- 'ncdf4'
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fLoadFluxNCInfo <- function(
  ##title<<
  ## Get site information from BGI NetCDF files
  ##description<<
  ## Load site information attributes such as latitude, longitude and others
  ## from BGI NetCDF files
  fileName               ##<< NetCDF file name as a string
  , ncPkg = requireNetCDFPackage()    ##<< scalar string of package name to be used
  , callingFunction = ''    ##<< Name (string) of function called from
) {
  ##author<< AMM, TW
  ##seealso<< \code{\link{fLoadFluxNCIntoDataframe}}
  InputNCF.s <- fSetFile(fileName,  T, 'fAddNCFVar')
  if (ncPkg == 'RNetCDF') {
    NCFile.C <- RNetCDF::open.nc(InputNCF.s)
    tryCatch({
      ##details<<
      ## Description of attribute list:
      ##describe<<
      SiteInfo.L <- list(
        ID    = RNetCDF::att.get.nc(                  ##<< SiteID
          NCFile.C, 'NC_GLOBAL', 'Site_ID')
        , DIMS = RNetCDF::dim.inq.nc(                 ##<< Number of data rows
          NCFile.C, 'time')$length
        , LON  = as.numeric(RNetCDF::att.get.nc(      ##<< Longitude
          NCFile.C, 'NC_GLOBAL', 'Longitude'))
        , LAT  = as.numeric(RNetCDF::att.get.nc(      ##<< Latitude
          NCFile.C, 'NC_GLOBAL', 'Latitude'))
        , TZ   = as.numeric(RNetCDF::att.get.nc(      ##<< Time zone
          NCFile.C, 'NC_GLOBAL', 'TimeZone'))
        , ELEV = as.numeric(RNetCDF::att.get.nc(      ##<< Elevation
          NCFile.C, 'NC_GLOBAL', 'Elevation'))
        , IGBP = RNetCDF::att.get.nc(                 ##<< IGBP class
          NCFile.C, 'NC_GLOBAL', 'IGBP_class')
      )
    }, finally = RNetCDF::close.nc(NCFile.C))
  } else if (ncPkg == 'ncdf4') {
    NCFile.C <- ncdf4::nc_open(
      InputNCF.s, write = FALSE, readunlim = TRUE, verbose = FALSE)
    tryCatch({
      SiteInfo.L <- list(
        ID    = ncdf4::ncatt_get(NCFile.C, 0, 'Site_ID')$value
        , DIMS = NCFile.C$dim$time$len
        , LON  = as.numeric(ncdf4::ncatt_get(NCFile.C, 0, 'Longitude')$value)
        , LAT  = as.numeric(ncdf4::ncatt_get(NCFile.C, 0, 'Latitude')$value)
        , TZ   = as.numeric(ncdf4::ncatt_get(NCFile.C, 0, 'TimeZone')$value)
        , ELEV = as.numeric(ncdf4::ncatt_get(NCFile.C, 0, 'Elevation')$value)
        , IGBP = ncdf4::ncatt_get(NCFile.C, 0, 'IGBP_class')$value
      )
    }, finally = ncdf4::nc_close(NCFile.C))
  } else {
    stop('specified netCDF package is not supported: ', ncPkg)
  }
  ##value<< Attibute list
  SiteInfo.L
}

.tmp.f <- function(){
  fileName <- 'Example_DE-Tha.1996.1998.hourly_selVars.nc'
  Dir.s <- '../REddyProc/examples'
  ncPkg <- 'ncdf4'
  fLoadFluxNCInfo('Example_DE-Tha.1996.1998.hourly_selVars.nc',  'ncdf4')
}
