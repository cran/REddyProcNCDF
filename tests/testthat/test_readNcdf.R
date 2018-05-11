#require(REddyProcNCDF)
#require(testthat)
context('readNcdf')

# TRUE during testthat::check()
isNotOnCRAN <- identical(Sys.getenv("NOT_CRAN"), "true")
# isNotOnCRAN <- TRUE  # may set interactively to TRUE

#exPath <- REddyProc::getExamplePath(
#  'Example_DE-Tha.1996.1998.hourly_selVars.nc', isNotOnCRAN)
# if (!length(exPath)) warning(
#   "Example_DE-Tha.1996.1998.hourly_selVars.nc not available. "
#   , "Skipping corresponding tests.")
exPath <- system.file(
  file.path('examples','Example_DE-Tha.1996.1998.hourly_selVars.nc')
  , package = "REddyProcNCDF")
varNames <- c('NEE', 'Rg', 'rH', 'Tair', 'NEE_f')
nRec <- 1000L


expectDefaults <- function(ds){
	expect_that( nrow(ds), equals(nRec) )
	expect_true( all( varNames %in% colnames(ds)) )
	expect_true( all( c('DateTime','year','month','day','hour') %in% colnames(ds)) )
	expect_true( is.character(sapply(ds[,-1], function(var){attributes(var)$units})) )
	expect_that( as.vector(sapply(ds[,-1], function(var){attributes(var)$varnames}))
	             , equals(colnames(ds)[-1]) )
}

test_that('ncdf4',{
	if (!length(exPath)) skip("could not obtain example nc file")
	if (!requireNamespace('ncdf4') ) warning("ncdf4 package not installed, skipping test") else {
		ds <- fLoadFluxNCIntoDataframe(varNames, exPath, count = c(1L,1L,nRec)
			, ncPkg = 'ncdf4'
		)
		expectDefaults(ds)
	}
})

test_that('RNetCDF',{
	if (!length(exPath) ) skip("could not obtain example nc file")
	if (!requireNamespace('RNetCDF') ) warning("RNetCDF package not installed, skipping test") else {
			ds <- fLoadFluxNCIntoDataframe(varNames, exPath, count = c(1L,1L,nRec)
				, ncPkg = 'RNetCDF'
			)
			expectDefaults(ds)
		}
})

test_that('RNetCDF missing col',{
			if (!length(exPath) ) skip("could not obtain example nc file")
			if (!require('RNetCDF') ) warning("RNetCDF package not installed, skipping test") else {
				expect_warning(
						suppressMessages(
							ds <- fLoadFluxNCIntoDataframe(
							  c("bla",varNames), exPath, count = c(1L,1L,nRec)
									, ncPkg = 'RNetCDF'
							)
						)
				)
				expectDefaults(ds)
			}
		})

test_that('ncdf4 missing col',{
			if (!length(exPath) ) skip("could not obtain example nc file")
			if (!require('ncdf4') ) warning("ncdf4 package not installed, skipping test") else {
				expect_warning(
						suppressMessages(
							ds <- fLoadFluxNCIntoDataframe(
							  c("bla",varNames), exPath, count = c(1L,1L,nRec)
									, ncPkg = 'ncdf4'
							)
						)
				)
				expectDefaults(ds)
			}
		})

.tmp.f <- function(){
	# file still missing units and currently not in inst/examples
	test_that('RNetCDF Berkeley',{
				path <- "./tmp"
				fname <- "ES-Ln2.HH.2009.2009.nc"
				ds <-  fLoadFluxNCIntoDataframe(c('SW_IN_F', 'SW_IN_POT'), fname, path
				                                , 'RNetCDF', count = nRec)
				expect_true( all( c('SW_IN_F', 'SW_IN_POT') %in% colnames(ds)) )
				expect_true( all( c('DateTime','year','month','day','hour') %in% colnames(ds)) )
				#expect_true( is.character(sapply(ds[,-1], function(var){attributes(var)$units})) )
				expect_that( as.vector(sapply(ds[,-1], function(var){attributes(var)$varnames}))
				             , equals(colnames(ds)[-1]) )
			})
}


