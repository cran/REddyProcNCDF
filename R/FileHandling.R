# copied from REddyProc because not exported from there

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ File handling
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fInitFilesDir <- function(
  ##description<<
  ## Get all available files with specific file extension in directory
  Dir.s           ##<< Directory (string) to be searched for files
  , lFileExt.s     ##<< File extension (string) specification
  , fixed = TRUE	  ##<< set to FALSE, if using regular expressions
)
  ##author<<
  ## AMM
{
  # List files in path and grep files with specified file extension as character string
  list.files(path = Dir.s)[grep(lFileExt.s, list.files(path = Dir.s), fixed = fixed)]
  ##value<<
  ## Character vector with names of all available site files.
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fStripFileExtension <- function(
  ##description<<
  ## Strip file extension
  lFiles.V.s     ##<< String vector with names of all available site files
)
  ##author<<
  ## AMM
{
  # RegExp: Search for first dot and replace rest of string with nothing
  sub('[.]. * ', '', lFiles.V.s)
  ##value<<
  ## Character vector containing the first part of file names (before first dot in file name).
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fSetFile <- function(
  ##description<<
  ## Set file name with path and check if directory and / or file exists
  fileName            ##<< File name as a string
  , isInput                 ##<< Input / output flag, TRUE for input, FALSE for output
  , callingFunction = ''    ##<< Name (string) of the caller function for warnings
)
  ##author<<
  ## AMM
  # TEST: Dir.s <- 'inst / examples'; FileName.s <- 'Example_DETha98.txt'; isInput <- T; callingFunction <- 'test'
{
  FileName.s <- basename(fileName)
  Dir.s <- dirname(fileName)
  # Check if string for directory provided
  Dir.b <- (Dir.s != ".")

  # Check if directory exists
  if (isInput && Dir.b && (file.access(Dir.s, mode = 4) != 0))
    stop(callingFunction, ':::fSetFile::: Directory does not exist: ', Dir.s)

  # Make directory if mode is output
  if ( !isInput && Dir.b && (file.access(Dir.s, mode = 0) != 0) ) {
    dir.create(Dir.s)
    message(callingFunction, ':::fSetFile::: Directory created: ', Dir.s)
    if (file.access(Dir.s, mode = 2) != 0)
      stop(callingFunction, ':::fSetFile::: Directory could not be created: ', Dir.s)
  }

  # Set file name accordingly
  File.s <- if (Dir.b) file.path( Dir.s, FileName.s ) else FileName.s

  # If input file, check if file exists
  if (isInput && (file.access(File.s, mode = 4) != 0) )
    stop(callingFunction, ':::fSetFile::: File does not exist or has no read permission: ', File.s)

  File.s
  ##value<<
  ## Returns name of file with complete path.
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Variable check functions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fCheckValString <- function(
  ##description<<
  ## Check if variable is a non-empty character string
  Value.s               ##<< Value to be checked if string
)
  ##author<<
  ## AMM
  ##details<<
  ## See test_CheckValue.R for more details.
{
  if ( length(Value.s) == 0) {
    FALSE
  } else if (!is.na(Value.s) && (!is.character(Value.s) || !nzchar(Value.s)) ) {
    FALSE
  } else {
    TRUE
  }
  ##value<<
  ## Boolean value if true of false.
}

