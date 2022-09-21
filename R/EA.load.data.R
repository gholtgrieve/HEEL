#' @title Loads raw EA data for further analysis.
#'
#' @description This internal function loads raw EA data for analysis and makes it available for further analysis. If more than one
#' data file name (and path) is supplied, those files will e combined into a single dataframe and returned. The function also asks
#' the user to identify nteractively the directory to place the processed data file and report.
#'
#' @usage EA.load.data(results)
#'
#' @param results List containing XXX
#'
#' @return List with X objects...
#'
#' @author Gordon W. Holtgrieve
#'
#' @importFrom easycsv choose_dir
#' @importFrom dplyr bind_rows
#'
#' @keywords internal
#'
#' @export

EA.load.data <- function(results){

  #Ask user for the data directory
  print("Select a folder to place the finalized data file and report.")
  processed.data.dir <- easycsv::choose_dir()

  #List all .csv files that exist in the data directory.
  data.files <- results$data.files

  #Load, combine, and rename raw data
  n <- length(data.files)
  dataframe.list <- vector(mode="list", length = n)
  for (i in 1:n) dataframe.list[[i]] <- read.csv(data.files[i])
  raw.sequence.data <- bind_rows(dataframe.list)

  return(list(processed.data.dir = processed.data.dir,
              raw.sequence.data = raw.sequence.data))

  }
