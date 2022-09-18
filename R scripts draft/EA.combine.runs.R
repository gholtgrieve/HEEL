#' @title Combines the data (samples, standards, blanks) from from multiple EA runs into a single data set.
#'
#' @description  (Currently Not Implemented) Analyzes blanks for the run and reports result to the user, then asking whether or not to
#'               blank correct standard and sample data. The correction is based on empty tins and a simple mixing model.
#'
#' @usage EA.combine.runs(data.files = data.files)
#'
#' @param data.files    Character vector greater than length 1 containing full file names for the raw data files that are to be combined.
#'                      Must be a .csv files!
#'
#' @author Gordon W. Holtgrieve
#'
#' @keywords internal
#' @export

EA.combine.runs <- function(data.files){

  ##Organize and save files for future reference.##
  #Create a directory for the combined files. I store my raw data in data/base, but put my processed data in data/processed.
  workingDir <- getwd()
  processedFilesPath <- paste0(workingDir,"/", "processed")
  combinedFilesPath <- paste0(workingDir,"/", "combined")
  dir.create(combinedFilesPath)

  sequenceIDs <- tools::file_path_sans_ext(data.files)

  combinedSamples <- vector("list", length(data.files)) #make empty list to store combined data
  combinedStandards <- vector("list", length(data.files))
  #combinedBlanks <- vector("list", length(data.files))

  #name the elements of the lists based on the sequenceIDs
  names(combinedSamples) <- sequenceIDs
  names(combinedStandards) <- sequenceIDs
  #names(combinedBlanks) <- sequenceIDs

  for(i in 1:length(sequence.IDs)) {
    setwd(paste0(processedFilesPath, "/", sequenceIDs[i])) #Open directory
    #Need to figure out how to make a list of multiple data frames
    combinedStandards[[i]] <- read.csv(file = paste0("standardCN_",data.files[i]), header=T)
    combinedSamples[[i]] <- read.csv(file = paste0("sampleCN_",data.files[i]), header=T)
    #blankCN <- read.csv(file = paste0("blankCN_",data.files[i]), header=T)  #Not implemented
  }

  #Make single combined dataframes
  combinedStandardsDF <- dplyr::bind_rows(combinedStandards, .id = "SequenceID")
  combinedSamplesDF <- dplyr::bind_rows(combinedSamples, .id = "SequenceID")
  #combinedBlanksDF <- dplyr::bind_rows(combinedStandards[i], .id = "SequenceID")

  #Save the combined dataframes as .csv files in the /combined folder
  setwd(combinedFilesPath) #Open directory
  write_csv(combinedStandardsDF, file = "combinedStandards.csv")
  write_csv(combinedSamplesDF, file = "combinedSamples.csv")
  write_csv(combinedBlanksDF, file = "combinedBlanks.csv")

  return(flag=T)

}
