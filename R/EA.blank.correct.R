#' @title Blank corrects EA sample data. (Not Implemented)
#'
#' @description  Analyzes blanks for the run and reports result to the user, then asking whether or not to blank correct
#' standard and sample data. The correction is based on peak area of mass 28 (mass N) and d15N of empty tins and a simple mixing model.
#'
#' @usage EA.blank.correct(results)
#'
#' @return List with two tables and a Boolean flag:
#'   \describe{
#'     \item{standard.CN}{Dataframe of raw data IsoDat data for the standards and QTY standards; updated to with new column for blank corrected data, if required.}
#'     \item{sample.CN}{Dataframe of raw data IsoDat data for the unknown samples form the sequence; updated to with new column for blank corrected data, if required.}
#'     \item{blank.correct.flag}{Boolean flag to indicate if a blank correction was performed.}
#'     \item{current.data.columns}{Character: Names of the columns in sample.CN and standard.CN tables that should be used in future calculations.}
#'     }
#'
#' @author Gordon W. Holtgrieve
#'
#' @keywords internal
#'
#' @export


EA.blank.correct <- function(results){

  #Internal function to perform the isotope mixing model.
  mixing.model <- function(d, A, d.blank, A.blank){
    R <- d/1000 + 1
    R.blank <- d.blank/1000 + 1

    R.corr <- ((R * A)-(R.blank * A.blank))/(A - A.blank)

    d.corr <- (R.corr-1)*1000

    return(d.corr)
  }

  ## Access data ##
  blank.flag <- results$blank.flag
  blank.CN <- results$blank.CN
  sample.CN <- results$sample.CN
  standard.CN <- results$standard.CN
  current.data.columns <- c("d.15N.14N", "d.13C.12C")

  if(blank.flag == F){
    print("Non-detectable blanks. Samples and standards were NOT blank corrected.")
    return(list(standard.CN=standard.CN, sample.CN=sample.CN, blank.correct.flag = "none", current.data.columns=current.data.columns))
  } else {
      # Determine if N, C, or both need blank correcting
    if(any(!is.na(blank.CN$d.15N.14N) & !is.na(blank.CN$d.13C.12C))){
      blank.correct.flag <- "both"
      print("Measurable carbon (C) and nitrogen (N) blanks.")
      print(blank.CN)
    } else if(any(!is.na(blank.CN$d.15N.14N) & is.na(blank.CN$d.13C.12C))){
      blank.correct.flag <- "N"
      print("Measurable nitrogen (N) blanks.")
      print(blank.CN)
    } else if(any(is.na(blank.CN$d.15N.14N) & !is.na(blank.CN$d.13C.12C))){
      blank.correct.flag <- "C"
      print("Measurable carbon (C) blanks.")
      print(blank.CN)
    }

    user.flag <- readline("Would you like to blank correct the data? Enter 'y' or 'n'.")

    if(user.flag == "n"){

      return(list(standard.CN=standard.CN, sample.CN=sample.CN, blank.correct.flag = "none", current.data.columns=current.data.columns))

    } else if (user.flag == "y"){

      if(blank.correct.flag == "N"){
        # Average across blanks
        mean.d15N.blank <- mean(blank.CN$d.15N.14N, na.rm = T)
        mean.area28.blank <- mean(blank.CN$Area.28, na.rm = T)

        # Apply mixing model
        sample.CN$d.15N.14N.blank <- mixing.model(sample.CN$d.15N.14N, sample.CN$Area.28, mean.d15N.blank, mean.area28.blank)
        standard.CN$d.15N.14N.blank <- mixing.model(standard.CN$d.15N.14N, standard.CN$Area.28, mean.d15N.blank, mean.area28.blank)
        current.data.columns <- c("d.15N.14N.blank", "d.13C.12C")

      } else if(blank.correct.flag == "C"){
        # Average across blanks
        mean.d13C.blank <- mean(blank.CN$d.13C.12C, na.rm = T)
        mean.area44.blank <- mean(blank.CN$Area.44, na.rm = T)

        # Apply mixing model
        sample.CN$d.13C.12C.blank <- mixing.model(sample.CN$d.13C.12C, sample.CN$Area.44, mean.d13C.blank, mean.area44.blank)
        standard.CN$d.13C.12C.blank <- mixing.model(standard.CN$d.13C.12C, standard.CN$Area.44, mean.d13C.blank, mean.area44.blank)
        current.data.columns <- c("d.15N.14N", "d.13C.12C.blank")

      } else if(blank.correct.flag == "both"){
        # Average across blanks
        mean.d15N.blank <- mean(blank.CN$d.15N.14N, na.rm = T)
        mean.area28.blank <- mean(blank.CN$Area.28, na.rm = T)
        mean.d13C.blank <- mean(blank.CN$d.13C.12C, na.rm = T)
        mean.area44.blank <- mean(blank.CN$Area.44, na.rm = T)

        # Apply mixing model
        sample.CN$d.15N.14N.blank <- mixing.model(sample.CN$d.15N.14N, sample.CN$Area.28, mean.d15N.blank, mean.area28.blank)
        standard.CN$d.15N.14N.blank <- mixing.model(standard.CN$d.15N.14N, standard.CN$Area.28, mean.d15N.blank, mean.area28.blank)
        sample.CN$d.13C.12C.blank <- mixing.model(sample.CN$d.13C.12C, sample.CN$Area.44, mean.d13C.blank, mean.area44.blank)
        standard.CN$d.13C.12C.blank <- mixing.model(standard.CN$d.13C.12C, standard.CN$Area.44, mean.d13C.blank, mean.area44.blank)
        current.data.columns <- c( "d.15N.14N.blank", "d.13C.12C.blank")
      }

      return(list(standard.CN=standard.CN, sample.CN=sample.CN,
                  blank.correct.flag=blank.correct.flag, current.data.columns=current.data.columns))
    }
  }
}
