#' @title Blank corrects EA sample data. (Not Implemented)
#'
#' @description  (Currently Not Implemented) Analyzes blanks for the run and reports result to the user, then asking whether or not to
#'               blank correct standard and sample data. The correction is based on empty tins and a simple mixing model.
#'
#' @usage EA.blank.correct(results)
#'
#' @return List with two tables: standard C & N data ($standard.CN),
#'         sample C & N data ($sample.CN), and a flag indicating if a
#'         blank correction was performed ($blank.correct.flag).
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

  if(blank.flag == F){

    print("Non-detectable blanks. Samples and Standards were NOT blank corrected.")
    return(list(standard.CN=standard.CN, sample.CN=sample.CN, blank.correct.flag = "none"))

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

      return(list(standard.CN=standard.CN, sample.CN=sample.CN, blank.correct.flag = "none"))

    } else if (user.flag == "y"){

      if(blank.correct.flag == "both" | blank.correct.flag == "N"){
        # Average across blanks
        mean.d15N.blank <- mean(blank.CN$d.15N.14N, na.rm = T)
        mean.area28.blank <- mean(blank.CN$Area.28, na.rm = T)

        # Apply mixing model
        sample.CN$d.15N.14N.blank <- mixing.model(sample.CN$d.15N.14N, sample.CN$Area.28, mean.d15N.blank, mean.area28.blank)
        standard.CN$d.15N.14N.blank <- mixing.model(standard.CN$d.15N.14N, standard.CN$Area.28, mean.d15N.blank, mean.area28.blank)
      }

      if(blank.correct.flag == "both" | blank.correct.flag == "C"){
        # Average across blanks
        mean.d13C.blank <- mean(blank.CN$d.13C.12C, na.rm = T)
        mean.area44.blank <- mean(blank.CN$Area.44, na.rm = T)

        # Apply mixing model
        sample.CN$d.13C.12C.blank <- mixing.model(sample.CN$d.13C.12C, sample.CN$Area.44, mean.d13C.blank, mean.area44.blank)
        standard.CN$d.13C.12C.blank <- mixing.model(standard.CN$d.13C.12C, standard.CN$Area.44, mean.d13C.blank, mean.area44.blank)
      }

      return(list(standard.CN=standard.CN, sample.CN=sample.CN, blank.correct.flag=blank.correct.flag))
    }
  }
}
