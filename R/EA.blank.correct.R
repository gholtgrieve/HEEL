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

  ## Access data ##
  blank.flag <- results$blank.flag
  blank.CN <- results$blank.CN
  sample.CN <- results$sample.CN
  standard.CN <- results$standard.CN

  if(blank.flag == F){

    print("Non-detectable blanks. Samples and Standards were NOT blank corrected.")
    return(list(standard.CN=standard.CN, sample.CN=sample.CN, blank.correct.flag=F))

  } else {
    print("At least one blank was detectible. Samples and Standards were NOT blank corrected.")
    return(list(standard.CN=standard.CN, sample.CN=sample.CN, blank.correct.flag=F))
  }


  #FUTURE: If there are detectable blanks, plot the blanks and ask if the data should be blank corrected.


}
