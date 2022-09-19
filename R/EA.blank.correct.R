#' @title Blank corrects EA sample data. (Not Implemented)
#'
#' @description  (Currently Not Implemented) Analyzes blanks for the run and reports result to the user, then asking whether or not to
#'               blank correct standard and sample data. The correction is based on empty tins and a simple mixing model.
#'
#' @usage EA.blank.correct(results)
#'
#' @param results List containing results from previous functions.
#'
#' @author Gordon W. Holtgrieve
#'
#' @keywords internal
#'
#' @export


EA.blank.correct <- function(results){

  ## Access data ##
  blank.flag <- resulst$blank.flag
  blank.CN <- results$blank.CN
  sample.CN <- results$sample.CN
  standard.CN <- results$standard.CN

  if(blank.flag == F){

    print("Non-detectable blanks: Samples and Standards were NOT blank corrected.")
    return(list(standard.CN=standard.CN, sample.CN=sample.CN, blank.correct.flag=F))

  }


  #If there are detectable blanks, plot the blanks and ask if the data should be blank corrected.


}
