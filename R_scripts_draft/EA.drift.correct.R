#' @title Drift corrects raw data from the EA based on standards
#'
#' @description This function uses repeated analysis of known standard to check for an, if desired, correct raw data for instrumental drift.
#'              The function first test for sufficient data to make the correction. If data are insufficient, the raw data ar copied
#'              unchanged to new column, .csv written, and outcome printed to the console. If the data are sufficient, a drift analysis
#'              is done and plots generated. The user is asked if they want to drift correct the C data, N data, or both. The function
#'              returns a character vector that indicates the results of the drift correct as  N, C, both, or none.
#'
#' @usage EA.drift.correct(results)
#'
#' @param results  List containing results from previous functions.
#'
#' @import tidyverse
#' @importFrom tools file_path_sans_ext
#'
#' @return Character vector of length 1 indicating the results of the drift correct as  N, C, both, or none.
#'
#' @author Gordon W. Holtgrieve
#'
#' @keywords internal
#' @export


EA.drift.correct <- function(results){

  #Internal function to do the linear drift correct on a given set of data.
  drift.correct.function <- function(data, row, driftAnalysis, element){

    if(element == "N"){
      drift.slope <- mean(driftAnalysis[,1])
    } else if(element == "C"){
      drift.slope <- mean(driftAnalysis[,4])
    }
    output <- data - drift.slope * row
    return(output)
  }

  sample.CN <- results$sample.CN
  standard.CN <- results$standard.CN

  # Check to see if there are standards in the run that can be used to drift correct.
  # Appropriate standards would be at least 4 repeated measures of GA1, GA2 and salmon that are at approximately the same mass.
  # This excludes QTY standard that vary in their sample mass..
  standards.flag <- nrow(filter(standard.CN, Comment %in% c("STD","std", "Std") & group %in% c("GA1", "ga1", "Ga1"))) >= 4
  standards.flag <- c(standards.flag, nrow(filter(standard.CN, Comment %in% c("STD","std", "Std") & group %in% c("GA2", "ga2", "Ga2"))) >= 4)
  standards.flag <- c(standards.flag, nrow(filter(standard.CN, Comment %in% c("STD","std", "Std") & group %in% c("SALMON", "Salmon", "salmon"))) >= 4)

  drift.correct.flag1 <- any(standards.flag)

  if(!drift.correct.flag1){  #Tests if none of the above checks are TRUE

    print("Samples and Standards NOT drift corrected becuase of insufficent data to perform calculation.")
    return(list(standard.CN=standard.CN, sample.CN=sample.CN, drift.correct.flag = drift.correct.flag))

  } else if(drift.correct.flag1){

    # Check for Drift -----------------------------
    # Calculate the sign and confidence interval of the slope for each drift standard (STD)
    driftAnalysis <- matrix(data=NA, nrow=3, ncol=6)
    colnames(driftAnalysis) <- c("slope.N", "intercept.N", "slopeCI.N",
                                   "slope.C", "intercept.C", "slopeCI.C")
    rownames(driftAnalysis) <- c("GA1", "GA2", "SALMON")

    #use on STD (not QTY)
    dataSTD <- standard.CN[str_detect(data$Comment, "STD|std|Std"),]

    group <- c("GA1", "GA2", "SALMON")

    for(i in 1:3){
      tempData <- dataSTD[str_detect(dataSTD$group, group[i]),]

      if(nrow(tempData)==0){
        #do nothing
      } else {
        lm.drift.N <- lm(data = tempData, formula = d.15N.14N ~ Analysis)
        lm.drift.C <- lm(data = tempData, formula = d.13C.12C ~ Analysis)

        # Record slope of the regression line
        driftAnalysis[i,"slope.N"] <- round(lm.drift.N$coefficients[2], 5)
        driftAnalysis[i,"slope.C"] <- round(lm.drift.C$coefficients[2], 5)
        driftAnalysis[i,"intercept.N"] <- round(lm.drift.N$coefficients[1], 3)
        driftAnalysis[i,"intercept.C"] <- round(lm.drift.C$coefficients[1], 3)

        # Does the confidence interval for all of the LM slope values exclude zero?
        if(dplyr::between(0, confint(lm.drift.N, "Analysis", level = 0.95)[1], confint(lm.drift.N, "Analysis", level = 0.95)[2])){
          driftAnalysis[i,"slopeCI.N"] <- "Contains Zero"
        } else {
          driftAnalysis[i,"slopeCI.N"] <- "Does Not Contain Zero"
        }
        if(dplyr::between(0, confint(lm.drift.C, "Analysis", level = 0.95)[1], confint(lm.drift.C, "Analysis", level = 0.95)[2])){
          driftAnalysis[i,"slopeCI.C"] <- "Contains Zero"
        } else {
          driftAnalysis[i,"slopeCI.C"] <- "Does Not Contain Zero"
        }
      }
    }

    print("Here are the results of the drift analysis:")
    print(driftAnalysis)
    drift.correct.flag2 <- readline("Do you want to drift correct? Allowed responses: N, C, both, none.")

    # flow control
    if(str_detect(drift.correct.flag2, "none|NONE|None")) {
      # If drift.correct.flag is false (i.e., do not drift correct), then do nothing and change the
      return(list(standard.CN=standard.CN, sample.CN=sample.CN, drift.correct.flag=drift.correct.flag2))
    }

    if (str_detect(drift.correct.flag2, "C|c") | str_detect(drift.correct.flag2, "both|BOTH|Both")) {
       sample.CN$d.13C.12C.drift <- drift.correct.function(data = sample.CN$d.13C.12C, row = sample.CN$Row,
                                                          driftAnalysis = driftAnalysis, element = "C")

       standard.CN$d.13C.12C.drift <- drift.correct.function(data = standard.CN$d.13C.12C, row = standard.CN$Row,
                                                             driftAnalysis = driftAnalysis, element = "C")
    }


    if (str_detect(drift.correct.flag2, "N|n") | str_detect(drift.correct.flag2, "both|BOTH|Both")) {
       sample.CN$d.15N.14N.drift <- drift.correct.function(data = sample.CN$d.15N.14N, row = sample.CN$Row,
                                                           driftAnalysis = driftAnalysis, element = "N")

      standard.CN$d.15N.14N.drift <- drift.correct.function(data = standard.CN$d.15N.14N, row = standard.CN$Row,
                                                            driftAnalysis = driftAnalysis, element = "N")
    }

    return(list(standard.CN=standard.CN, sample.CN=sample.CN, drift.correct.flag=drift.correct.flag2))
  }
}

