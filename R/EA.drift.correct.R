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
#' @importFrom stringr str_detect
#' @importFrom dplyr between filter
#'
#' @return List with two tables and a Boolean flag:
#'   \describe{
#'     \item{standard.CN}{Dataframe of raw data IsoDat data for the standards and QTY standards; updated to with new column for drift corrected data, if required.}
#'     \item{sample.CN}{Dataframe of raw data IsoDat data for the unknown samples form the sequence; updated to with new column for drift corrected data, if required.}
#'     \item{drift.correct.flag}{Boolean flag to indicate if a drift correction was performed.}
#'     }
#'
#' @author Gordon W. Holtgrieve
#'
#' @keywords internal
#' @export


EA.drift.correct <- function(results){

  #Internal function to do a linear drift correct on a given set of data.
  drift.correct.function <- function(data, row, driftAnalysis, element){

    if(element == "N"){
      drift.slope <- mean(as.numeric(driftAnalysis[,1]))
    } else if(element == "C"){
      drift.slope <- mean(as.numeric(driftAnalysis[,4]))
    }
    output <- data - drift.slope * row
    return(output)
  }

  ##Load data##
  standard.CN <- results$standard.CN
  sample.CN <- results$sample.CN
  blank.correct.flag <- results$blank.correct.flag
  current.data.columns <- results$current.data.columns
  standards.in.run <- results$standards.in.run

  standard.CN.temp <- standard.CN[,c("Analysis", "Row", "Comment", "group", "Area.28", "Area.44", current.data.columns)]
  sample.CN.temp <- sample.CN[,c("Analysis", "Row", "Comment", "Area.28", "Area.44", current.data.columns)]
  names(standard.CN.temp) <- c("Analysis", "Row", "Comment", "group", "Area.28", "Area.44", "d15N", "d13C")
  names(sample.CN.temp) <- c("Analysis", "Row", "Comment", "Area.28", "Area.44", "d15N", "d13C")

  #Decide which standards to use
  STD1 <- "GA1"
  STD2 <- "GA2"
  if("SALMON" %in% standards.in.run){
    STD3 <- "SALMON"
  } else if ("PL" %in% standards.in.run) {
    STD3 <- "PL"
    print("Using peach leaves standard in place of salmon.")
  }

  # Check to see if there are standards in the run that can be used to drift correct.
  # Appropriate standards would be at least 4 repeated measures of GA1, GA2 and salmon that are at approximately the same mass.
  # This excludes QTY standard that vary in their sample mass..
  standards.flag <- nrow(filter(standard.CN.temp, Comment %in% c("STANDARD") & group %in% c(STD1))) >= 4
  standards.flag <- c(standards.flag, nrow(filter(standard.CN.temp, Comment %in% c("STANDARD") & group %in% c(STD2))) >= 4)
  standards.flag <- c(standards.flag, nrow(filter(standard.CN.temp, Comment %in% c("STANDARD") & group %in% c(STD3))) >= 4)

  drift.correct.flag1 <- any(standards.flag)

  if(!drift.correct.flag1){  #Tests if none of the above checks are TRUE

    print("Samples and Standards NOT drift corrected becuase of insufficent data to perform calculation.")
    return(list(standard.CN=standard.CN, sample.CN=sample.CN,
                drift.correct.flag = "none", current.data.columns=current.data.columns))

  } else if(drift.correct.flag1){

    # Check for Drift -----------------------------
    # Calculate the sign and confidence interval of the slope for each drift standard (STD)
    driftAnalysis <- matrix(data=NA, nrow=3, ncol=6)
    colnames(driftAnalysis) <- c("slope.N", "intercept.N", "slopeCI.N",
                                   "slope.C", "intercept.C", "slopeCI.C")
    rownames(driftAnalysis) <- c(STD1, STD2, STD3)

    #use on STD (not QTY)
    dataSTD <- standard.CN.temp[standard.CN.temp$Comment == "STANDARD",]

    group <- c(STD1, STD2, STD3)

    for(i in 1:3){
      tempData <- dataSTD[stringr::str_detect(dataSTD$group, group[i]),]

      if(nrow(tempData)==0){
        #do nothing
      } else {
        lm.drift.N <- lm(data = tempData, formula = d15N ~ Row)
        lm.drift.C <- lm(data = tempData, formula = d13C ~ Row)

        # Record slope of the regression line
        driftAnalysis[i,"slope.N"] <- round(lm.drift.N$coefficients[2], 5)
        driftAnalysis[i,"slope.C"] <- round(lm.drift.C$coefficients[2], 5)
        driftAnalysis[i,"intercept.N"] <- round(lm.drift.N$coefficients[1], 3)
        driftAnalysis[i,"intercept.C"] <- round(lm.drift.C$coefficients[1], 3)

        # Does the confidence interval for all of the LM slope values exclude zero?
        if(dplyr::between(0, confint(lm.drift.N, "Row", level = 0.95)[1], confint(lm.drift.N, "Row", level = 0.95)[2])){
          driftAnalysis[i,"slopeCI.N"] <- "Contains Zero"
        } else {
          driftAnalysis[i,"slopeCI.N"] <- "Does Not Contain Zero"
        }
        if(dplyr::between(0, confint(lm.drift.C, "Row", level = 0.95)[1], confint(lm.drift.C, "Row", level = 0.95)[2])){
          driftAnalysis[i,"slopeCI.C"] <- "Contains Zero"
        } else {
          driftAnalysis[i,"slopeCI.C"] <- "Does Not Contain Zero"
        }
      }
    }

    print("Here are the results of the drift analysis:")
    print(driftAnalysis)
    drift.correct.flag2 <- readline("Do you want to drift correct? Allowed responses: N, C, both, none.")

    # Standardize case to UPPER
      drift.correct.flag2 <- stringr::str_to_upper(drift.correct.flag2)

    # Flow Control
    # If the user indicates no drift correction, return the original data.
    if(drift.correct.flag2 == "NONE") {
      # If drift.correct.flag is false (i.e., do not drift correct), then do nothing and change the
      return(list(standard.CN=standard.CN, sample.CN=sample.CN, drift.correct.flag=drift.correct.flag2, current.data.columns=current.data.columns))
    }

    # If the user indicates drift correction of N or both C and N, do this.
    if (drift.correct.flag2 == "N" | drift.correct.flag2 == "BOTH") {
      sample.CN$d.15N.14N.drift <- drift.correct.function(data = sample.CN.temp$d15N, row = sample.CN.temp$Row,
                                                          driftAnalysis = driftAnalysis, element = "N")
      standard.CN$d.15N.14N.drift <- drift.correct.function(data = standard.CN.temp$d15N, row = standard.CN.temp$Row,
                                                            driftAnalysis = driftAnalysis, element = "N")
      current.data.columns[1] <-c("d.15N.14N.drift")
    }

    # If the user indicates drift correction of C or both C and N, do this.
    if (drift.correct.flag2 == "C" | drift.correct.flag2 == "BOTH") {
       sample.CN$d.13C.12C.drift <- drift.correct.function(data = sample.CN.temp$d13C, row = sample.CN.temp$Row,
                                                          driftAnalysis = driftAnalysis, element = "C")

       standard.CN$d.13C.12C.drift <- drift.correct.function(data = standard.CN.temp$d13C, row = standard.CN.temp$Row,
                                                             driftAnalysis = driftAnalysis, element = "C")
       current.data.columns[2] <-c("d.13C.12C.drift")
    }

    #Return results
    return(list(standard.CN=standard.CN, sample.CN=sample.CN,
                drift.correct.flag=drift.correct.flag2, current.data.columns=current.data.columns))
  }
}

