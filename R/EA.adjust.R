#' @title Corrects raw d13C and 15N to VPBD and air
#'
#' @description This function takes the raw d13C and d15N from IsoDat software and corrects them to VPDB and air, respectively,
#'              based on the working standards from the run. Two-point calibration curves are generated based on measured and known
#'              values of the working standards; this calibration is then applied to both the samples and standards.
#'              The function also calculates the percent C and N of samples and standards in a similar fashion using known
#'              mass C and N in the glutamic acid standard (GA1) and peak area (masses 44 and 28).
#'
#' @usage EA.adjust(results)
#'
#' @param results List containing results from previous functions.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom tibble tibble
#' @importFrom stringr str_detect
#' @importFrom dplyr summarize filter group_by
#'
#' @return List with two tables: 1) standard C & N data ($standard.CN),
#'         sample C & N data ($sample.CN), and 2) calibration model coefficients ($calibration.coefficients).
#'
#' @author Gordon W. Holtgrieve
#'
#' @keywords internal
#' @export

EA.adjust <- function(results){

  known.standard.d13C.d15N<- data.frame(group = c("GA1", "GA2", "SALMON"),
                         d13C = c(-28.3, -13.7, -21.3),
                         d15N = c(-4.6, -5.7, 11.3))

  ## Access data ##
  sample.CN <- results$sample.CN
  standard.CN <- results$standard.CN
  drift.correct.flag <- results$drift.correct.flag
  blank.correct.flag <- results$blank.correct.flag

  # Pick the correct data depending on which corrections were performed.  This is ugly, but OK for now.
  if(str_detect(drift.correct.flag, "both|BOTH|Both")) {
    standard.d13C <- standard.CN$d.13C.12C.drift
    sample.d13C <- sample.CN$d.13C.12C.drift
    standard.d15N <- standard.CN$d.15N.14N.drift
    ssample.d15N <- sample.CN$d.15N.14N.drift
  } else if(str_detect(drift.correct.flag, "none|NONE|None")){
    standard.d13C <- standard.CN$d.13C.12C
    sample.d13C <- sample.CN$d.13C.12C
    standard.d15N <- standard.CN$d.15N.14N
    sample.d15N <- sample.CN$d.15N.14N
  } else if(str_detect(drift.correct.flag, "C|c")){
    standard.d13C <- standard.CN$d.13C.12C.drift
    sample.d13C <- sample.CN$d.13C.12C.drift
    standard.d15N <- standard.CN$d.15N.14N
    sample.d15N <- sample.CN$d.15N.14N
  } else if(str_detect(drift.correct.flag, "N|n")){
    standard.d15N <- standard.CN$d.15N.14N.drift
    sample.d15N <- sample.CN$d.15N.14N.drift
    standard.d13C <- standard.CN$d.13C.12C
    sample.d13C <- sample.CN$d.13C.12C
  } else if (blank.correct.flag){
    standard.d13C <- standard.CN$d.13C.12C.blank
    sample.d13C <- sample.CN$d.13C.12C.blank
    standard.d15N <- standard.CN$d.15N.14N.blank
    sample.d15N <- sample.CN$d.15N.14N.blank
  }
  standard.CN.temp <- data.frame(standard.d13C = standard.d13C,
                                 standard.d15N = standard.d15N,
                                 group = standard.CN$group)
  sample.CN.temp <- data.frame(sample.d13C = sample.d13C,
                               sample.d15N = sample.d15N)

  #Make data matrix to populate with means of the standard in the run(s)
  mean.standard.measured <- standard.CN.temp %>%
                          group_by(group) %>%
                          dplyr::summarize(d13C = mean(standard.d13C), d15N = mean(standard.d15N))

  ## Adjust d13 vs. tank to vs. VPDB using GA1 and GA2 working standards
  ## Build linear model using VPDB values on the y and drift corrected raw values on the x.
  x <- dplyr::filter(mean.standard.measured, group == "GA1" | group == "GA2")$d13C
  y <- dplyr::filter(known.standard.d13C.d15N, group == "GA1" | group == "GA2")$d13C
  C.lm <- lm(y ~ x)
  C.lm.coeff <- coefficients(C.lm)
  sample.CN$d.13C.12C.VPDB <- C.lm.coeff[1] + C.lm.coeff[2] * sample.CN.temp$sample.d13C
  standard.CN$d.13C.12C.VPDB <- C.lm.coeff[1] + C.lm.coeff[2] * standard.CN.temp$standard.d13C

  ## Adjust d13 vs. tank to vs. VPDB using GA1 and GA2 working standards
  ## Build linear model using VPDB values on the y and drift corrected raw values on the x.
  x <- dplyr::filter(mean.standard.measured, group == "SALMON" | group == "GA2")$d15N
  y <- dplyr::filter(known.standard.d13C.d15N, group == "SALMON" | group == "GA2")$d15N
  N.lm <- lm(y ~ x)
  N.lm.coeff <- coefficients(N.lm)
  sample.CN$d.15N.14N.air <- N.lm.coeff[1] + N.lm.coeff[2] * sample.CN.temp$sample.d15N
  standard.CN$d.15N.14N.air <- N.lm.coeff[1] + N.lm.coeff[2] * standard.CN.temp$standard.d15N

  #Calculate percent C using peak area vs. mass of GA1 QTY.
  mass.C.lm.coeff <-  dplyr::filter(standard.CN, group == "GA1", ) %>%
                      lm(mass.C.mg ~ Area.44, data = .) %>%
                      coefficients()
  sample.CN$percent.C <- (mass.C.lm.coeff[1] + mass.C.lm.coeff[2] * sample.CN$Area.44) / sample.CN$Amount * 100
  standard.CN$percent.C <- (mass.C.lm.coeff[1] + mass.C.lm.coeff[2] * standard.CN$Area.44) / standard.CN$Amount * 100

  #Calculate percent C using peak area vs. mass of GA1 QTY.
  mass.N.lm.coeff <-  filter(standard.CN, group == "GA1", ) %>%
    lm(mass.N.mg ~ Area.28, data = .) %>%
    coefficients()
  sample.CN$percent.N <- (mass.N.lm.coeff[1] + mass.N.lm.coeff[2] * sample.CN$Area.28) / sample.CN$Amount * 100
  standard.CN$percent.N <- (mass.N.lm.coeff[1] + mass.N.lm.coeff[2] * standard.CN$Area.28) / standard.CN$Amount * 100

  #Make model coefficients dataframe
  model.coeff.temp <- data.frame(Model=c("d13C", "d15N", "Percent C", "Percent N"),
                     rbind(C.lm.coeff, N.lm.coeff, mass.C.lm.coeff, mass.N.lm.coeff))
  colnames(model.coeff.temp) <- c("Value","Intercept", "Slope")
  calibration.coefficients <- tibble::tibble(model.coeff.temp)

  return(list(standard.CN=standard.CN, sample.CN=sample.CN, calibration.coefficients=calibration.coefficients))
}
