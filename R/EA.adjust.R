#' @title Corrects raw d13C and 15N to vs. VPBD and air
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
#' @return List containing four dataframes:
#'   \describe{
#'     \item{standard.CN}{Dataframe of raw data IsoDat data for the standards and QTY standards; updated to with new column for final corrected data.}
#'     \item{sample.CN}{Dataframe of raw data IsoDat data for the unknown samples form the sequence; updated to with new column for final corrected data.}
#'     \item{calibration.coefficients}{Dataframe of slope and intercept values used during data standardization.}
#'     \item{measured.standard.means}{Dataframe of average d13C and d15N of standards vs. tank and vs. international standards.}
#'    }
#'
#' standard C & N data ($standard.CN),
#'         sample C & N data ($sample.CN),
#'         and calibration model coefficients ($calibration.coefficients).
#'
#' @author Gordon W. Holtgrieve
#'
#' @keywords internal
#' @export

EA.adjust <- function(results){

  ## Load data ##
  known.standard.values <- results$known.standard.values
  sample.CN <- results$sample.CN
  standard.CN <- results$standard.CN
  return.mass.percent.CN <- results$return.mass.percent.CN
  standards.in.run <- results$standards.in.run
  current.data.columns <- results$current.data.columns
  standard.coefficients <- results$standard.coefficients

  #
  standard.d15N <- standard.CN[,current.data.columns[1]]
  standard.d13C <- standard.CN[,current.data.columns[2]]
  sample.d15N <- sample.CN[,current.data.columns[1]]
  sample.d13C <- sample.CN[,current.data.columns[2]]

  standard.CN.temp <- standard.CN[,c("group", current.data.columns)]
  names(standard.CN.temp) <- c("group","d15N", "d13C")

  sample.CN.temp <- sample.CN[,current.data.columns]
  names(sample.CN.temp) <- c("d15N", "d13C")


  #Make data matrix to populate with means of the standard raw values in the run(s)
  measured.standard.means.raw <- standard.CN.temp %>%
                          group_by(group) %>%
                          dplyr::summarize(d13C_raw = mean(d13C), d15N_raw = mean(d15N))

  #Decide which standards to use
  STD1 <- "GA1"
  STD2 <- "GA2"
  if("SALMON" %in% standards.in.run){
    STD3 <- "SALMON"
  } else if ("PL" %in% standards.in.run) {
    STD3 <- "PL"
    print("Using peach leaves standard in place of salmon.")
  }

  ## Adjust d13 vs. tank to vs. VPDB using GA1 and GA2 working standards
  ## Build linear model using VPDB values on the y and drift corrected raw values on the x.
  x <- dplyr::filter(measured.standard.means.raw, group == STD1 | group == STD2)$d13C_raw
  y <- dplyr::filter(known.standard.values, group == STD1 | group == STD2)$d13C
  C.lm <- lm(y ~ x)
  C.lm.coeff <- coefficients(C.lm)
  sample.CN$d.13C.12C.VPDB <- C.lm.coeff[1] + C.lm.coeff[2] * sample.CN.temp$d13C
  standard.CN$d.13C.12C.VPDB <- C.lm.coeff[1] + C.lm.coeff[2] * standard.CN.temp$d13C

  ## Adjust d13 vs. tank to vs. VPDB using GA1 and GA2 working standards
  ## Build linear model using VPDB values on the y and drift corrected raw values on the x.
  x <- dplyr::filter(measured.standard.means.raw, group == STD3 | group == STD2)$d15N_raw
  y <- dplyr::filter(known.standard.values, group == STD3 | group == STD2)$d15N
  N.lm <- lm(y ~ x)
  N.lm.coeff <- coefficients(N.lm)
  sample.CN$d.15N.14N.air <- N.lm.coeff[1] + N.lm.coeff[2] * sample.CN.temp$d15N
  standard.CN$d.15N.14N.air <- N.lm.coeff[1] + N.lm.coeff[2] * standard.CN.temp$d15N

  #Calculate percent C using peak area vs. mass of GA1 and/or GA2 QTY.
  mass.C.lm.coeff <- filter(standard.coefficients, Model=="MassC.vs.Area44")
  mass.N.lm.coeff <- filter(standard.coefficients, Model=="MassN.vs.Area28")

  if(return.mass.percent.CN == F){
    sample.CN$mg.C <- (mass.C.lm.coeff$Intercept + mass.C.lm.coeff$Slope * sample.CN$Area.44)
    standard.CN$mg.C <- (mass.C.lm.coeff$Intercept + mass.C.lm.coeff$Slope * standard.CN$Area.44)
    sample.CN$mg.N <- (mass.N.lm.coeff$Intercept + mass.N.lm.coeff$Slope * sample.CN$Area.28)
    standard.CN$mg.N <- (mass.N.lm.coeff$Intercept + mass.N.lm.coeff$Slope * standard.CN$Area.28)
  } else {
    sample.CN$mass.percent.C <- (mass.C.lm.coeff$Intercept + mass.C.lm.coeff$Slope * sample.CN$Area.44) / sample.CN$Amount * 100
    standard.CN$mass.percent.C <- (mass.C.lm.coeff$Intercept + mass.C.lm.coeff$Slope * standard.CN$Area.44) / standard.CN$Amount * 100
    sample.CN$mass.percent.N <- (mass.N.lm.coeff$Intercept + mass.N.lm.coeff$Slope * sample.CN$Area.28) / sample.CN$Amount * 100
    standard.CN$mass.percent.N <- (mass.N.lm.coeff$Intercept + mass.N.lm.coeff$Slope * standard.CN$Area.28) / standard.CN$Amount * 100
  }

  #Make model coefficients dataframe
  model.coeff.temp <- data.frame(Model=c("d13C", "d15N", "mass.percent C", "mass.percent N"),
                     rbind(C.lm.coeff, N.lm.coeff, mass.C.lm.coeff[1,2-3], mass.N.lm.coeff[1,2-3]))
  colnames(model.coeff.temp) <- c("Value","Intercept", "Slope")
  calibration.coefficients <- tibble::tibble(model.coeff.temp)

  #Make data matrix to populate with means of the standard  adjusted values in the run(s)
  measured.standard.means.adjusted <- standard.CN %>%
    group_by(group) %>%
    dplyr::summarize(d13C_VPDB = mean(d.13C.12C.VPDB), d15N_air = mean(d.15N.14N.air))

  return(list(standard.CN=standard.CN, sample.CN=sample.CN,
              calibration.coefficients=calibration.coefficients,
              measured.standard.means = bind_cols(measured.standard.means.raw, measured.standard.means.adjusted[,2:3])))
}
