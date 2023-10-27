#' @title Calculates precision and accuracy metrics of finalized data from NACHO
#'
#' @description Calculates precision and accuracy of d13C, d15N, mass percent C, and mass percent N.
#'
#' @usage EA.check(results)
#'
#' @return List with one tibble:
#'    \describe{
#'      \item{error.analysis.results}{Table containing accuracy and precision of derived data.}
#'    }
#'
#' @author Gordon W. Holtgrieve
#'
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
#' @export

EA.check <- function(results){

  ## Load data ##
  known.standard.values <- results$known.standard.values
  standard.CN <- results$standard.CN
  standards.in.run <- results$standards.in.run

  error.analysis.results <- tibble::tibble(
                                Value = c("d13C_VPDB", "d15N_air", "mass.percent.C", "mass.percent.N"),
                                Precision = NA,
                                Accuracy = NA)

  #Calculate mean measured values or d13C, d15N, pct C and pct N. Use salmon or peach leaves standard for C and GA1 for N.
  mean.STD3.d13C.measured <- mean(standard.CN$d.13C.12C.VPDB[standard.CN$group == standards.in.run[3]], na.rm = T)
  mean.STD3.pctC.measured <- mean(standard.CN$mass.percent.C[standard.CN$group == standards.in.run[3]], na.rm = T)
  mean.STD1.d15N.measured <- mean(standard.CN$d.15N.14N.air[standard.CN$group == standards.in.run[1]], na.rm = T)
  mean.STD1.pctN.measured <- mean(standard.CN$mass.percent.N[standard.CN$group == standards.in.run[1]], na.rm = T)

  #Calculate accuracy for d13C, d15N, pctC, and pctN. Use salmon or peach leaves standard for C and GA1 for N.
  error.analysis.results$Accuracy[error.analysis.results$Value == "d13C_VPDB"] <- mean.STD3.d13C.measured - filter(known.standard.values, group==standards.in.run[3])$d13C_VPDB
  error.analysis.results$Accuracy[error.analysis.results$Value == "mass.percent.C"] <- mean.STD3.pctC.measured - filter(known.standard.values, group==standards.in.run[3])$mass.percent.C
  error.analysis.results$Accuracy[error.analysis.results$Value == "d15N_air"] <- mean.STD1.d15N.measured - filter(known.standard.values, group==standards.in.run[1])$d15N_air
  error.analysis.results$Accuracy[error.analysis.results$Value == "mass.percent.N"] <- mean.STD1.pctN.measured - filter(known.standard.values, group==standards.in.run[1])$mass.percent.N

  #Calculate precision for d13C, d15N, pctC, and pctN. Use salmon or peach leaves standard for C and GA1 for N.
  error.analysis.results$Precision[error.analysis.results$Value == "d13C_VPDB"] <- sd(standard.CN$d.13C.12C.VPDB[standard.CN$group == standards.in.run[3]], na.rm = T)
  error.analysis.results$Precision[error.analysis.results$Value == "mass.percent.C"] <- sd(standard.CN$mass.percent.C[standard.CN$group == standards.in.run[3]], na.rm = T)
  error.analysis.results$Precision[error.analysis.results$Value == "d15N_air"] <- sd(standard.CN$d.15N.14N.air[standard.CN$group == standards.in.run[1]], na.rm = T)
  error.analysis.results$Precision[error.analysis.results$Value == "mass.percent.N"] <- sd(standard.CN$mass.percent.N[standard.CN$group == standards.in.run[1]], na.rm = T)

  return(list(error.analysis.results = error.analysis.results))
}
