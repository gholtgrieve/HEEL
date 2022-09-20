#' @title Calculates precision and accuracy metrics of finalized data from NACHO
#'
#' @description This function calculates the r
#'
#' @usage EA.check(results)
#'
#' @param results List containing results from previous functions.
#'
#' @return List with two objects: known.standard.values, error.analysis.results
#'
#' @author Gordon W. Holtgrieve
#'
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
#' @export

EA.check <- function(results){

  known.standard.values <- data.frame(group = c("GA1", "GA2", "SALMON"),
                             d13C = c(-28.3, -13.7, -21.3),
                             d15N = c(-4.6, -5.7, 11.3),
                             pctC = c(40.8168, 40.8168, 45.7),
                             pctN = c(9.52, 9.52, 11.83))

  ## Access data ##
  standard.CN <- results$standard.CN

  error.analysis.results <- tibble::tibble(
                                Value = c("d13C", "d15N", "pctC", "pctN"),
                                Precision = NA,
                                Accuracy = NA)

  #Calculate mean measured values dor d13C, d15N, pct C and pct N. Use salmon standard for C and GA1 for N.
  mean.salmon.d13C.measured <- mean(standard.CN$d.13C.12C.VPDB[standard.CN$group == "SALMON"], na.rm = T)
  mean.salmon.pctC.measured <- mean(standard.CN$pctC[standard.CN$group == "SALMON"], na.rm = T)
  mean.GA1.d15N.measured <- mean(standard.CN$d.15N.14N.air[standard.CN$group == "GA1"], na.rm = T)
  mean.GA1.pctN.measured <- mean(standard.CN$pctN[standard.CN$group == "GA1"], na.rm = T)

  #Calculate accuracy for d13C, d15N, pctC, and pctN. Use salmon standard for C and GA1 for N.
  error.analysis.results$Accuracy[error.analysis.results$Value == "d13C"] <- mean.salmon.d13C.measured - known.standard.values[3, "d13C"]
  error.analysis.results$Accuracy[error.analysis.results$Value == "pctC"] <- mean.salmon.pctC.measured - known.standard.values[3, "pctC"]
  error.analysis.results$Accuracy[error.analysis.results$Value == "d15N"] <- mean.GA1.d15N.measured - known.standard.values[1, "d15N"]
  error.analysis.results$Accuracy[error.analysis.results$Value == "pctN"] <- mean.GA1.pctN.measured - known.standard.values[1, "pctN"]

  #Calculate precision for d13C, d15N, pctC, and pctN. Use salmon standard for C and GA1 for N.
  error.analysis.results$Precision[error.analysis.results$Value == "d13C"] <- sd(standard.CN$d.13C.12C.VPDB[standard.CN$group == "SALMON"], na.rm = T)
  error.analysis.results$Precision[error.analysis.results$Value == "pctC"] <- sd(standard.CN$pctC[standard.CN$group == "SALMON"], na.rm = T)
  error.analysis.results$Precision[error.analysis.results$Value == "d15N"] <- sd(standard.CN$d.15N.14N.VPDB[standard.CN$group == "GA1"], na.rm = T)
  error.analysis.results$Precision[error.analysis.results$Value == "pctN"] <- sd(standard.CN$pctN[standard.CN$group == "GA1"], na.rm = T)

  return(list(known.standard.values = known.standard.values,
              error.analysis.results = error.analysis.results))
}
