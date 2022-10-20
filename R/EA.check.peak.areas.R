#' @title Check peak areas
#'
#' @description This internal function check whether the peak areas of samples (masses 28 and 44) are 30%, 50% or 300% of
#' the target mass. Target masses are 200 ug C (mass 44) and 40 ug N (mass 38).
#'
#' @usage EA.check.peak.areas(results)
#'
#' @param results List containing results from previous functions.

#' @return Boolean dataframe
#'
#' @author Gordon W. Holtgrieve
#'
#' @keywords internal
#' @export

EA.check.peak.areas <- function(results){
  N.mass.vs.Area28.lm.coeff <- filter(results$standard.coefficients, Model == "MassC.vs.Area44")
  C.mass.vs.Area44.lm.coeff <- filter(results$standard.coefficients, Model == "MassN.vs.Area28")
  sample.CN <- results$sample.CN

  # Calculate the Area of masses 44 and 28 that corresponds to 1/2 of target mass
  # Target masses are 200 ug C and 40 ug N.
  target.N.mg <- 0.040
  target.C.mg <- 0.200

  area.28.50pct.threshold <- as.numeric((target.N.mg * 0.5 - N.mass.vs.Area28.lm.coeff[2]) / N.mass.vs.Area28.lm.coeff[3])
  area.44.50pct.threshold <- as.numeric((target.C.mg * 0.5 - C.mass.vs.Area44.lm.coeff[2]) / C.mass.vs.Area44.lm.coeff[3])

  area.28.30pct.threshold <- as.numeric((target.N.mg * 0.3 - N.mass.vs.Area28.lm.coeff[2]) / N.mass.vs.Area28.lm.coeff[3])
  area.44.30pct.threshold <- as.numeric((target.C.mg * 0.3 - C.mass.vs.Area44.lm.coeff[2]) / C.mass.vs.Area44.lm.coeff[3])

  area.28.300pct.threshold <- as.numeric((target.N.mg * 3 - N.mass.vs.Area28.lm.coeff[2]) / N.mass.vs.Area28.lm.coeff[3])
  area.44.300pct.threshold <- as.numeric((target.C.mg * 3 - C.mass.vs.Area44.lm.coeff[2]) / C.mass.vs.Area44.lm.coeff[3])

  #check samples for peak areas below the 50% and 30% thresholds.
  peak.area.flags <- data.frame(
    unique.ID = sample.CN$unique.ID,
    area.28.50pct.threshold.flag = sample.CN$Area.28 <= area.28.50pct.threshold & sample.CN$Area.28 >= area.28.30pct.threshold,
    area.28.30pct.threshold.flag = sample.CN$Area.28 <= area.28.30pct.threshold,
    area.28.300pct.threshold.flag = sample.CN$Area.28 >= area.28.300pct.threshold,
    area.44.50pct.threshold.flag = sample.CN$Area.44 <= area.44.50pct.threshold & sample.CN$Area.44 >= area.44.30pct.threshold,
    area.44.30pct.threshold.flag = sample.CN$Area.44 <= area.44.30pct.threshold,
    area.44.300pct.threshold.flag = sample.CN$Area.44 >= area.44.300pct.threshold
  )


  return(list(peak.area.flags))
}
