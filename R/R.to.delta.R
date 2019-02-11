#' Converts isotopic ratio data to standard delta notation
#'
#' This function is used to convert isotopic ratio data (R) into standard delta notation.
#' If argument 'std' is provided, then the data should be the raw ratio (R = heavy isotope / light isotope).
#' Do not use this option if ratios are given as light isotope / heavy isotope (not typical).  If 'std'
#' is NULL (default) then data should be the 'grand ratio' (R-sample/R-standard).
#'
#' @usage
#' R.to.delta(x, std = NULL)
#' @param x Numeric.
#' @param std Character vector of length one indicating the accepted international standard the data are relative to.
#'            Possible options are:
#' \describe{
#'   \item{VPDB}{13C/12C = 0.01118 (note: carbon only)}
#'   \item{air-N}{15N/14N = 0.003676}
#'   \item{air-O}{18O/16O = 0.002053}
#'   \item{VSMOW-O}{18O/16O = 0.002005}
#'   \item{VSMOW-H}{D/H = 0.000156}
#'   \item{VCDT}{34S/32S = 0.043990}
#' }
#' @return Object same as 'x' with data in dleta notation.
#' @examples
#' @author Gordon W. Holtgrieve
#' @export

R.to.delta <- function(x, std = NULL){
  STD.options <- c("VPDB", "air-N", "air-O", "VSMOW-O", "VSMOW-H", "VCDT")
  STD.R <- c(isotope.standards[isotope.standards$Name=="VPDB","R_13C12C"],
             isotope.standards[isotope.standards$Name=="air","R_15N14N"],
             isotope.standards[isotope.standards$Name=="air","R_18O16O"],
             isotope.standards[isotope.standards$Name=="VSMOW","R_18O16O"],
             isotope.standards[isotope.standards$Name=="VSMOW","R_DH"],
             isotope.standards[isotope.standards$Name=="VCDT","R_34S32S"])

  # Error handling
  if(is.null(std)){
    out <- (x - 1) * 1000
  } else {
    if(length(std)>1) stop("Error: Argument 'std' should be length == 1 (i.e., a single reference standard.)")
    if(!(std %in% STD.options)) stop("Error: Argument 'std' is not one of the currently supported options.")

    Rstd <-STD.R[STD.options == std]

    out <- (x/Rstd - 1) * 1000
  }

  return(out)

}
