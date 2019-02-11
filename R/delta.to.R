#' Converts standard delta notation to isotopic ratio
#'
#' This function is used to convert isotope data in standard delta notation into isotopic ratio (R).
#' If argument 'std' is provided, then the raw ratio (R = heavy isotope / light isotope) is returned.
#' Do not use this option if ratios are given as light isotope / heavy isotope (not typical).  If 'std'
#' is NULL (default) then the 'grand ratio' (R-sample/R-standard) is returned.
#'
#' @usage
#' delta.to.R(x, std = NULL)
#' @param x Numeric.
#' @param std Character vector of length one indicating the accepted international standard the data are relative to.
#'            Possible options are:
#' \describe{
#'   \item{VPDB}{13C/12C = 0.011180 (note: carbon only)}
#'   \item{air-N}{15N/14N = 0.003676}
#'   \item{air-O}{18O/16O = 0.00205292}
#'   \item{VSMOW-O}{18O/16O = 0.00200520}
#'   \item{VSMOW-H}{D/H = 0.00015576}
#'   \item{VCDT}{34S/32S = 0.0441626}
#' }
#' @return Object same as 'x' with data as an isotopic ratio.
#' @examples
#' delta.to.R(10, std = "air")  # Returns R_sample
#' delta.to.R(1.0)  # Returns R_sample/R_standard
#' @author Gordon W. Holtgrieve
#' @export

delta.to.R <- function(x, std = NULL){
  STD.options <- c("VPDB", "air-N", "air-O", "VSMOW-O", "VSMOW-H", "VCDT")
  STD.R <- c(isotope.standards[isotope.standards$Name=="VPDB","R_13C12C"],
             isotope.standards[isotope.standards$Name=="air","R_15N14N"],
             isotope.standards[isotope.standards$Name=="air","R_18O16O"],
             isotope.standards[isotope.standards$Name=="VSMOW","R_18O16O"],
             isotope.standards[isotope.standards$Name=="VSMOW","R_DH"],
             isotope.standards[isotope.standards$Name=="VCDT","R_34S32S"])

  # Error handling
  if(is.null(std)){
    out <- x / 1000 + 1
  } else {
    if(length(std)>1) stop("Error: Argument 'std' should be length == 1 (i.e., a single reference standard.)")
    if(!(std %in% STD.options)) stop("Error: Argument 'std' is not one of the currently supported options.")

    Rstd <-STD.R[STD.options == std]

    out <- Rstd * (x / 1000 + 1)
  }

  return(out)

}
