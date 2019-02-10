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
#'   \item{VPDB}{}
#'   \item{air-N}{}
#'   \item{air-O}{}
#'   \item{VSMOW-O}{}
#'   \item{VSMOW-H}{}
#'   \item{VCDT}{}
#' }
#' @return Object same as 'x' with data as an isotopic ratio.
#' @examples
#' delta.to.R(10, std = "air")  # Returns R_sample
#' delta.to.R(1.0)  # Returns R_sample/R_standard
#' @author Gordon W. Holtgrieve
#' @export

delta.to.R <- function(x, std = NULL){
  # Error handling
  if(is.null(std)) stop("Error: Missing argument 'mol'."){

  } else
  if(!is.character(mol)) stop("Error: Argument 'mol' must be of type character.")
  if(length(mol)>1) stop("Error: Argument 'std' should be length == 1 (i.e., a single reference standard.)")

  molar.mass <- get.molar.mass(mol = mol)

  out <- x / molar.mass * 1000  #Convert to micromolar units
  return(out)
}
