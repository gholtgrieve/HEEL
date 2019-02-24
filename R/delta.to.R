#' Converts standard delta notation to isotopic ratio
#'
#' This function is used to convert isotope data in standard delta notation into isotopic ratio (R).
#' If arguments 'isotope.system' and 'reference.std' are provided, then the raw ratio (R = heavy isotope / light isotope) is returned.
#' Do not use this option if ratios are given as light isotope / heavy isotope (not typical).
#' If 'isotope.system' or 'reference.std' is NULL (default) then the 'grand ratio' (R-sample/R-standard) is returned.
#'
#' @usage
#' delta.to.R(x, isotope.system=NULL, reference.std = NULL)
#' @param x Numeric.
#' @param isotope.system  Character vector of length one indicating the (single) isotope system of interest. Currently supported isotope systems are "C", "N", "S", "H", "O18", "O17".
#' @param reference.std Character vector of length one indicating the reference standard the data are relative to.
#' @return Object same as 'x' with data as an isotopic ratio.
#' @examples
#' delta.to.R(1.0)  # Returns R_sample/R_standard (==1.001)
#' delta.to.R(10, isotope.system = "N", reference.std = "air")  # Returns ratio of heavy to light isotope (e.g., 15N/14N)
#' @author Gordon W. Holtgrieve
#' @export

delta.to.R <- function(x, isotope.system=NULL, reference.std = NULL){

  # Error handling
  if(is.null(reference.std) | is.null(isotope.system)){
    out <- x / 1000 + 1
  } else {
    if(length(reference.std)>1) stop("Error: Argument 'reference.std' should be length == 1 (i.e., a single reference standard.)")
    if(length(isotope.system)>1) stop("Error: Argument 'isotope.system' should be length == 1 (i.e., a single set of isotopes.)")
    std.R <- get.isotope.standard(std = reference.std, isotope.system = isotope.system)$std.R
    if(length(std.R)==0) stop("Error: Argument 'reference.std' or 'isotope.system' is not one of the currently supported options.")
    out <- std.R * (x / 1000 + 1)
  }

  return(out)

}
