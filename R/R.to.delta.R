#' @title Converts isotopic ratio data to standard delta notation
#'
#' @description This function is used to convert isotopic ratio data (R) into standard delta notation.
#' If arguments 'isotope.system' and 'reference.std' are provided, then x should be the raw ratio (R = heavy isotope / light isotope).
#' Do not use this option if ratios are given as light isotope / heavy isotope (not typical). If 'isotope.system' or 'reference.std' is
#' NULL (default) then x should be the 'grand ratio' (R-sample/R-standard).
#'
#' @usage R.to.delta(x, isotope.system=NULL, reference.std = NULL)
#'
#' @param x Numeric.
#' @param isotope.system  Character vector of length one indicating the (single) isotope system of interest.
#'                        Currently supported isotope systems are "C", "N", "S", "H", "O18", "O17".
#' @param reference.std Character vector of length one indicating the reference standard the data are relative to.
#'
#' @return Object same as 'x' with data in delta notation rounded to two decimal places.
#'
#' @author Gordon W. Holtgrieve
#'
#' @export

R.to.delta <- function(x, isotope.system=NULL, reference.std = NULL){

  # Error handling
  if(is.null(reference.std) | is.null(isotope.system)){
    out <- (x - 1) * 1000
  } else {
    if(length(reference.std)>1) stop("Error: Argument 'reference.std' should be length == 1 (i.e., a single reference standard.)")
    if(length(isotope.system)>1) stop("Error: Argument 'isotope.system' should be length == 1 (i.e., a single set of isotopes.)")
    std.R <- get.isotope.standard(std = reference.std, isotope.system = isotope.system)$std.R
    if(length(std.R)==0) stop("Error: Argument 'reference.std' or 'isotope.system' is not one of the currently supported options.")
    out <- (x/std.R - 1) * 1000
  }

  return(round(out,digits=2))


}
