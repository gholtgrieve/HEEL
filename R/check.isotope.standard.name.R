#' Checks given names of an isotope standard against a list of possible names.
#'
#' Convenience function for checking a given name of an isotope standard against a list of possible names,
#' returning the 'offical' name for use going forward.
#'
#' @usage
#' check.isotope.standard.name(x)
#' @param x Proposed name to be checked against list of possible names.
#' @return Character vector contaiing the 'offical' name of the standard.
#' @author Gordon W. Holtgrieve
#' @export

check.isotope.standard.name <- function(x){
  # Error handling
  if(any(is.null(x))) stop("Error: Missing the standard name to be checked.")
  if(any(!is.character(x))) stop("Error: The standard name must be of type character.")

  n <- length(x)
  out <- vector("character", n)
  for (i in seq(1,n,1)){

      flag <- T
      index = 1
      while(flag){
        if(index > length(isotope.standard.names)) {
          warning("The name of the standard you entered does not exist in our database.")
          break
        }
        if(x[i] %in% isotope.standard.names[[index]]){
          out[i] = names(isotope.standard.names)[index]
          flag = F
        } else {
          index = index + 1
        }
      }
  }

  return(out)
}
