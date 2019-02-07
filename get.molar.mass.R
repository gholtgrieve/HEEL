#' Returns data on average molar mass for supported molecules/compounds.
#'
#' Convenience function for obtaining the average molar mass for supported molecules/compounds.
#' @usage
#' get.molar.mass(mol)
#' @param mol Character vector of molecules. Use "all" to get the full list of supported molecules.
#' @return Numeric vector of average molar masses same length as 'mol'.
#' @examples
#' get.molar.mass()  #returns full list of supported molecules
#' get.molar.mass(mol="CO2")
#' @author Gordon W. Holtgrieve
#' @export

get.molar.mass <- function(mol="all"){
  # Error handling
  if(is.null(mol)) stop("Error: Missing argument 'mol'.")
  if(!is.character(mol)) stop("Error: Argument 'mol' must be of type character.")
  if(any(!mol %in% ave.molar.mass$MOL & mol != "all")){
    stop(cat("Error: Supplied argument 'mol' not included in list of supported molecules.\n Call ave.molar.mass() to see current list.\n"))
  }

  if(mol=="all"){
    return(ave.molar.mass)
  } else{
    return(ave.molar.mass$x[match(mol, ave.molar.mass$MOL)])
  }

}
