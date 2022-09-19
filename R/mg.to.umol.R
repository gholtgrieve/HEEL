  #' @title Converts mass or concentration data from mass to molar units
  #'
  #' @description This function is used to convert mass or concentration data from units of milligrams to micromoles.
  #'
  #' @usage mg.to.umol(x, mol)
  #'
  #' @param x Mass (or concentration) of the molecule, in milligrams, to be converted
  #' @param mol Character vector of length one indicating the molecule in question.
  #'
  #' @return Object same as 'x' but in units of micromoles
  #'
  #' @author Gordon W. Holtgrieve
  #'
  #' @export

mg.to.umol <- function(x, mol){
  # Error handling
  if(is.null(mol)) stop("Error: Missing argument 'mol'.")
  if(!is.character(mol)) stop("Error: Argument 'mol' must be of type character.")
  if(length(mol)>1) stop("Error: Argument 'mol' should be length == 1 (i.e., a single molecule.)")

  molar.mass <- get.molar.mass(mol = mol)

  out <- x / molar.mass * 1000  #Convert to micromolar units
  return(out)
}
