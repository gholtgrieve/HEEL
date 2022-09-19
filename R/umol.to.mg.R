  #' @title Converts moles or molar concentration data from molar units to mass units
  #'
  #' @description This function is used to moles or molar concentration data from units of micromoles to milligrams.
  #'
  #' @usage umol.to.mg(x, mol)
  #'
  #' @param x Mass (or concentration) of the molecule, in micromoles, to be converted
  #' @param mol Character vector of length one indicating the molecule in question.
  #'
  #' @return Object same as 'x' but in units of milligrams
  #'
  #' @author Gordon W. Holtgrieve
  #'
  #' @export

umol.to.mg <- function(x, mol){
  # Error handling
  if(is.null(mol)) stop("Error: Missing argument 'mol'.")
  if(!is.character(mol)) stop("Error: Argument 'mol' must be of type character.")
  if(length(mol)>1) stop("Error: Argument 'mol' should be length == 1 (i.e., a single molecule.)")

   molar.mass <- get.molar.mass(mol = mol)

  out <- x / 1000 * molar.mass #Convert to mg (or mg L-1, for example)
  return(out)
}
