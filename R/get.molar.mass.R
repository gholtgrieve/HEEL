#' Calculate average molar mass of molecules.
#'
#' Convenience function for obtaining the average molar mass of chemical compounds as found in nature.
#' @usage
#' get.molar.mass(mol)
#' @param mol Character vector of simple chemcial formulas.
#' @return Numeric vector of average molar masses same length as 'mol'.
#' @examples
#' get.molar.mass(mol="C2H6O")
#' get.molar.mass(mol=c("Ag(S2O3)2", "CO2"))
#' @author Gordon W. Holtgrieve
#' @importFrom CHNOSZ makeup
#' @export

get.molar.mass <- function(mol){
  # Error handling
  if(any(is.null(mol))) stop("Error: Missing argument 'mol'.")
  if(any(!is.character(mol))) stop("Error: Argument 'mol' must be of type character.")

  ave.molar.mass <- vector("numeric", length(mol))
  for(i in 1:length(mol)){
    elemental.breakdown <- CHNOSZ::makeup(mol[i]) #Borrows the makeup function from the CHNOSZ package.  Returns integer vector (count) with named elements
    temp <- data.frame(Symbol=names(elemental.breakdown), count=elemental.breakdown) #Make a 2 column data frame with symbol and count for the molcule
    merged <- merge(x=ave.element.mass, y=temp, by="Symbol") #Merge the temp data frame with the full ave.element.mass data frame.  Only matches will be kept.
    ave.molar.mass[i] <- sum(merged$averageMass * merged$count) #Sumproduct of elemental mass and count.
  }
  return(ave.molar.mass)

}
