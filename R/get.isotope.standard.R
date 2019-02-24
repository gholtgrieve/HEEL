#' Helper function to access information in the isotope.standards dataframe
#'
#' This function is used to pull out isotope standard data from the isotope.standards dataframe
#' easily and efficently.  The isotope.standards dataframe contains only raw isotope ratios,
#' so this function also will convert to delta value relative to the common international standard.
#'
#' @usage
#' @param std  Character vector with name(s) of the isotope standard of interest.
#' @param isotope.system  Character vector of length one indicating the (single) isotope system of interest. Currently supported isotope systems are "C", "N", "S", "H", "O18", "O17".
#' @return A list with the following elements.
#' \describe{
#'   \item{delta}{Numeric value of the standard in delta notation for the given isotope system. Uses the most common international standard for each isotope system. Specifically, VPDB for C, air for N, VCDT for S, and VSMOW for H, O18, and O17.}
#'   \item{std.R}{The ratio of heavy to light isotope of the standard for the given isotope system (e.g., 13C/12C)}
#'   \item{R}{The ratio of the standard relative to ratio of the common international standard for the given isotope system (i.e., R_std/R_VPDB)}
#'   \item{massPct}{The mass percent of the standard for the given isotope system.}
#'   }
#' @author Gordon W. Holtgrieve
#' @export

get.isotope.standard <- function(std, isotope.system){

 switch(isotope.system,
        C = {
          dataType_R <- "R_13C12C"
          dataType_mass <- "massPctC"
          reference.std.R <- 0.01118000  #Value for VPDB
        },
        N = {
          dataType_R <- "R_15N14N"
          dataType_mass <- "massPctN"
          reference.std.R <-0.00367600 #Value for air
        },
        S = {
          dataType_R <- "R_34S32S"
          dataType_mass <- "massPctS"
          reference.std.R <- 0.04416260  #Value for VCDT
        },
        H = {
          dataType_R <- "R_DH"
          dataType_mass <- "massPctH"
          reference.std.R <- 0.00015576  #Value for VSMOW
        },
        O18 = {
          dataType_R <- "R_18O16O"
          dataType_mass <- "massPctO"
          reference.std.R <- 0.00200520  #Value for VSMOW
        },
        O17 = {
          dataType_R <- "R_17O16O"
          dataType_mass <- "massPctO"
          reference.std.R <- 0.00037990  #Value for VSMOW
        },
        stop("Parameter isotope.system is missing or not one of the currently supported systems.")
        )
  std.R <- isotope.standards$Value[isotope.standards$Name %in% std & isotope.standards$dataType == dataType_R]
  if(length(std.R)==0)  stop("No data available.  Try again.")

  R <- std.R/reference.std.R
  delta <- (R - 1) * 1000
  temp <- isotope.standards$Value[isotope.standards$Name %in% std & isotope.standards$dataType == dataType_mass]
  if(length(temp)==0){
    massPct <- NA
  } else {
    massPct = temp
  }

 return(list(std = std, isotope.system = isotope.system, delta = delta, std.R = std.R, R = R, massPct = massPct))
}
