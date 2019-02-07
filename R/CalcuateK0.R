#' @title Calculates Henery's constant (mol/L/atm)
#' @description Calculates Henry's constant for multiple gases at a given temperature, salinity and
#' pressure. Currently supported gases include CO2, CH4, O2, Ar, N2, and N2O.  Units are converted from
#' kg to volume (L) using water density given by the rho() function in seacarb.
#' @usage
#' @param temperature Water temperature in degrees Celsius.
#' @param gas
#' @param salinity Salinity in practical salinity units (PSU). Defaults to 0.
#' @param pressure Surface pressure in atmospheres. Defaults to 1.
#' @return Numeric vector of Henry's constant for a specified gas in units of mol/L/atm.
#' @note
#' @references
#' @note Units of mol/L/atm and umol/L/uatm are equivalent.
#' @author Gordon W. Holtgrieve (gholt@uw.edu)
#' @export

CalculateK0 <- function(temperature, gas, salinity = 0.25, pressure = 1){
  possibleGasses <- c("CO2", "CH4", "O2", "Ar", "N2", "N2O")

  #### Error Checking ####
  if(is.null(temperatureC)){
    stop("Error: 'temperatureC' is required data.")
  } else {
    temperatureK <- temperatureC + 273.15  #Convert Celsius to Kelvin
  }

  #Error check the 'gas' parameter
  if(is.null(gas)) stop("Error: Missing argument 'gas'.")
  if(!is.character(gas)) stop("Error: Argument 'gas' is must be of type character.")
  if(!gas %in% possibleGasses){
    stop("Error: The argument 'gas' must be one of the following: 'CO2', 'CH4', 'O2', 'Ar', 'N2', or 'N2O'.")
  } else {index <- which(possibleGasses == gas)
  }


  if (gas =="O2"){
    A <- c(5.80818, 3.20684, 4.11890, 4.93845, 1.01567, 1.41575)      #O2, umol/kg Garcia and Gordon. 1992. Limnol. Oceanogr. 37(6): 307-1312
    B <- c(-7.01211e-3, -7.25958e-3, -7.93334e-3, -5.54491e-3)
    C <- -1.32412e-7

    #Garcia and Gordon 1992 equation
    Ts <- log((298.15 - temperature) / (273.15 + temperature))
    lnConc <-  A[1] + A[2] * Ts + A[3] * Ts^2 + A[4] * Ts^3 + A[5] * Ts^4 + A[6] * Ts^5 + salinity * (B[1] + B[2] * Ts + B[3] * Ts^2 + B[4] * Ts^3) + C * salinity^2
    O2Sat_umolperkg <- exp(lnConc)

    rho <- seacarb::rho(S = salinity, T = temperature, P = 0) # Density in kg/m3
    O2Sat_umolperL <- O2Sat_umolperkg * rho  / 1000 # Convert from kg water to L

    return(O2Sat_umolperL)

  } else if(gas == "CO2"){




  }
}
