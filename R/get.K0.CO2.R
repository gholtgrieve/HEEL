#' @title Calculates Henry's constant for CO2 (mol/L/atm)
#'
#' @description Calculates Henry's constant for CO2 at a given temperature, salinity and pressure.
#'
#' @param temperature Water temperature in degrees Celsius.
#'
#' @param salinity Salinity in practical salinity units (PSU). Defaults to 0.
#' @param pressure.atm Surface pressure in atmospheres. Defaults to 1.
#'
#' @return Numeric vector of Henry's constant for a specified gas in units of mol/L/atm.
#'
#' @note Units of mol/L/atm and umol/L/uatm are equivalent.
#'
#' @author Gordon W. Holtgrieve
#'
#' @export

get.K0.CO2 <- function(temperature=20, salinity = 0, pressure.atm = 1){

  #### Error Checking ####
  if(is.null(temperature)){
    stop("Error: 'temperature' is required data.")
  } else {
    temperatureK <- temperature + 273.15  #Convert Celsius to Kelvin
  }

    #Constants
    A <- c(-58.0931, 90.5069, 22.294)
    B <- c(0.027766, -0.025888, 0.0050578)
    R = 0.0820568    # Ideal gas constant with units of L atm mol-1 K
    vbar = 0.03023  # Not really sure what vbar is...

    lnK0 <- A[1] + A[2] * (100/temperatureK) + A[3] * (log(temperatureK/100))
    + salinity * (B[1] + (B[2] * (temperatureK/100)) + (B[3] * ((temperatureK/100)^2)))

    K0 <- exp(lnK0)  # Units of mol/L/atm, same as umol/L/uatm

    # KH=(K0*(exp((1-pressure)*(vbar)/(R*TK))))
    KH <- K0 * (exp((1-pressure.atm) * vbar / (R * temperatureK))) # mol/L/atm

    return(KH)
}
