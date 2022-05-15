# Function to calculate forest carbon growth with and without carrying capacity

#' Parameters
#' @param r early exponential growth rate before canopy closure, default = 0.01
#' @param C size of forest, default = 10, units = kgC which is kilograms of carbon
#' @param g linear growth rate (once canopy closure has been reached), default = 2, units = kg/year
#' @param K carrying capacity, default = 250
#' @param c_growth canopy closure threshold, default = 50
#' @return c_list list of carbon outputs from function

# Function

C_growth <- function(r = 0.01, C = 10, g = 2, K = 250, canopy_thresh = 50) {
  if (C < canopy_thresh){
    c_list = r*C
  }
  else {
    c_list = g * (1 - C/K)
  }
  return(list(c_list)) # the ODE solver needs the output to be a list
}