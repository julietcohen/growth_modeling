# Function to calculate forest carbon growth with and without carrying capacity

#' Parameters
#' @param r early exponential growth rate before canopy closure, default = 0.01
#' @param C size of forest, default = 10, units = kgC which is kilograms of carbon
#' @param g linear growth rate (once canopy closure has been reached), default = 2, units = kg/year
#' @param K carrying capacity, default = 250
#' @param c_growth canopy closure threshold, default = 50
#' @return c_list list of carbon outputs from function

# Function

C_growth <- function(time, C, parms, canopy_thresh =50) { #need time as a parameter because we are running the function specifically in an ODE solver
  if (C < canopy_thresh){
    c_list = parms$r*C
  }
  else {
    c_list = parms$g * (1 - C/parms$K)
  }
  return(list(c_list)) # the ODE solver needs the output to be a list
}
#do we need a time parameter? since we are putting this into an ODE?