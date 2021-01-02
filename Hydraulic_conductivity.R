## Calculation of Hydraulic Conductivity of Soil ##

# Equation (based on Campbell relationships)

    # k = k.sat * (theta - theta.sat)^(2*b+3)

# input variables

theta <-         # Volumetric water content from model

# state variables

stat.var <- read.csv("State_Variables.csv", header = T, sep = ';')

theta.sat <- stat.var[1,1]    # saturated volumetric water content (m3 m-3)
k.sat <- stat.var[1,3]        # hydraulic conductivity at saturation


# parameter

param <- read.csv("Parameter.csv", header = T, sep = ';')

b <- param[1,1]       # Exponent


# calculating hydraulic conductivity

time <- seq(1, 52608) # [0.5h over data time period]

for (t in time) {
 k.t <- k.sat * (theta[t] - theta.sat)^(2*b+3) 
}
