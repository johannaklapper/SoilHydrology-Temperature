## Calculation of Hydraulic Conductivity of Soil ##

# Equation (based on Campbell relationships)

    # k = k.sat * (theta - theta.sat)^(2*b+3)


# input variables

parameter <- read.table("Parameter_C.txt", header = T, sep = '\t')

theta <- input.var[1,3]        # Volumetric water content from climate data?
theta.sat <- parameter[1,1]    # Volumetric water content at saturation
b <- parameter[1,3]            # Exponent
k.sat <- parameter[1,4]         # Hydraulic conductivity at saturation

# calculating hydraulic conductivity

time <- seq(1, 52608) # [0.5h over data time period]

for (t in time) {
 k.t <- k.sat * (theta - theta.sat)^(2*b+3) 
}
