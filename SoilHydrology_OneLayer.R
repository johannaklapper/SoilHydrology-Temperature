## Modelling Soil Moisture with one soil layer ##

# time steps

time <- seq(1, x) # [h over data time period]

# input variables

read.csv("filename") # file with all the input variables
theta.in <-         # initial soil moisture [m3 m-3]; from climate data
prec <-              # precipitation [m m-3]; from climate data
evap <-              # evaporation [m m-3], from 'Evaporation.R'
trans <-             # transpiration [m m-3]; from Leaf Temperature Model
  
# state variables

stat_var <- read.table("Parameter_C.txt", header = T, sep = '\t')

theta.sat <- parameter[1,1]    # Volumetric water content at saturation
psi.sat <- parameter[1,2]      # Matric potential at saturation
K.sat <- parameter[1,4]         # Hydraulic conductivity at saturation

# parameters

# soil.layer <- 6  # number of soil layers [unitless]
# z.layer <- 0.1   # thickness of soil layers [m]
depth.layer <- 0.6 # depth of soil column [m]
sps <-             # soil pore space [kg m-3]

# output variables

theta.t <- rep(NA, length(time))    # soil moisture [m3 m-3]
drain.t <- rep(NA, length(time))    # drainage [m s-1]
runoff.t <- rep(NA, length(time))   # runoff [m s-1]
k.t <- rep(NA, length(time))        # hydraulic conductivity [m s-1]
inf.t <- rep(NA, length(time))      # infiltration [m s-1]
evap.t <- rep(NA, length(time))     # evaporation [m s-1]
  



# Iterative calculations over time

for(t in 1:length(time)) {
  if(t==1) {theta.t <- theta.in} else {theta.t <- theta.t[t-1]}
  prec.t <- prec[t]
  trans.t <- trans[t]
  evap.t <- evap[t]
  runoff.t <- theta.t - sps # not quite sure about the units, because runoff is a flux, but theta.t and sps are fixed (for each time step)
  inf.t <- prec.t - trans.t - evap.t - runoff.t
  # drain.t <-  # calculation in 'Drainage.R'
  k.t <- k.sat * (theta.t - theta.sat)^(2*b+3)
  theta.t <- theta.t + inf.t - drain.t
  if(theta.t > sps) theta.t <- sps
  if (runoff.t < 0) runoff.t <- 0
  theta[n] <- theta.t
  runoff[n] <- runoff.t
  evap[n] <- evap.t
  drain[n] <- drain.t
}
