## Modelling Soil Moisture with one soil layer ##


library(dplyr)


# time steps

time <- seq(1, 365*24) # [h over data time period (Year 2017)]


# state variables

stat.var <- read.csv("State_Variables.csv", header = T, sep = ';')

theta.sat <- stat.var[1,1]    # Volumetric water content at saturation
psi.sat <- stat.var[1,2]      # Matric potential at saturation
k.sat <- stat.var[1,3]        # Hydraulic conductivity at saturation
BD <- stat.var[1,4]           # bulk density
SOC <- stat.var[1,5]          # soil organic carbon
SD <- stat.var[1,6]           # soil sampling depth/depth of soil layer
clay <- stat.var[1,7]         # % of clay in the soil
PD <- stat.var[1,8]           # particle density (from literature)
B.om <- stat.var[1,9]         # given by Lett et al. 2000 (in CLM4.5)
p <- stat.var[1,10]           # air density (kg m-3)
psi.a <- stat.var[1,11]       # water potential of air (Pa)
psi.fc <-  stat.var[1,12]     # soil water potential at field capacity (Pa)
sps <- 1 - (BD/PD)            # soil pore space [unitless]: bulk density = 1200 kg m-3 (climate data), particle density = 2650 kg m-3 (literature)
V <- 1 * 1 *SD                # volume of soil layer [m3]


# parameters

param <- read.csv("Parameter.csv", header = T, sep = ';')

b <- param[1,1]            # Exponent
cp <- param[1,2]           # specific heat of air (J kg-1)
lambda <- param[1,3]       # latent heat of vaporization (J kg-1)
MWrat <- param[1,4]        # ratio molecular weight of water vapor/dry air


# input data

input.data <- read.csv("Data_SoilHydrology_1.csv", header = T, sep = ";")                     # file with all the input variables
input <- input.data[-(1:2), ]                                                                 # remove first two rows (units and blank)
input$Date.Time <- as.Date(input$Date.Time, format = "%d.%m.%Y")                              # first column as Date
input.2017 <- filter(input, input$Date.Time > "2016-12-31" & input$Date.Time < "2018-01-01")  # only data from 2017
input.2017[ , 2:11] <-lapply(input.2017[ , 2:11], as.numeric)                                 # change class to numeric


# input variables

  # theta (volumetric water content)
theta.in <- mean(x = c(38.130, 33.84, 47.09))   # initial soil moisture [m3 m-3]; from climate data (mean of first row)

  # evaporation from soil
# evap <-              # evaporation, from 'Evaporation.R'
  
  # transpiration
trans <-             # transpiration; from Leaf Temperature Model
  
  # drainage
  
    # drain.t = -(k / SD )*(psi - psi.n1) - k
      # k: hydraulic conductivity 
      # SD: soil layer thickness
      # psi: matric potential of soil 
      # psi.n1: matric potential of soil beneath soil layer
            # psi.n1 <- psi.sat * (s^-B) (equation from CLM4.5)
              # s <- 0.5 ((theta.sat + theta)/theta.sat)
              # B <- (1 - f) * B.min + f * B.om
                # B.min <- 2.91 + 0.159 * clay
                # B.om <- 2.7
                # f <- (SOC / (BD * SD)) * 1.72 # soil organic matter fraction; f = %C * 1.72 
                    # SOC(kg/ha) = SOC (%)× BD (g/cm3)× SD (cm) x 1000                
                    # where,  SOC - Concentration of soil organic carbon (%);   BD - Bulk density (g/cm3); SD-   soil sampling depth (cm)
  

  # precipitation
prec.NA <- rep(NA, length(time))        # precipitation [m h-1]; from climate data
  time.2 <- seq(1, 365*24*2)           # every half hour
  for (t in time.2) {                   
    prec.30 <-  input.2017[ , 2]
    if (t%%2 != 0) {prec.t <- prec.30[t] + prec.30[t+1]} else next
    prec.NA[t] <- prec.t
  }
prec <- na.omit(prec.NA) # remove NA, because now every second row (every even time step) is NA


# output variables

theta <- rep(NA, length(time))    # soil moisture [m3 m-3]
drain <- rep(NA, length(time))    # drainage [m h-1]
runoff <- rep(NA, length(time))   # runoff [m h-1]
k <- rep(NA, length(time))        # hydraulic conductivity [m h-1]
inf <- rep(NA, length(time))      # infiltration [m h-1]
evap <- rep(NA, length(time))     # evaporation [m h-1]
  



# Iterative calculations over time

for(t in time) {
  
  # first water content is taken from climate data, then theta from previous time step is taken for calculation
  # theta.in is the mean water content on 01.01.2017 00.00-00.30 over all 'layers'
  if(t==1) {theta.t <- theta.in} else {theta.t <- theta[t-1]}
  if(theta.t > sps) {theta.t <- sps} else {theta.t <- theta[t-1]}
  
  # precipitation is taken from climate data; every two half-hour values are added for a precipitation value for one hour
  prec.t <- prec[t]
  
  # transpiration data is taken from Leaf Temperature Model
  # trans.t <- trans[t]
  
  # evaporation still needs to be done
    # Calculating potential evaporation
  delta <- 4098 * (0.6108*10^3 * exp( 17.27 * temp[t] / (temp[t] + 237.3))) / (temp[t] + 237.3)^2  # (Pa ºC-1) --> convert T in K beforehand?
  es <- 0.6108 * exp(17.27*10^3 * temp[t] / (temp[t] + 237.3)) # (Pa)
  ea <- es[t] * Rh # (Pa)
  ra <-   # (s m-1)
    gamma <- (cp * p) / (lamda * MWrat)
  Ep <- (delta[t] * (Rn - Gs) + ρ * cp * (es[t] - ea[t]) / ra) / (lambda * (delta[t] + gamma)) # (kg m−2 s−1)
  
    # Calculating soil water potential
  psi <- psi_sat * (theta[t] / theta_sat)^-b   # (m)
  
    # Calculating actual evaporation
  evap.t <- Ep[t] * ( (log[psi[t]] - log[psi.a]) / (log[psi.fc] - log[psi.a]) ) / (V / BD)  # (m s-1)
  
  # runoff is the excess water; if runoff is negative, no runoff occurs
  runoff.t <- (theta.t - sps) * V 
  if (runoff.t < 0) {runoff.t <- 0} else {runoff.t == runoff.t}
  
  # infiltration (without infiltration capacity -> if there's space, water will infiltrate)
  inf.t <- prec.t - runoff.t - trans.t - evap.t
  
  # hydraulic conductivity
  k.t <- k.sat * (theta.t - theta.sat)^(2*b+3)
  
  # drainage: has to be calculated
    # Calculating psi for given theta
  psi <- psi_sat * (theta[t] / theta_sat)^-b   # (m); matric potential for soil
  
    # Calculating psi for soil beneath soil layer
  s <- 0.5 ((theta.sat + theta[t])/theta.sat)
  B.min <- 2.91 + 0.159 * clay
  B.om <- 2.7
  f <- (SOC / (BD * SD)) * 1.72 # soil organic matter fraction; f = %C * 1.72 
  B <- (1 - f) * B.min + f * B.om
  psi.n1 <- psi.sat * (s[t]^-B)  # matric potential for layer N+1 (layer beneath layer N) -> equation taken from CLM4.5
  
    # Calculating drainage
  drain.t <- -(k / SD) * (psi[t] - psi.n1[t]) - k
  
  # theta (water content) is current water content plus infiltration minus drainage
  theta.t <- theta.t + inf.t - drain.t 
 
  theta[t] <- theta.t
  runoff[t] <- runoff.t
  k[t] <- k.t
  evap[t] <- evap.t
  drain[t] <- drain.t
}
