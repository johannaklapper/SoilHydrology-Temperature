## Calculation of Evaporation from the Soil ##

# equation for evaporation #

  # E = ((h.s1*q.sat(T1))-q.ref)/(g.c)^-1+(g.ac)^-1

    # h.s1 = exp((g*M.w*psi.1)/(R*T.1))
    # g.c = p.m*exp(4.255*S.e-8.206)
        # with p.m = molar density [mol m-3]
        # with S.e = water content of the top soil layer relative to saturation

# Input variables #

input.data <- read.csv("Data_SoilHydrology_1.csv", header = T, sep = ";")
input.var <- input.data[-(1:2), ]
input.var$SoilTemperature_2 <- as.numeric(input.var$SoilTemperature_2)

psi.1 <-    # matric potential of first soil layer [m]; from 'Campbell_relationships.R'
temp.1 <- input.var$SoilTemperature_2  # temperature of first soil layer [°C; to be converted to K]; from climate data
g <- 9.80665             # gravitational acceleration [m s-2]
mol.h2o <- 18.02 * 1000  # molecular mass of water [kg mol-1]
R <- 8.31446             # universal gas constant [J K-1 mol-1]
  
g.c <-    # conductance
g.ac <-   # aerodynamic conductance for scalars between the surface and the reference height
q.sat.temp.1 <- 
q.ref <-              # q.sat.temp.1 - q.ref is the water vapor deficit between
                             # the evaporating surface, which is saturated
                             # with moisture at the temperature T1, and the
                             # surrounding air.

  
# Converting temperature from °C to K
  
time <- seq(1, 52608)
temp.1.t <- rep(NA, length(time))

for (t in time) {
  temp.1.t <- temp.1 + 274.15
}

# Calculate psi for a given theta

psi <- psi_sat * (theta / theta_sat)^-b

# Calculating evaporation

h.s1 <- rep(NA, length(time))
E <- rep(NA, length(time))

for (t in time) {
  h.s1 <- exp((g*mol.h2o*psi.1)/(R*temp.1.t))
  E <- (h.s1*q.sat.temp.1-q.ref)/((g.c)^-1+(g.ac)^-1)  
}

  
  
  
  
  
  
  
  