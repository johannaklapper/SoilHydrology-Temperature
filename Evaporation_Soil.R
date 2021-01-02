## Calculation of Evaporation from the Soil ##

# equation for evaporation #

  # Penman-Monteith potential evapotranspiration

      # Ep = (Δ(Rn – Gs) + ρcp(es – ea)/ra) / (λ(Δ+γ))

        # where:
  
          # Ep: potential evapotranspiration (kg m−2 s−1)
          # Δ: slope of the es to T curve = 4098 * (0.6108 * exp( 17.27 * T / (T + 237.3))) / (T + 237.3)^2 (kPa ºC-1) (T = Tsoil?; T in K?)
          # Rn: net radiation (J m-2 s-1) (from radiation model)
          # Gs: ground heat flux (J m-2 s-1) (from soil temperature model)
          # cp: specific heat of air (J kg-1)
          # ρ: the air density (kg m−3)
          # es: air saturation vapor pressure (kPa)
          # ea: air actual vapour pressure (kPa)
          # ra: aerodynamic resistance (s m-1) --> literature value?
          # γ: psycrometric constant (kPa ◦C−1) --> has to be calculated, but then we assume it to be constant
              # γ = (cp * p) / (λ * MWrat)
          # λ: latent heat of vaporization (J kg−1)


      # Actual soil evaporation (adapted from Aydin et. al 2005)

        # Ea = Ep * ( (log[WP] – log[WPa]) / (log[WPfc] – log[WPa]) ) / (Vsoil/BD)

          # WP = soil water potential
          # WPa = water potential of air ( -100MPa (average value))
          # WPfc = soil water potential at field capacity


# Input variables #

  # model data 
temp <-     # temperature of first soil layer [°C; to be converted to K]; from climate data or from 'Soil Temperature Model'
Rn <-       # from radiation group
Gs <-       # from soil temperature model
  
  # climate data
Rh <-       # relative humidity --> from climate data
  
  
# state variables
  
stat.var <- read.csv("State_Variables.csv", header = T, sep = ';')

theta.sat <- stat.var[1,1]    # saturated volumetric water content (m3 m-3)
psi.sat <- stat.var[1,2]      # saturated matric potential (m)
BD <- stat.var[1,4]           # bulk density
SD <- stat.var[1,6]           # soil sampling depth/depth of soil layer
p <- stat.var[1,10]           # air density (kg m-3)
psi.a <- stat.var[1,11]       # water potential of air (Pa)
psi.fc <-  stat.var[1,12]     # soil water potential at field capacity (Pa)
V <- 1 * 1 *SD                # volume of soil layer [m3]


# parameter

param <- read.csv("Parameter.csv", header = T, sep = ';')

cp <- param[1,2]           # specific heat of air (J kg-1)
lambda <- param[1,3]       # latent heat of vaporization (J kg-1)
MWrat <- param[1,4]        # ratio molecular weight of water vapor/dry air
b <- param[1,1]            # Exponent


# Converting temperature from °C to K
  
  # time <- seq(1, 52608)
  # temp.1.t <- rep(NA, length(time))

  # for (t in time) {
    # temp.1.t <- temp.1 + 274.15
  # }



# Calculating potential evaporation

delta <- 4098 * (0.6108*10^3 * exp( 17.27 * temp[t] / (temp[t] + 237.3))) / (temp[t] + 237.3)^2  # (Pa ºC-1) --> convert T in K beforhand?
es <- 0.6108 * exp(17.27*10^3 * temp[t] / (temp[t] + 237.3)) # (Pa)
ea <- es[t] * Rh # (Pa)
ra <-   # (s m-1)
gamma <- (cp * p) / (lamda * MWrat)

Ep <- (delta[t] * (Rn - Gs) + ρ * cp * (es[t] - ea[t]) / ra) / (lambda * (delta[t] + gamma)) # (kg m−2 s−1)


# Calculating soil water potential

psi <- psi_sat * (theta[t] / theta_sat)^-b   # (m)


# Calculating actual evaporation

evap <- Ep[t] * ( (log[psi[t]] - log[psi.a]) / (log[psi.fc] - log[psi.a]) ) / (V / BD)  # (m s-1)


