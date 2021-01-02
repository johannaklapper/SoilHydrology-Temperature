## Calculation of Drainage from the Soil ##

# Equation (as lower boundary condition of finite difference approximation)

  # drain.t = -(k / dz )*(psi - psi.n1) - k
        # k: hydraulic conductivity of soil at time t
        # dz: soil depth
        # psi: matric potential at time t
        # psi.n1: matric potential of soil beneath soil layer at time t

# input state variables

stat.var <- read.csv("State_variables.csv", header = T, sep = ';')

psi.sat <- stat.var[1,2]   # saturated hydraulic conductivity
theta.sat <- stat.var[1,1] # saturated volumetric water content
theta <-                   # calculated water content
b <- stat.var[1, 3]        # Exponent
clay <- stat.var[1,8]      # % of clay in the soil
SOC <- stat.var[1,6]       # soil organic carbon
BD <- stat.var[1,5]        # bulk density
SD <- stat.var[1,7]        # soil sampling depth

# input variables

k <-      # hydraulic conductivity of soil
dz <- 0.6 # soil layer thickness [m]
psi <-  # matric potential for soil
psi.n1 <- psi.sat * (s^-B)# matric potential for layer N+1 (layer beneath layer N) -> equation taken from CLM4.5
    s <- 0.5 ((theta.sat + theta)/theta.sat)
    B <- (1 - f) * B.min + f * B.om
        B.min <- 2.91 + 0.159 * clay
        B.om <- 2.7
        f <- (SOC / (BD * SD)) * 1.72 # soil organic matter fraction; f = %C * 1.72 
              # SOC(kg/ha) = SOC (%)× BD (g/cm3)× SD (cm) x 1000                
              # where,  SOC - Concentration of soil organic carbon (%);   BD - Bulk density (g/cm3); SD-   soil sampling depth (cm)

# calculating drainage

  
time <- seq(1, 52608) # [0.5h over data time period]

drain.t <- rep(NA, length(time))

for (t in time) {
  drain.t <- -(k / dz) * (psi.n - psi.n1) - k
}

# Creating functions

# psi.fun <- function(psi.sat, theta, theta.sat, b) {
#   psi <- psi.sat * (theta / theta.sat)^-b
#   return(psi)
# }
# 
# s.fun <- function(theta.sat, theta) {
#   s <- 0.5 * ((theta.sat + theta)/theta.sat)
#   return(s)
# }
# 
# B.min.fun <- function(clay) {
#   B.min <- 2.91 + 0.159 * clay
#   return(B.min)
# }
# 
# f.fun <- function(SOC, BD, SD) {
#   f <- (SOC / (BD * SD)) * 1.72
#   return(f)
# }
# 
# B.fun <- function(B.om) {
#   B <- (1 - f) * B.min + f * B.om
#   return(B)
# }
# 
# psi.n1.fun <- function(psi.sat) {
#   psi.n1 <- psi.sat * (s^-B)
#   return(psi.n1)
# }
# 
# drain.fun <- function(dz) {
#   drain <- -(k / dz) * (psi - psi.n1) - k
#   return(drain)
# }