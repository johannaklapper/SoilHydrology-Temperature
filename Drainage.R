## Calculation of Drainage from the Soil ##

# Equation (as lower boundary condition of finite difference approximation)

  # drain.t = -(k.t.n0.5 / dz.n0.5 )*(psi.t.n - psi.t.n1) - k.t.n0.5
        # k.t.n0.5: hydraulic conductivity of layer N+1/2 at time t+1
        # dz.n0.5: layer thickness
        # psi.t.n: matric potential of layer N at time t+1
        # psi.t.n1: matric potential of layer N+1 at time t+1

# input variables

k.0.5 <-   # hydraulic conductivity of layer N+0.5
dz <- 0.1 # soil layer thickness [m]
psi.n <-  # matric potential for layer N (last layer)
psi.n1 <- # matric potential for layer N+1 (layer above layer N)

# calculating drainage

  
time <- seq(1, 52608) # [0.5h over data time period]

drain.t <- rep(NA, length(time))

for (t in time) {
  drain.t <- -(k.0.5 / dz) * (psi.n - psi.n1) - k.0.5
}