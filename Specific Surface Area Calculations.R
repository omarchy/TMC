################################################################################
#                                                                              #
# Specific surface area analysis for two interpretations of spherical particle #
# size distribution                                                            #
# 1) 10,000 particles of varying size                                          #
# 2) 10,000 g, each made up of particles of specific size                      #
#                                                                              #
# Completed to check Omar Chowdhury's microplastics TMC analysis               #
#                                                                              #
# Prepared by P. Schmidt, 12-Feb-2021                                          #
#                                                                              #
################################################################################

### Define lognormal particle size distribution
MU_X <- 100     # average particle diameter (micron)
RSD_X <- 1      # relative standard deviation of particle diameters (micron)
SIGMA_X <- RSD_X * MU_X   # standard deviation of particle diameters (micron)
SIGMA_Y <- sqrt(log(1 + SIGMA_X^2 / MU_X^2))    # stdev of ln(X)
MU_Y <- log(MU_X) - 0.5 * SIGMA_Y^2             # average of ln(X)

### Generate 10,000 diameters, each with equal probability
D <- qlnorm(p = (seq(1:10000)-0.5)/10000, meanlog = MU_Y, sdlog = SIGMA_Y)

### Convert to volume, mass, and surface area per spherical particle
VP <- 4/3 * pi * (D/2)^3  # Particle volume in cubic microns
Density <- 10^(-12)       # Density in grams per cubic micron (water = 10^-12)
MP <- VP * Density        # Particle mass in grams
SAP <- 4 * pi * (D/2)^2   # Surace area per particle in square microns

### Calculate specific surface area per gram of particles
### 1) Assume particle size distribution describes individual particles
SUM_MASS <- sum(MP)         # Mass of 10,000 particles
SUM_SA <- sum(SAP)          # Surface area of 10,000 particles
SSA1 <- sum(SAP) / sum(MP)  # Specific surface area (square microns/gram)
### 2) Assume particle size distribution describes mass fractions
MF <- rep(1 / 10000, 10000) # 10,000 mass fractions of 0.0001 grams each
NUM <- MF / MP              # Number of paricles of size to reach 0.0001 g
SAF <- NUM * SAP            # Total surface area of mass fraction
SSA2 <- sum(SAF)            # Specific surface area (square microns/gram)