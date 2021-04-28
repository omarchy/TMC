# Scenario specification and transformations
# All dimensions are in µm if not specified
library(dplyr)
library(reshape2)
library(writexl)

MU_X <- rep(c(1, 10, 20, 50, 100, 150, 300, 500, 750), 5)
RSD_X <- c(rep(0, 9), rep(0.1, 9), rep(0.5, 9), rep(1, 9), rep(2, 9))
SIGMA_X <- RSD_X*MU_X
SIGMA_Y <- sqrt(log(1 + SIGMA_X^2/MU_X^2))
MU_Y <- log(MU_X) - 0.5*SIGMA_Y^2
DF1 <- data.frame(MU_X, SIGMA_X, RSD_X, MU_Y, SIGMA_Y)



# Develop data frame of 10,000 values from lognormal
SIZE <- matrix(nrow = 10000, ncol = 45)
for (i in 1:10000) {
  for (j in 1:45) {
    SIZE[i,j] <- qlnorm((i - 0.5)/10000, DF1$MU_Y[j], DF1$SIGMA_Y[j])
  }
}

#Defining functions for surface area and volume of shapes (sphere, long & short cylinders and oblate spheroids (e = 0.2 and 0.9))
SPa <- function(d){4*pi*(d/2)^2}
SPv <- function(d){(4/3)*pi*(d/2)^3}
LCa <- function(d){10.5*pi*d^2}
LCv <- function(d){pi*(d/2)^2*(10*d)}
SCa <- function(d){0.6*pi*d^2}
SCv <- function(d){pi*(d/2)^2*(0.1*d)}
OSa <- function(d,e){2*pi*(d/2)^2 + (pi/e)*log((1+e)/(1-e))*((d/2)*(1-e^2)^0.5)^2}
OSv <- function(d,e){(pi/6)*d^3*sqrt(1-e^2)}

#Creating vectors to add as column to the dataframe

Microplastics2 <- rep(c("PE","PP","PS","PET","PC","PVC","HDPE"),5)

Shape2 <- c(rep("Sphere",7),rep("Long Cylinder",7),
            rep("Short Cylinder",7),
            rep("Oblate Spheroid e=0.2",7),
            rep("Oblate Spheroid e=0.9",7))

particles.df <- data.frame(Microplastics2,Shape2)

# Creating empty matrix for total particles
total_particles_per_gram <- matrix(ncol = 45,nrow = 35)

# Densities of microplastics following the sequence as the Micropastics2 vector
mp_unit_volume <- c(1.16, 1.18, 0.93, 0.73, 0.81, 0.72, 1.03)

#Creating a row in the matrix
ppg <- matrix(c(1:45), nrow = 1)
particles_per_gram_all <-ppg

#Total surface area per gram of particles
tot_SA_per_gram <- matrix(c(1:45), nrow =1)

# Generating a matrix of values where each row represents total number of particles per 1/10000 th gram of microplastic
for (i in 0:4){
  for (j in 1:7){
    
    n = j + i*7
    
    if (particles.df$Shape2[n] == "Sphere"){
      
      s.area <- SPa(SIZE)
      
      volume <- SPv(SIZE)
                                                 
      
      
    } else if (particles.df$Shape2[n] == "Long Cylinder"){
      
      s.area <- LCa(SIZE)
      
      volume <- LCv(SIZE)
                                                 
      
      
    } else if (particles.df$Shape2[n] == "Short Cylinder"){
      
      s.area <- SCa(SIZE) 
      
      volume <- SCv(SIZE)
      

      
    } else if (particles.df$Shape2[n] == "Oblate Spheroid e=0.2"){
      
      s.area <- OSa(SIZE,0.2)
      
      volume <- OSv(SIZE,0.2)
      
      
    } else if (particles.df$Shape2[n] == "Oblate Spheroid e=0.9"){
      
      s.area <- OSa(SIZE,0.9)
      
      volume <- OSv(SIZE,0.9)
     

    }
    
    mass <- (1/mp_unit_volume[j])*volume*10^(-12)
    
    #Assuming that 10000 particles = 1 g, each value in the matrix below is the number
    #of particles of that size (depending on the column) in 1/10000 th of a gram
    #The sum of all the numbers in a given column will give the total number of particles
    #per gram
    particles <- (1/10000)/mass 
    
    # Total particles matrix by summing all the column values to resulting in a 1 by 45 matrix
    particles_per_gram <- matrix(colSums(particles),nrow = 1)
    particles_per_gram_all <- rbind(particles_per_gram_all, particles_per_gram)
    
    # Calculating the Total surface area of these particles matrix
    SA_particles <- s.area * particles
    SA_per_gram <- matrix(colSums(SA_particles), nrow = 1)
    tot_SA_per_gram <- rbind(tot_SA_per_gram, SA_per_gram)
  }
  
}

# Removing 1st row of dummy values (1:45)
particles_per_gram_all <- particles_per_gram_all[-1,]
tot_SA_per_gram <- tot_SA_per_gram[-1,]

# Arranging the total particles dataframe
total_particles_per_gram.df <- data.frame(particles.df, particles_per_gram_all)
total_particles_per_gram.df <- melt(total_particles_per_gram.df, id = c("Microplastics2","Shape2"))

# Arranging the total surface area dataframe
tot_SA_per_gram.df <- data.frame(particles.df, tot_SA_per_gram)
tot_SA_per_gram.df <- melt(tot_SA_per_gram.df, id = c("Microplastics2","Shape2"))

# Defining vectors for adding columns to the above dataframes
size_rsd <- rep(rep(c(1,10,20,50,100,150,300,500,750), each = 35),5)
rsd <- rep(c(0,0.1,0.5,1,2), each = 315)

total_particles_per_gram.df$variable <- size_rsd
total_particles_per_gram.df$RSD <- rsd

total_particles_per_gram.df <- total_particles_per_gram.df %>%
  rename(Microplastic = Microplastics2,
         Shape = Shape2,
         Size = variable,
         Particles_per_gram = value)

tot_SA_per_gram.df$variable <- size_rsd
tot_SA_per_gram.df$RSD <- rsd

tot_SA_per_gram.df <- tot_SA_per_gram.df %>%
  rename(Microplastic = Microplastics2,
         Shape = Shape2,
         Size = variable,
         Surface_area_per_gram = value)



# Converting units of the Surface_area_per_gram column from µm^2 to m^2
tot_SA_per_gram.df <- tot_SA_per_gram.df %>%
  mutate(Surface_area_per_gram = Surface_area_per_gram * 10^(-12))



a <- TMC_full[c(1:4,8:10)]
names(a)[7] <- "Size"
tot_SA_per_gram.df$Size <- as.factor(tot_SA_per_gram.df$Size)
c <- inner_join(a, tot_SA_per_gram.df)
total_particles_per_gram.df$Size <- as.factor(total_particles_per_gram.df$Size)
c <- inner_join(c,total_particles_per_gram.df)

c$Total_surface_area <- c$Total_surface_area*10^(-12)

TMC_num_integration <- c %>%
  mutate(TMC = (G/((AC_la/Total_surface_area)*Surface_area_per_gram))*Particles_per_gram)

write_xlsx(TMC_num_integration,"TMC_num_int.xlsx")



#Finding the mass based microplastics concentration

volume <- LCv(SIZE)
surface_area <- LCa(SIZE)
mass <- (1/mp_unit_volume[6])*volume*10^(-12)
particles <- (1/10000)/mass

mm <- (1/0.72)*10^(-12)*volume
tmm <- apply(mm,2,sum)
a <- SIZE[,45]
quantile(a, probs = (0.5))



#Discussion with Phil

MU_X <- rep(c(1, 10, 20, 50, 100, 150, 300, 500, 750), 5)
RSD_X <- c(rep(0, 9), rep(0.1, 9), rep(0.5, 9), rep(1, 9), rep(2, 9))
SIGMA_X <- RSD_X*MU_X
SIGMA_Y <- sqrt(log(1 + SIGMA_X^2/MU_X^2))
MU_Y <- log(MU_X) - 0.5*SIGMA_Y^2
DF1 <- data.frame(MU_X, SIGMA_X, RSD_X, MU_Y, SIGMA_Y)


SIZE <- matrix(nrow = 10000, ncol = 45)
for (i in 1:10000) {
  for (j in 1:45) {
    SIZE[i,j] <- qlnorm((i - 0.5)/10000, DF1$MU_Y[j], DF1$SIGMA_Y[j])
  }
}
s.area <- LCa(SIZE)

volume <- LCv(SIZE)
mass <- (1/mp_unit_volume[j])*volume*10^(-12)
particles <- (1/10000)/mass 