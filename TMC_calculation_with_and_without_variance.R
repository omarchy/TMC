V_m <- data.frame("Microplastics" = c("PE","PP", "PS","PET","PC","PVC","HDPE"),
                  "Volume ccpg" = c(1.16, 1.18, 0.93, 0.73, 0.81, 0.72, 1.03))
Contaminant <- data.frame("Contaminant"= c("Aluminum","Antimony","Arsenic","BPA","Bromine",
                                           "Cadmium","Chromium","Manganese","Mercury","Propanolol",
                                           "Sulfamethoxazole"),
                          "Adsorption capacity (mg/g)" = c(0.375,27.8,1.92,0.19,13,0.00014,0.000454,0.13,
                                                           0.00125,0.133,0.087))
#Unit volumes of mp (µm^3/g)
MPType <- (c(rep(1.16,55), rep(1.18,55), rep(0.93,55), rep(0.73,55), rep(0.81,55), rep(0.72,55), rep(1.03,55)))/10^6


Contaminant <- rep(c("Aluminum","Antimony","Arsenic","BPA","Bromine",
                "Cadmium","Chromium","Manganese","Mercury","Propanolol","Sulfamethoxazole"),5)

#Health guideline concentrations in mg/L of Contaminants as per sequence^
G <- rep(c(2.9,0.004,0.003,0.00006,0.01,0.005,0.03,0.12,0.002,0.0005,0.02),35)

#Literature acquired adsorption capacities of the contaminants^ in mg/g
AC_la <- rep(c(0.375,27.8,1.92,0.19,13,0.00014,0.000454,0.13,0.00125,0.133,0.087),35)

#Literature acquired sizes of microplastics in ?m
dia_la <- c(2000,1000,100,13.2,1000,3000,500,2000,4.5,45,45)

#Sizes chosen for analysis in ?m
dia <- c(1,10,20,50,100,150,300,500,750)

#Dataframe of all collected data of contaminants adsorbe onto microplastics
MPData <- data.frame(Contaminant,G,AC_la,dia_la)

#Defining functions for surface area and volume of shapes (sphere, cylinders and oblate spheroids)
SPa <- function(d){4*pi*(d/2)^2}
SPv <- function(d){(4/3)*pi*(d/2)^3}
LCa <- function(d){10.5*pi*d^2}
LCv <- function(d){pi*(d/2)^2*(10*d)}
SCa <- function(d){0.6*pi*d^2}
SCv <- function(d){pi*(d/2)^2*(0.1*d)}
OSa <- function(d,e){2*pi*(d/2)^2 + (pi/e)*log((1+e)/(1-e))*((d/2)*(1-e^2)^0.5)^2}
OSv <- function(d,e){(pi/6)*d^3*sqrt(1-e^2)}



##Generating surface areas and volumes of different shapes based on literature acquired mp size 

geometry <- matrix(ncol = 2, nrow = 55)
for (k in 0:4){
  for (i in 1:11){
    row = i + k*11
    for (j in 1){
      if (row <= 11){
    geometry[row,j] <- SPa(dia_la[i])
    geometry[row,j+1] <- SPv(dia_la[i])
    
      } else if (row > 11 & row <=22){
        geometry[row,j] <- LCa(dia_la[i])
        geometry[row,j+1] <- LCv(dia_la[i])
        
      } else if (row > 22 & row <= 33){
        geometry[row,j] <- SCa(dia_la[i])
        geometry[row,j+1] <- SCv(dia_la[i])
        
      } else if (row > 33 & row <= 44){
        geometry[row,j] <- OSa(dia_la[i], 0.2)
        geometry[row,j+1] <- OSv(dia_la[i], 0.2)
        
      } else if (row > 44 & row <= 55){
        geometry[row,j] <- OSa(dia_la[i], 0.9)
        geometry[row,j+1] <- OSv(dia_la[i], 0.9)
      }
    }
  }
}

geometry.df <- as.data.frame(geometry)

#Defining a vector of shape
Shape <- c(rep("Sphere",11),
           rep("Long Cylinder",11),
           rep("Short Cylinder",11),
           rep("Oblate Spheroid e=0.2",11),
           rep("Oblate Spheroid e=0.9",11))

colnames(geometry.df) <- c("Surface_area","Volume")
geometry.df <- cbind("Size (µm)" = rep(dia_la,5),geometry.df)
geometry.df <- cbind(Shape, geometry.df)
geometry.df <- cbind(Contaminant, geometry.df)


#Adding total surface area columns for microplastics of different types in micron^2/g
geometry.df$Tot_SA_PE <- ((1.16*10^12)/geometry.df$Volume)*geometry.df$Surface_area

library(dplyr)
geometry.df <- geometry.df %>%
  mutate(Tot_SA_PP = ((1.18*10^12)/Volume)*Surface_area) %>%
  mutate(Tot_SA_PS = ((0.93*10^12)/Volume)*Surface_area) %>%
  mutate(Tot_SA_PET = ((0.73*10^12)/Volume)*Surface_area) %>%
  mutate(Tot_SA_PC = ((0.81*10^12)/Volume)*Surface_area) %>%
  mutate(Tot_SA_PVC = ((0.72*10^12)/Volume)*Surface_area) %>%
  mutate(Tot_SA_HDPE = ((1.03*10^12)/Volume)*Surface_area)

#Converting some columns in the dataframe from wide to long

library(reshape2)
geometry.df <- melt(geometry.df, id = c("Contaminant","Shape","Size (µm)","Surface_area", "Volume"))     

geometry.df <- geometry.df %>%
  rename(MP_Type=variable ,
         Total_surface_area=value)

#Changing the names of MP_Type column attributes
Microplastics <- c(Tot_SA_PE = "PE", Tot_SA_PP = "PP", Tot_SA_PS = "PS",
                   Tot_SA_PET = "PET", Tot_SA_PC = "PC", Tot_SA_PVC = "PVC", Tot_SA_HDPE = "HDPE")

geometry.df$Microplastic <- as.character(Microplastics[geometry.df$MP_Type])

#Removing a column from the dataframe
geometry.df <- geometry.df[-c(6)]

#Reordering column positions
geometry.df <- geometry.df[c(1,2,3,4,5,7,6)]


#Adding the health guideline concentration and adsorption capacity column
geometry.df <- cbind(G, geometry.df)
geometry.df <- cbind(AC_la, geometry.df)


#Defining a matrix for calculating TMC assigned sizes (1 to 750 µm) without considering any log removal of particles
TMC_assigned_size <- matrix(ncol = 9, nrow = 385)
for (i in 1:385){
  for (j in 1:9){
    if (geometry.df$Shape[i] == "Sphere"){
      TMC_assigned_size[i,j] <- (geometry.df$G[i]/geometry.df$AC_la[i])*(geometry.df$Total_surface_area[i]/SPa(dia[j]))
      
    } else if (geometry.df$Shape[i] == "Long Cylinder"){
      TMC_assigned_size[i,j] <- (geometry.df$G[i]/geometry.df$AC_la[i])*(geometry.df$Total_surface_area[i]/LCa(dia[j]))
    
    } else if (geometry.df$Shape[i] == "Short Cylinder"){
      TMC_assigned_size[i,j] <- (geometry.df$G[i]/geometry.df$AC_la[i])*(geometry.df$Total_surface_area[i]/SCa(dia[j]))
      
    } else if (geometry.df$Shape[i] == "Oblate Spheroid e=0.2"){
      TMC_assigned_size[i,j] <- (geometry.df$G[i]/geometry.df$AC_la[i])*(geometry.df$Total_surface_area[i]/OSa(dia[j],0.2))
      
    } else if (geometry.df$Shape[i] == "Oblate Spheroid e=0.9"){
      TMC_assigned_size[i,j] <- (geometry.df$G[i]/geometry.df$AC_la[i])*(geometry.df$Total_surface_area[i]/OSa(dia[j],0.9))
      }
      
  }
}

geometry.df <- data.frame(geometry.df, TMC_assigned_size)

TMC_full <- geometry.df %>%
  rename("1"=X1, "10"=X2,"20"=X3, "50"=X4, "100"=X5,
         "150"=X6,"300"=X7,"500"=X8,"750"=X9)

TMC_full <- melt(TMC_full, id=c("AC_la","G","Contaminant","Shape","Size..µm.", "Surface_area",
                                "Volume","Microplastic","Total_surface_area"))
TMC_full <- TMC_full %>%
  rename(size_assigned = variable,
         TMC_rsd_0 = value)

#Creating a void matrix to store TMCs calculated using variances
TMC_var <- matrix(nrow = 3465, ncol = 5)

#Calculating TMCs based on 5 different variances and storing them in the matrix/Numerical integration
for (k in 0:8){
  for (i in 1:385){
    n = i + k*385
    
    if (TMC_full$Shape[n] == "Sphere"){
      SA <- SPa(SIZE)
      
      
    } else if (TMC_full$Shape[n] == "Long Cylinder"){
      SA <- LCa(SIZE)
      
      
    } else if (TMC_full$Shape[n] == "Short Cylinder"){
      SA <- SCa(SIZE)
      
      
    } else if (TMC_full$Shape[n] == "Oblate Spheroid e=0.2"){
      SA <- OSa(SIZE, 0.2)
      
      
    } else if (TMC_full$Shape[n] == "Oblate Spheroid e=0.9"){
      SA <- OSa(SIZE,0.9)
     
    }
    TSA <- apply(SA,2,sum)
    TSA <- matrix(TSA, ncol = 9, byrow = TRUE)
    for (j in 1:5){
  
      TMC_var[n,j] <- (10000/TSA[j,k+1])*
        ((TMC_full$Total_surface_area[n]/TMC_full$AC_la[n])*TMC_full$G[n])
    }
  }
}

#Adding the new matrix to TMC_full dataframe and renaming columns
TMC_full <- data.frame(TMC_full,TMC_var)
TMC_full <- TMC_full %>%
  rename(TMC_rsd_0n = X1,
         TMC_rsd_10 = X2,
         TMC_rsd_50 = X3,
         TMC_rsd_100 = X4,
         TMC_rsd_200 = X5)

TMC_full <- TMC_full[c(3,2,1,4:16)]
write.csv(TMC_full, "O:\\Me\\R\\TMC_dataset.csv", row.names = FALSE)
