dens <- c(0.97,1.23,0.86,1.37,0.85,1.08,1.39)
d_pvc <- 1.39
mp_type <- c("HDPE","PC","PE","PET","PP","PS","PVC")
names(dens) <- mp_type
d_c <- 1000                    #literature acquired mp size
d <- rlnorm(1000,mu,sigma)
G <- 0.004
R_c <- 27.8
SA_c <- 10.5*pi*(d_c)^2
SA <-  10.5*pi*(d)^2/(10^12)
TSA_c <- (10^12*(1/d_pvc)/((5/2)*pi*(d_c)^3)*SA_c)/10^12

TMC <- (G/R_c)*(TSA_c/SA)
TMC

#Not considering literature acquired particle size
mass <- 0
n = 0
while (mass < (1/27.8)*4){                      #4 = 1000*0.004 (considering 3log removal)
  d_750 <- rlnorm(1,mu,sigma)
  mass <- mass + (1.39*(5/2*pi*d_750^3)/10^12)  #long cylinders
  n <- n+1
  d_750
}



##Test code for TMC of LC PVC MPs wrt antimony
mean <- c(1,10,20,50,100,150,300,500,750)     #size of microplastics
slice <- (seq(0,89999)+0.5)/90000
stdev = 2*mean
variance <- sqrt(stdev)
sigma <- sqrt(log(1+variance/mean^2))
mu <- log(mean) -0.5*sigma^2


d <- rlnorm(90000,mu,sigma)                   #vector of 90000 values of size, 10000 of each

G <- 0.004
R_c <- 27.8
SA_c <- 10.5*pi*(d_c)^2
SA <-  10.5*pi*(d)^2/(10^12)
TSA_c <- (10^12*(1/d_pvc)/((5/2)*pi*(d_c)^3)*SA_c)/10^12

TMC <- (G/R_c)*(TSA_c/SA)


#naming columns
coln <- c(1,10,20,50,100,150,300,500,750)
rown<- c(1:10000)
TMC_matrix <- matrix(TMC,ncol = 9,byrow = TRUE, dimnames = list(rown,coln))
apply(TMC_matrix,2,max) #max value of each column


#Numerical integration to obtain TMC
#generating mass for 10000 particles of each size (1 to 750)
mass <- 1.39*(5/2*pi*d^3)/10^12

mass_matrix <- matrix(mass,ncol = 9, byrow = TRUE, dimnames = list(rown,coln))

#vector of total mass of all 10000 particles of each size
tot_mass <- apply(mass_matrix,2,sum)  
p <- (10000/tot_mass)*(4/27.8)

pv <- (10000/(apply((matrix(1.39*(5/2*pi*(rlnorm(90000,mu,sigma))^3)/10^12,ncol = 9, byrow = TRUE, dimnames = list(rown,coln))),2,sum)))*(4/27.8)

#iterating the calculation 50 times with the rlnorm to generate 50 values
library(magicfor)
magic_for(silent = TRUE)
for (n in 1:50){
  pv <- (10000/(apply((matrix(1.39*(5/2*pi*(rlnorm(90000,mu,sigma))^3)/10^12,ncol = 9, byrow = TRUE, dimnames = list(rown,coln))),2,sum)))*(4/27.8)
  put(pv)
}
a <- magic_result_as_vector()

#removing these values from the generated vector using setdiff
sl <- c(1:50)
remove <- c("1","10","20","50","100","150","300","500","750")
TMC_antimony_lc <- matrix(setdiff(a, remove),ncol = 9, byrow = TRUE, dimnames = list(sl,coln))

#Doing the same analysis based on surface area
#Total surface area of LC PVC of 1 µm mp = 3.02 m^2/g
d <- rlnorm(90000,mu,sigma) 
TSA_LC_PVC_1um <- 0.00302*10^6*10^6          #um^2/g, the target surface area
SA <- 10.5*pi*(d)^2                       #surface area of LC

#surface area of 90000 value as d is defined by rlnorm to produce 90000 values
SA_matrix <- matrix(SA,ncol = 9, byrow = TRUE, dimnames = list(rown,coln))
tot_SA <- apply(SA_matrix,2,sum)            #Sum of all values in a column

ps1 <- (10000/tot_SA)*((TSA_LC_PVC_1um/27.8)*0.004*1000)            #TMC by size

ps <- (90000/(sum(SA_matrix)))*((TSA_LC_PVC_1um/27.8)*0.004*1000)   #TMC based on average of 90000 values of all sizes

#Another approach to calculate TMC based on random particle sizes

mean1 <- 100     #size of microplastics
variance1 <- 5000
sigma1 <- sqrt(log(1 + variance1/mean1^2))
mu1 <- log(mean1) - 0.5*sigma^2



s_area <- 0
n = 0
while (s_area < ((TSA_LC_PVC_1um/27.8)*0.004*1000)){                      #4 = 1000*0.004 (considering 3log removal)
  d_random <- rlnorm(1,mu1,sigma1)
  s_area <- s_area + 10.5*pi*(d_random)^2  #long cylinders
  n <- n+1
  print(d_random)
  
}




for (i in c(0,0.1,0.5,1,2))
{
  mean <- c(1,10,20,50,100,150,300,500,750)     #size of microplastics
  stdev = i*mean
  variance <- (stdev)^2
  sigma <- sqrt(log(1+variance/mean^2))
  mu <- log(mean) -0.5*sigma^2
  
  d <- rlnorm(90000,mu,sigma)
  TSA_LC_PVC_1um <- 0.00302*10^6*10^6          #um^2/g, the target surface area
  SA <- 10.5*pi*(d)^2                          #surface area of LC
  
  #surface area of 90000 value as d is defined by rlnorm to produce 90000 values
  SA_matrix <- matrix(SA,ncol = 9, byrow = TRUE, dimnames = list(rown,coln))
  tot_SA <- apply(SA_matrix,2,sum)            #Sum of all values in a column
  
  ps1 <- (10000/tot_SA)*((TSA_LC_PVC_1um/27.8)*0.004*1000)
  print(i)
  print(variance)
  print(ps1)
  
}

library(readxl)
library(reshape2)
library(ggplot2)
library(dplyr)

TMC_var <- read_excel("O:/Me/R/TMC Matrix.xlsx", sheet = 2)
TMC_var_new <- melt(TMC_var, id.vars=c("rel_std_dev"))
TMC_var_new

TMC_var_new <- TMC_var_new %>%
  rename(Size = variable, TMC = value)

TMC_var_new$Size <- as.numeric(as.character(TMC_var_new$Size))

ggplot(TMC_var_new, aes(x = Size, y = TMC, group = rel_std_dev))+
  geom_point(aes(color = as.factor(rel_std_dev)), alpha = 0.60)+
  theme_bw()+
  
  scale_y_log10()+ xlab("Size (µm)")+ylab("TMC (#/L)")+
  scale_x_continuous(breaks=c(1,100,200,300,400,500,600,700))+
  labs(color = "Relative standard deviation")+
  theme(legend.position = "top")+
  ggtitle("TMC for LC PVC in the case of Antimony")