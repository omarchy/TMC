library(ggplot2)
library(readxl)
library(dplyr)
library(reshape2)

# WOrking with TMCs calculated using heterogenous distribution particles (numerical integration)

TMC_num_int <- read_excel("TMC_num_int.xlsx")


#Full TMC plot including all factors
ggplot(TMC_num_int, aes(x = as.numeric(Assigned_size), y = TMC, color = RSD))+
  geom_jitter(width = 70, alpha = 0.75)+
  scale_y_log10()


#Isolating Antimony, long cylinders and PVC
TMC_Sb_PVC_LC <- TMC_num_int %>%
  filter(Microplastic == "PVC" &
           Shape == "Long Cylinder" &
           Contaminant == "Antimony") %>%
  ggplot(aes(x=as.numeric(Assigned_size), y = TMC, color = as.factor(RSD), group = RSD))+
  geom_point()+
  #geom_line()+
  scale_y_log10()

# Working with TMCs calculated using homogenous distribution of particles

TMC_homogenous <- read_excel("TMC_full_homogenous.xlsx")
names(TMC_homogenous)[5] <- "Size"
TMC_homogenous1 <- TMC_homogenous[-12]

TMC_homogenous1 <- melt(TMC_homogenous1, id = c("AC_la","G", "Contaminant", "Shape",
                                              "Size", "Surface_area", "Volume","Microplastic",
                                              "Total_surface_area", "size_assigned"))

TMC_homogenous1 <- TMC_homogenous1 %>%
  rename(RSD = variable,
         TMC = value)

#Renaming the column attributes of RSD
TMC_homogenous1$RSD <- as.character(TMC_homogenous1$RSD)
rename.values(TMC_homogenous1$RSD, TMC_rsd_0 = "0")

TMC_homogenous1 <- TMC_homogenous1 %>%
  mutate(RSD = ifelse(as.character(RSD) == "TMC_rsd_0", "0",
                      ifelse(as.character(RSD) == "TMC_rsd_10", "0.1",
                             ifelse(as.character(RSD) == "TMC_rsd_50", "0.5",
                                    ifelse(as.character(RSD) == "TMC_rsd_100", "1",
                                           ifelse(as.character(RSD) == "TMC_rsd_200", "2",0))))))





TMC_curve_homogenous <- TMC_homogenous1 %>%
  filter(Contaminant == "Antimony" &
           Shape == "Long Cylinder" &
           Microplastic == "PVC") %>%
ggplot(aes(x=as.numeric(size_assigned), y = TMC, color = RSD))+
  geom_point(alpha = 0.6)+
  scale_y_log10()

colors <- c("TMC" = "green", "TMC_num_int$TMC" = "blue")
ggplot(TMC_homogenous1, aes(x = as.numeric(size_assigned)))+
  geom_jitter(aes(y = TMC, color = "TMC"), alpha = 0.50, width = 70)+
  geom_jitter(aes(y = TMC_num_int$TMC, color = "TMC_num_int$TMC"), alpha = 0.50, width = 70)+
  scale_y_log10()+

  labs(x = "Size (µm)", y = "Microplastics concentration (#/L)", color = "Legend")+
  scale_color_manual(values = colors)


#Preparing a dataset containing TMC from both dataset (PVC, LC, Antimony)

TMC_data_homogenous <- TMC_homogenous1 %>%
  filter(Contaminant == "Antimony" &
           Shape == "Long Cylinder" &
           Microplastic == "PVC")

TMC_data_heterogenous <- TMC_num_int %>%
  filter(Microplastic == "PVC" &
           Shape == "Long Cylinder" &
           Contaminant == "Antimony")

TMC_data_heterogenous <- TMC_data_heterogenous[c(7,9,11)]
names(TMC_data_heterogenous)[1] <- "Size"
names(TMC_data_heterogenous)[3] <- "TMC_heterogenous"
TMC_data_heterogenous$RSD <- as.factor(TMC_data_heterogenous$RSD)

TMC_data_homogenous <- TMC_data_homogenous[c(10:12)]
names(TMC_data_homogenous)[1] <- "Size"
names(TMC_data_homogenous)[3] <- "TMC_homogenous"
TMC_data_homogenous$RSD <- as.factor(TMC_data_homogenous$RSD)

Full_TMC <- full_join(TMC_data_homogenous,TMC_data_heterogenous)

Full_TMC <- melt(Full_TMC, id = c("Size", "RSD"))

Full_TMC <- Full_TMC %>%
  rename(Type = variable,
         TMC = value)

ggplot(Full_TMC, aes(x = as.numeric(Size), y = TMC, color = Type, shape = RSD), group = RSD)+
  geom_point()+
  geom_line()+
  scale_y_log10()+
  scale_x_continuous(breaks = c(0,100,200,300,400,500,600,700,800))+
  labs(x = "Size (µm)", y = "Microplastics concentration (#/L)")+
  ggtitle("TMCs based on heterogenous and homogenous particle distribution")
