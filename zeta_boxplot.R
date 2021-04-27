library("readxl")
library(writexl)
library(dplyr)
library(ggplot2)
library(rstatix)
library(reshape2)


#######################################
zeta <- read_xlsx("OmarZeta.xlsx")

##Boxplots of microplastics initial zeta potentials in diluted jars##
zeta$Dose <- as.factor(zeta$Dose)

initial_zp <- zeta %>%
  filter(Water == "MilliQ" |
           Water == "Grand River" |
           Water == "Lake Ontario" |
           Water == "Salt water") %>%
  filter(Dose == "0" &
           Sample_type == "Dosed")
  
  ggplot(initial_zp, aes(x = Water, y = ZP))+
  geom_boxplot()+
  facet_wrap(~Microplastic)+
  theme_bw()+
  geom_hline(yintercept = 0, color = "Blue", linetype = "dotted")+
  coord_flip()+
  labs(y = "Zeta potential (mV)", title = "Zeta potentials before alum dosing (concentration = 500 #/mL)")+
    theme(axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text = element_text(size = 12))



##Zeta potentials of stock solutions##

zeta %>%
  filter(Sample_type == "Stock") %>%
  filter(Water != "Calgary water")%>%
  filter(Microplastic != "AcrylicG") %>%
  filter(Microplastic != "None") %>%
  ggplot(aes(x = Water, y = ZP))+
  geom_boxplot()+
  facet_wrap(~Microplastic)+
  theme_bw()+
  coord_flip()+
  geom_hline(yintercept = 0, color = "blue", linetype = "dotted")+
  labs(y = "Zeta potential (mV)", title = "Zeta potentials of microplastic stock solutions") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12))

####################################
##Zeta potential of the raw waters##
####################################
zeta_raw <- zeta %>%
  filter(Sample_type == "Raw")

  ggplot(zeta_raw, aes(x = Water, y = ZP))+
  geom_boxplot()+
    theme_bw()+
    #coord_flip()+
    geom_hline(yintercept = 0, color = "blue", linetype = "dotted")+
    ylim(-40,0)+
    labs(y = "Zeta potential (mV)", title = "Zeta potentials of raw water") +
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text = element_text(size = 12))
           

##Optimum dose determination for Grand River water##
zeta$Dose <- as.factor(zeta$Dose)

zeta %>%
  filter(Sample_type == "Dosed" &
         Water == "Grand River" &
         Microplastic == "PS") %>%
  ggplot(aes(x = Dose, y = ZP), color = Water)+
  geom_boxplot()+
  #facet_wrap(~Microplastic)+
  theme_bw()+
  #coord_flip()+
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed")+
  labs(x = "Dose (mg/L)",y = "Zeta potential (mV)", 
       title = "Optimum alum dose determination for Grand River water")+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12))

###############################################
##Optimum dose determination for Lake Ontario##
###############################################
zeta %>%
  filter(Sample_type == "Dosed" &
           Water == "Lake Ontario" &
           Microplastic == "PS") %>%
  ggplot(aes(x = Dose, y = ZP), color = Water)+
  geom_boxplot()+
  #facet_wrap(~Microplastic)+
  theme_bw()+
  #coord_flip()+
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed")+
  labs(x = "Dose (mg/L)",y = "Zeta potential (mV)", 
       title = "Optimum alum dose determination for Lake Ontario water")+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12))

##Comparing raw water zeta and dose 0 zetas

zeta %>%
  filter(Sample_type == "Raw" |
           Sample_type == "Dosed" &
           Dose == 0) %>%
  ggplot(aes(x = Water, y = ZP))+
  geom_boxplot()+
  facet_wrap(Microplastic~Sample_type)+
  theme_bw()+
  coord_flip()+
  geom_hline(yintercept = 0, color = "blue", linetype = "dotted")+
  labs(y = "Zeta potential (mV)", title = "Zeta potentials of raw water and seeded sample solutions")


##Calculating mean and std. deviations of zeta potential for 0 and optimimum dose (200 mg/L)
##GRAND RIVER##

Stats_GR <- zeta %>%
  filter(Water == "Grand River" &
             (Sample_type == "Dosed" |
             Sample_type == "Stock")) %>%
 group_by(Microplastic, Sample_type, Dose) %>%
 summarize(Mean = mean(ZP), Standard_deviation = sd(ZP))

Stats_GR <- Stats_GR[-c(13:25,27),]

write_xlsx(Stats_GR,"Stats_GR.xlsx")

ggplot(Stats_GR, aes(x = Microplastic, y = Mean, shape = Dose, color = Sample_type))+
  geom_point(size = 2.5)+
  #facet_wrap(~Sample_type)+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed")+
  labs(shape = "Alum dose (mg/L)",y = "Mean zeta potential (mV)", 
       title = "Mean zeta potentials of Grand River water samples")+
  theme(plot.title = element_text(size = 16),
       axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))

  



##Mean zetas of both Grand River and Lake Ontario in one##
Stats_GR_LO <- zeta %>%
  filter((Water == "Grand River" |
            Water == "Lake Ontario") &
           (Sample_type == "Dosed" |
              Sample_type == "Stock"|
              Sample_type == "Raw")) %>%
  group_by(Water,Microplastic, Sample_type, Dose) %>%
  summarize(Mean = mean(ZP), Standard_deviation = sd(ZP))

Stats_GR_LO <- Stats_GR_LO[-c(14:26,28,40,42,45:49),]

write_xlsx(Stats_GR_LO,"Stats_GR_LO.xlsx")

ggplot(Stats_GR_LO, aes(x = Microplastic, y = Mean, ymin = Mean - Standard_deviation,
                        ymax = Mean + Standard_deviation, shape = Dose, color = Sample_type))+
  geom_errorbar(width = 0.2)+
  geom_point(size = 2.5)+
  facet_wrap(~Water)+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed")+
  labs(shape = "Alum dose (mg/L)",y = "Mean zeta potential (mV)", 
       title = "Mean zeta potentials of Grand River and Lake Ontario water samples")+

  theme(plot.title = element_text(size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.position = "bottom")

########################################
##LO,GR,MilliQ and Salt water together##
########################################

Stats_GR_LO_MQ_SW <- zeta %>%
  filter((Water == "Grand River" |
            Water == "Lake Ontario" |
            Water == "MilliQ" |
            Water == "Salt water") &
           (Sample_type == "Dosed" |
            Sample_type == "Stock"|
            Sample_type == "Raw") &
            Microplastic != "AcrylicG" )   %>%
  
  group_by(Water,Microplastic, Sample_type, Dose) %>%
  summarize(Mean = mean(ZP), Standard_deviation = sd(ZP))

Stats_GR_LO_MQ_SW <- Stats_GR_LO_MQ_SW[-c(13:25,27,38,40,43:46,48,57,58,59,63,67,68,77,84,84),]

write_xlsx(Stats_GR_LO_MQ_SW,"Stats_all.xlsx")

stat_all_new <- read_xlsx("Stats_all_new.xlsx")

ggplot(stat_all_new, aes(x = factor(Microplastic,levels = c("None", "Acrylic","PE","PEEK","PS")), 
                                    y = Mean, ymin = Mean - Standard_deviation,
                        ymax = Mean + Standard_deviation,  shape = Sample_type))+
  geom_errorbar(width = 0.1)+
  geom_point(size = 2.5, alpha = 0.70)+
  facet_wrap(~Water)+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed")+
  labs(shape = "Sample type",y = "Mean zeta potential (mV)", x = "Microplastic")+ 
       #title = "Mean zeta potentials of Grand River, Lake Ontario, MilliQ and Saltwater samples")+
  
  theme(plot.title = element_text(size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.position = "right",
        strip.text.x = element_text(size = 14))+
  scale_shape_manual(values = 15:18)


########################################
##Mean zetas for MilliQ and Salt water##
########################################
Stats_MQ_SW <- zeta %>%
  filter((Water == "MilliQ" |
            Water == "Salt water") &
           (Sample_type == "Dosed" |
              Sample_type == "Stock")) %>%
  group_by(Water,Microplastic, Sample_type, Dose) %>%
  summarize(Mean = mean(ZP), Standard_deviation = sd(ZP))

Stats_MQ_SW <- Stats_MQ_SW[-c(4,6,7,11,15,16,20,28,29),]
write_xlsx(Stats_MQ_SW,"Stats_MQ_SW.xlsx")

ggplot(Stats_MQ_SW, aes(x = Microplastic, y = Mean, shape = Dose, color = Sample_type))+
  geom_point(size = 2.5)+
  facet_wrap(~Water)+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed")+
  labs(shape = "Alum dose (mg/L)",y = "Mean zeta potential (mV)", 
       title = "Mean zeta potentials of MilliQ and Salt water samples")+
  
  theme(plot.title = element_text(size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.position = "bottom")


##Calculating mean and std. deviations## Lake Ontario

Stats_LO <- zeta %>%
  filter(Water == "Lake Ontario" &
           (Sample_type == "Dosed" |
           Sample_type == "Stock")) %>%
  group_by(Microplastic, Sample_type, Dose) %>%
  summarize(Mean = mean(ZP), Standard_deviation = sd(ZP))

Stats_LO <- Stats_LO[-c(15:19,10,12),]

write_xlsx(Stats_LO,"Stats_LO.xlsx")

ggplot(Stats_LO, aes(x = Microplastic, y = Mean, shape = Dose))+
  geom_point()+
  facet_wrap(~Sample_type)+
  theme_bw()+
  #scale_color_brewer(palette = "Dark2")+
  geom_hline(yintercept = 0, color = "blue", linetype = "dotted")+
  labs(shape = "Alum dose (mg/L)",y = "Mean zeta potential (mV)", title = "Mean zeta potentials for Lake Ontario")


##########
##T-test##
##########

##Raw water zeta and particle addition zeta comparison## PEEK

compare <- zeta %>%
  filter(Sample_type == "Raw" |
           Dose == 0 &
           Microplastic == "PEEK")

compare <- compare[,-c(1,2,6,7,9:13)]

  compare <- compare %>%
    subset(Sample_type != "Stock")
  
Ttest_PEEK <- t.test(ZP ~ Sample_type, data = compare) 

Ttest_PEEK$p.value

##Raw water zeta and particle addition zeta comparison## PE MilliQ

compare <- zeta %>%
  filter(Sample_type == "Raw" |
           Dose == 0 &
           Microplastic == "PS")%>%
  filter(Water == "MilliQ")
  

compare <- compare[,-c(1,2,6,7,9:13)]

compare <- compare %>%
  subset(Sample_type != "Stock")

Ttest_PE <- t.test(ZP ~ Sample_type, data = compare) 

Ttest_PE$p.value

with(compare, shapiro.test(ZP[Sample_type == "Dosed"]))
var.test(ZP ~ Sample_type, data = compare)



###############################
##Adding a column of p values##
###############################

compare_ps <- zeta %>%
  filter(Sample_type == "Raw" |
           Dose == 0 &
           Microplastic == "PS") %>% 
  group_by(Water) %>%
  subset(Sample_type != "Stock") %>%
  subset(Water != "Calgary water") %>%
  mutate(p_value = t.test(ZP ~ Sample_type)$p.value)
  
compare_ps <- compare_ps[,-c(1,2,6,7,8,9:13)]

p_value_ps <- distinct(compare_ps)

##################################

compare_pe <- zeta %>%
  filter(Sample_type == "Raw" |
           Dose == 0 &
           Microplastic == "PE") %>% 
  group_by(Water) %>%
  subset(Sample_type != "Stock") %>%
  subset(Water != "Calgary water") %>%
  mutate(p_value = t.test(ZP ~ Sample_type)$p.value)

compare_pe <- compare_pe[,-c(1,2,6,7,8,9:13)]

p_value_pe <- distinct(compare_pe)

#################################

compare_peek <- zeta %>%
  filter(Sample_type == "Raw" |
           Dose == 0 &
           Microplastic == "PEEK") %>% 
  group_by(Water) %>%
  subset(Sample_type != "Stock") %>%
  subset(Water != "Calgary water") %>%
  mutate(p_value = t.test(ZP ~ Sample_type)$p.value)

compare_peek <- compare_peek[,-c(1,2,6,7,8,9:13)]

p_value_peek <- distinct(compare_peek)

#################################

compare_ac <- zeta %>%
  filter(Sample_type == "Raw" |
           Dose == 0 &
           Microplastic == "Acrylic") %>% 
  group_by(Water) %>%
  subset(Sample_type != "Stock") %>%
  subset(Water != "Calgary water") %>%
  mutate(p_value = t.test(ZP ~ Sample_type)$p.value)

compare_ac <- compare_ac[,-c(1,2,6,7,8,9:13)]

p_value_ac <- distinct(compare_ac)

p_values <- rbind(p_value_pe,p_value_peek,p_value_ps,p_value_ac)
p_values <- subset(p_values, Microplastic != "None")
p_values <- p_values[,-2]

write_xlsx(p_values,"p_values.xlsx")
