library("readxl")
library(writexl)
library(dplyr)
library(ggplot2)
library(rstatix)
library(reshape2)


########################################
##LO,GR,MilliQ and Salt water together##
########################################

zeta <- read_xlsx("OmarZeta.xlsx")

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

ggplot(stat_all_new, aes(x = factor(Microplastic,levels = c("None", "ACR","PE","PEEK","PS")), 
                         y = Mean, ymin = Mean - Standard_deviation,
                         ymax = Mean + Standard_deviation,  shape = Sample_type))+
  geom_errorbar(width = 0.1)+
  geom_point(size = 2.5, alpha = 0.70)+
  facet_wrap(~factor(Water, levels = c("Lake Ontario","Grand River",
                                       "Electrolyte solution","MilliQT")))+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  geom_hline(yintercept = 4, color = "blue", linetype = "dashed")+
  geom_hline(yintercept = -4, color = "blue", linetype = "dashed")+
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