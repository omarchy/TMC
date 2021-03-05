library(readxl)
TMC_matrix <- read_excel("O:/Me/R/TMC matrix.xlsx")
View(TMC_matrix)

# Changing wide table to long table
library(reshape2)
TMC_matrix_new <- melt(TMC_matrix, id.vars=c("Contaminant","Microplastic","Shape"))
TMC_matrix_new

# Changing column names
library(dplyr)
TMC_matrix_new <- TMC_matrix_new %>%
  rename(Size = variable, TMC = value)

# Changing data type to numeric from character
TMC_matrix_new$Size <- as.numeric(as.character(TMC_matrix_new$Size))
TMC_matrix_new$Size <- as.factor(as.numeric(TMC_matrix_new$Size))

library(ggplot2)


TMC_vs_size<- ggplot(TMC_matrix_new, aes(x=Size, y=TMC))+
  theme_minimal()+
  geom_jitter(width= 0.5, height = 0.5, size = 3,alpha= 0.60, aes(color = Contaminant, shape = Shape))+
  scale_color_brewer(palette="Paired")+
  scale_y_log10(breaks=c(100,10000,1000000,100000000,10000000000,1000000000000,100000000000000,10000000000000000), minor_breaks= minor_breaks)

TMC_vs_size + xlab('Size(µm)')+
  ylab('TMC (#/L)')+ #ggtitle('Threshold microplastics concentration curve')
  theme(axis.text=element_text(size = 10),
        axis.title=element_text(size = 12))
  

# Graph for PVC LC MPs
TMC_matrix_new$Size <- as.numeric(as.character(TMC_matrix_new$Size))

TMC_summary <- TMC_matrix_new %>%
  filter(Microplastic == "PVC", Shape == "LC")

ggplot(TMC_summary, aes(x=Size, y=TMC))+
  geom_line(size = 2,alpha=0.75, aes(color = Contaminant))+
  geom_point(size = 3, alpha = 0.60, aes(color = Contaminant))+
  scale_color_brewer(palette="Paired")+
  theme_bw()+
  scale_y_log10(breaks=c(100,10000,1000000,100000000,10000000000,1000000000000,100000000000000))+
  
  scale_x_continuous(breaks=c(1,100,200,300,400,500,600,700), limits=c(1,750))+
  geom_hline(yintercept = 3605, size = 1, color = "Brown", alpha = 0.6)+
  #geom_vline(xintercept = 100, size = 1, color = "Blue", alpha = 0.6)+
  ylab('TMC (particles/L)')+
  theme(legend.position="top")

# Grouped by contaminants
breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 42)*(10^rep(-20:20, each=9))
ggplot(TMC_summary, aes(x=Size, y=TMC, group = Contaminant))+
  geom_line(size = 1,alpha=0.75)+
  geom_point(size = 2.5, alpha = 0.60, aes(shape = Contaminant))+
  scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8,9,15,16))+
  scale_color_brewer(palette="Paired")+
  theme_bw()+
  scale_y_log10(breaks=c(100,1.3*10^3,10000,1.4*10^5,1000000,100000000,3.3*10^9,10000000000,1000000000000,100000000000000), minor_breaks = minor_breaks )+
  scale_x_continuous(breaks=c(1,100,200,300,400,500,600,700), limits=c(1,750))+
  geom_hline(yintercept = c(24, 187, 3605), size = 1, color = "Brown", alpha = 0.6)+
  geom_vline(xintercept = 100, size = 1, color = "Blue", alpha = 0.6)+
  ylab('TMC (particles/L)')+
  xlab('Size (µm)')+
  theme(legend.position="top")
  
