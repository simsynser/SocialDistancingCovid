update.packages("ggplot2")
library(ggplot2)
library(egg)
library(reshape)

Sys.setlocale("LC_TIME", "C")

setwd("C:/Users/b1045498/Nextcloud/01_Salzburg_new/03_Forschung/05_Applications/06_InfectiousDiseaseModelValidation/04_results/01_FinalResults/04_AlternativeMeasures")


#####Austria
FigA_all_df <- read.csv("Lifted1stLockdown/Austria/FigA_MobilityAll.csv", header = TRUE, sep = ";")
options(max.print=500)
FigA_all_df <- melt(FigA_all_df)
FigA_all_df$time <- as.Date(FigA_all_df$time, "%d.%m.%Y")

p1 <- ggplot(FigA_all_df, aes(x = time, y=value, group =variable)) +  
  geom_line(aes(color = variable), size = 0.1) +
  #scale_color_manual(values=c("grey", "black"), name =  "Scenarios") +  
  ylab("Mobility [% change from baseline]") +
  xlab("Time (days)") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=10),
    axis.text.x=element_text(size=10),
    axis.title.y=element_text(size=11),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    #legend.text = element_text(size=9),
    #legend.title = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  ggtitle("A")

FigB_all_df <- read.csv("Lifted1stLockdown/Austria/FigB_RT_All.csv", header = TRUE, sep = ";")
FigB_all_df <- melt(FigB_all_df)
FigB_all_df$time <- as.Date(FigB_all_df$time, "%d.%m.%Y")

p2 <- ggplot(FigB_all_df, aes(x = time, y=value, group =variable)) +  
  geom_line(aes(color = variable), size = 0.1) +
  #scale_color_manual(values=c("grey", "black"), name =  "Scenarios") +  
  ylab(expression("Effective reproduction R"["t"])) +                 
  xlab("Time (days)") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=10),
    axis.text.x=element_text(size=10),
    axis.title.y=element_text(size=11),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    #legend.text = element_text(size=9),
    #legend.title = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  ggtitle("B")

FigC_all_df <- read.csv("Lifted1stLockdown/Austria/FigC_Died_All.csv", header = TRUE, sep = ";")
FigC_all_df <- melt(FigC_all_df)
FigC_all_df$time <- as.Date(FigC_all_df$time, "%d.%m.%Y")


p3 <- ggplot(FigC_all_df, aes(x = time, y=value, group =variable)) +  
  geom_line(aes(color = variable), size = 0.1) +
  #scale_color_manual(values=c("grey", "black"), name =  "Scenarios") +  
  ylab("Cumulative deaths") +
  xlab("Time (days)") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=10),
    axis.text.x=element_text(size=10),
    axis.title.y=element_text(size=11),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    #legend.text = element_text(size=9),
    #legend.title = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  ggtitle("C")


#####Slovenia
FigD_all_df <- read.csv("Lifted1stLockdown/Slovenia/FigA_MobilityAll.csv", header = TRUE, sep = ";")
options(max.print=500)
FigD_all_df <- melt(FigD_all_df)
FigD_all_df$time <- as.Date(FigD_all_df$time, "%d.%m.%Y")

p4 <- ggplot(FigD_all_df, aes(x = time, y=value, group =variable)) +  
  geom_line(aes(color = variable), size = 0.1) +
  #scale_color_manual(values=c("grey", "black"), name =  "Scenarios") +  
  ylab("Mobility [% change from baseline]") +
  xlab("Time (days)") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=10),
    axis.text.x=element_text(size=10),
    axis.title.y=element_text(size=11),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    #legend.text = element_text(size=9),
    #legend.title = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  ggtitle("D")

FigE_all_df <- read.csv("Lifted1stLockdown/Slovenia/FigB_RT_All.csv", header = TRUE, sep = ";")
FigE_all_df <- melt(FigE_all_df)
FigE_all_df$time <- as.Date(FigE_all_df$time, "%d.%m.%Y")

p5 <- ggplot(FigE_all_df, aes(x = time, y=value, group =variable)) +  
  geom_line(aes(color = variable), size = 0.1) +
  #scale_color_manual(values=c("grey", "black"), name =  "Scenarios") +  
  ylab(expression("Effective reproduction R"["t"])) +                 
  xlab("Time (days)") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=10),
    axis.text.x=element_text(size=10),
    axis.title.y=element_text(size=11),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    #legend.text = element_text(size=9),
    #legend.title = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  ggtitle("E")

FigF_all_df <- read.csv("Lifted1stLockdown/Slovenia/FigC_Died_All.csv", header = TRUE, sep = ";")
FigF_all_df <- melt(FigF_all_df)
FigF_all_df$time <- as.Date(FigF_all_df$time, "%d.%m.%Y")


p6 <- ggplot(FigF_all_df, aes(x = time, y=value, group =variable)) +  
  geom_line(aes(color = variable), size = 0.1) +
  #scale_color_manual(values=c("grey", "black"), name =  "Scenarios") +  
  ylab("Cumulative deaths") +
  xlab("Time (days)") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=10),
    axis.text.x=element_text(size=10),
    axis.title.y=element_text(size=11),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    #legend.text = element_text(size=9),
    #legend.title = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  ggtitle("F")

grid.arrange(arrangeGrob(p1, p2, p3, p4, p5, p6,  ncol = 3))


