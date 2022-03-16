

update.packages("ggplot2")
library(ggplot2)
library(egg)

Sys.setlocale("LC_TIME", "C")

setwd("C:/Users/b1045498/Nextcloud/01_Salzburg_new/03_Forschung/05_Applications/06_InfectiousDiseaseModelValidation/04_results/01_FinalResults/03_BAUvsSocialDistancing")

###read results Austria and visualize 
Austria_df_BAU_SD <- read.csv("02_Austria/BAU_SD_only_Austria.csv", header = TRUE, sep = ";")

###read results Austria and visualize 
Austria_df <- read.csv("02_Austria/BAUvsSocialAustria_Rformat.csv", header = TRUE, sep = ";")

Austria_df_BAU_SD$time <- as.Date(Austria_df_BAU_SD$time, "%d.%m.%Y")
Austria_df$time <- as.Date(Austria_df$time, "%d.%m.%Y")

p1 <- ggplot(Austria_df_BAU_SD, aes(x = time, y=fatalities, group =scenario)) +  
  geom_line(aes(linetype = scenario), size = 0.7) +
  scale_linetype_manual(values=c("dashed", "dotted"), name =  "Scenarios") +  
  ylab("Deaths per day") +
  xlab("Time (days)") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=6),
    axis.text.x=element_text(size=6),
    axis.title.y=element_text(size=7),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    #legend.text = element_text(size=7),
    #legend.title = element_text(size=8),
    legend.position = "none",
    panel.background = element_blank(),
    plot.title = element_text(size = 9),
    plot.subtitle = element_text(size = 6))+
  ggtitle("A")
  #labs(title = "Austria", subtitle = "2020-02-25 to 2021-03-23")

p2 <- ggplot(Austria_df, aes(time)) +
  #geom_ribbon(aes(ymin = relative.fatalities.r0.lower, ymax = relative.fatalities.r0.upper), fill = "grey90") + ##fadbd8  
  #geom_ribbon(aes(ymin = relative.fatalities.uve.lower, ymax = relative.fatalities.uve.upper), fill = "#d2b4de") +
  geom_line(aes(y = relative.fatalities), size = 0.6, colour = 'black') +
  ylab("Relative fatalities") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=6),
    axis.text.x=element_text(size=6),
    axis.title.y=element_text(size=7),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    plot.title = element_text(size = 9),
    panel.background = element_blank())+
  ggtitle("B")#+ 
  #annotate(geom="text", x=as.Date("2021-02-1"), y=0.5, label=expression("R"["0"]*"+5%"),     
  #         color="black", size = 2.5, hjust = 0)+ 
  #annotate(geom="text", x=as.Date("2021-02-1"), y=0.22, label=expression("R"["0"]*"-5%"),     
  #         color="black", size = 2.5, hjust = 0)

###read results Czechia and visualize 
Czechia_df_BAU_SD <- read.csv("03_Czechia/BAU_SD_only_Czechia.csv", header = TRUE, sep = ";")

###read results Czechia and visualize 
Czechia_df <- read.csv("03_Czechia/BAUvsSocialCzechia_Rformat.csv", header = TRUE, sep = ";")

Czechia_df_BAU_SD$time <- as.Date(Czechia_df_BAU_SD$time, "%d.%m.%Y")
Czechia_df$time <- as.Date(Czechia_df$time, "%d.%m.%Y")

p3 <- ggplot(Czechia_df_BAU_SD, aes(x = time, y=fatalities, group =scenario)) +  
  geom_line(aes(color = scenario), size = 0.5) +
  scale_color_manual(values=c("grey", "#69bd45", "#ed2024"), name =  "Scenarios") +  
  ylab("Deaths") +
  xlab("Time (days)") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=6),
    axis.text.x=element_text(size=6),
    axis.title.y=element_text(size=7),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    #legend.text = element_text(size=7),
    #legend.title = element_text(size=8),
    legend.position = "none",
    panel.background = element_blank(),
    plot.title = element_text(size = 9),
    plot.subtitle = element_text(size = 6))+
  labs(title = "Czechia", subtitle = "2020-01-13 to 2021-01-31")

p4 <- ggplot(Czechia_df, aes(time)) +
  geom_line(aes(y = relative.fatalities), size = 0.5, colour = 'black') +
  ylab("Relative fatalities") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=6),
    axis.text.x=element_text(size=6),
    axis.title.y=element_text(size=7),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    panel.background = element_blank())


###read results Hungary and visualize 
Hungary_df_BAU_SD <- read.csv("01_Hungary/BAU_SD_only_Hungary.csv", header = TRUE, sep = ";")

###read results Hungary and visualize 
Hungary_df <- read.csv("01_Hungary/BAUvsSocialHungary_Rformat.csv", header = TRUE, sep = ";")

Hungary_df_BAU_SD$time <- as.Date(Hungary_df_BAU_SD$time, "%d.%m.%Y")
Hungary_df$time <- as.Date(Hungary_df$time, "%d.%m.%Y")

p5 <- ggplot(Hungary_df_BAU_SD, aes(x = time, y=fatalities, group =scenario)) +  
  geom_line(aes(color = scenario), size = 0.5) +
  scale_color_manual(values=c("grey", "#69bd45", "#ed2024"), name =  "Scenarios") +  
  ylab("Deaths") +
  xlab("Time (days)") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=6),
    axis.text.x=element_text(size=6),
    axis.title.y=element_text(size=7),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    #legend.text = element_text(size=7),
    #legend.title = element_text(size=8),
    legend.position = "none",
    panel.background = element_blank(),
    plot.title = element_text(size = 9),
    plot.subtitle = element_text(size = 6))+
  labs(title = "Hungary", subtitle = "2020-01-13 to 2021-01-31")

p6 <- ggplot(Hungary_df, aes(time)) +
  geom_line(aes(y = relative.fatalities), size = 0.5, colour = 'black') +
  ylab("Relative fatalities") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=6),
    axis.text.x=element_text(size=6),
    axis.title.y=element_text(size=7),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    panel.background = element_blank())

###read results Slovenia and visualize 
Slovenia_df_BAU_SD <- read.csv("04_Slovenia/BAU_SD_only_Slovenia.csv", header = TRUE, sep = ";")

###read results Slovenia and visualize 
Slovenia_df <- read.csv("04_Slovenia/BAUvsSocialSlovenia_Rformat.csv", header = TRUE, sep = ";")

Slovenia_df_BAU_SD$time <- as.Date(Slovenia_df_BAU_SD$time, "%d.%m.%Y")
Slovenia_df$time <- as.Date(Slovenia_df$time, "%d.%m.%Y")

p7 <- ggplot(Slovenia_df_BAU_SD, aes(x = time, y=fatalities, group =scenario)) +  
  geom_line(aes(linetype = scenario), size = 0.7) +
  scale_linetype_manual(values=c("dashed", "dotted"), name =  "Scenarios") +   
  ylab("Deaths per day") +
  xlab("Time (days)") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=6),
    axis.text.x=element_text(size=6),
    axis.title.y=element_text(size=7),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    #legend.text = element_text(size=7),
    #legend.title = element_text(size=8),
    legend.position = "none",
    panel.background = element_blank(),
    plot.title = element_text(size = 9),
    plot.subtitle = element_text(size = 6))+
  ggtitle("C")
  #labs(title = "Slovenia", subtitle = "2020-01-13 to 2021-03-23")

p8 <- ggplot(Slovenia_df, aes(time)) +
  geom_line(aes(y = relative.fatalities), size = 0.5, colour = 'black') +
  ylab("Relative fatalities") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=6),
    axis.text.x=element_text(size=6),
    axis.title.y=element_text(size=7),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    plot.title = element_text(size = 9),
    panel.background = element_blank())+
  ggtitle("D")

#create a dummy plot, the common legend is extracted from this plot
dummy_p <- ggplot(Slovenia_df_BAU_SD, aes(x = time, y=fatalities, group =scenario)) +  
  geom_line(aes(linetype = scenario), size = 0.7) +
  scale_linetype_manual(values=c("dashed", "dotted"), name =  "Scenarios") + 
  ylab("Deaths") +
  xlab("Time (days)") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=6),
    axis.text.x=element_text(size=6),
    axis.title.y=element_text(size=7),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    legend.text = element_text(size=9),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.background = element_blank())

# Create user-defined function, which extracts legends from ggplots
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

# Apply user-defined function to extract legend
shared_legend <- extract_legend(dummy_p)


grid.arrange(arrangeGrob(p1, p2, p7, p8, ncol = 2),
             shared_legend, nrow = 2, heights = c(10, 1)) #, 

#ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, shared_legend, ncol = 2)