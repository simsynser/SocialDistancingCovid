
update.packages("ggplot2")
library(ggplot2)
library(egg)
library(gridExtra)
#remove.packages("ggpubr")
#library(ggpubr)
#library(ggrepel)

setwd("C:/Users/b1045498/Nextcloud/01_Salzburg_new/03_Forschung/05_Applications/06_InfectiousDiseaseModelValidation/04_results/01_FinalResults/02_ModelFit")

###Deaths per day data and model
Austria_df <- read.csv("ModelFitAustria/FitAustria.csv", header = TRUE, sep = ";")
Austria_df$timeMonth <- as.Date(Austria_df$timeMonth, "%Y-%m-%d")

Slovenia_df <- read.csv("ModelFitSlovenia/FitSlovenia.csv", header = TRUE, sep = ";")
Slovenia_df$timeMonth <- as.Date(Slovenia_df$timeMonth, "%d.%m.%Y")

p1 <- ggplot(Austria_df, aes(timeMonth)) +
  geom_line(aes(y = d..data.), size = 0.1, colour = '#0001ff64') +
  geom_line(aes(y = death), size = 0.5 , colour = '#0001ffff') +
  geom_line(data = Slovenia_df, aes(x = timeMonth, y = d..data.), size = 0.1, colour = '#ff010064') +
  geom_line(data = Slovenia_df, aes(x = timeMonth, y = deaths.overall), size = 0.5, colour = '#ff0100ff') +
  ylab("Deaths per day") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=6),
    axis.text.x=element_text(size=6),
    axis.title.y=element_text(size=7),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    panel.background = element_blank(),
    plot.title = element_text(size = 9))+
  ggtitle("A")+ 
  annotate(geom="text", x=as.Date("2020-02-25"), y=80, label=expression("R0"["model"]*"=2.2"),     
                               color="#0001ffff", size = 3, hjust = 0)+
  annotate(geom="text", x=as.Date("2020-02-25"), y=65, label=expression("R0"["model"]*"=2.6"),  #"R0 model = 1.98\nR0 data = 2.87"
           color="#ff0100ff", size = 3, hjust = 0)




#p1 <- p1 + geom_line(Slovenia_df, aes(x = timeMonth, y = d..data.), size = 0.1, colour = 'grey')
#p1 <- p1 + geom_line(Slovenia_df$deaths.overall, size = 0.5 , colour = 'black')

p2 <- ggplot(Austria_df, aes(timeMonth)) +
  geom_line(aes(y = dsum..data.), size = 1.5, colour = 'grey70') +
  geom_line(aes(y = died.overall), size = 0.5 , colour = 'black') +
  ylab("Cumulative deaths") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=6),
    axis.text.x=element_text(size=6),
    axis.title.y=element_text(size=7),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    panel.background = element_blank(),
    plot.title = element_text(size = 9))+
  ggtitle("B")

###read results Czechia and visualize
Czechia_df <- read.csv("ModelFitCzechia/FitCzechia.csv", header = TRUE, sep = ";")

#print(Czechia_df$timeMonth)
Czechia_df$timeMonth <- as.Date(Czechia_df$timeMonth, "%d.%m.%Y")
#print(Czechia_df$timeMonth)
#Czechia_df$timeMonth <- format(as.Date(Czechia_df$timeMonth, "%Y-%m-%d"), "%m-%d-%Y")
#Czechia_df$timeMonth <- as.Date(Czechia_df$timeMonth)

p3 <- ggplot(Czechia_df, aes(timeMonth)) +
  geom_line(aes(y = d), size = 0.1, colour = 'grey') +
  geom_line(aes(y = deaths.overall), size = 0.5 , colour = 'black') +
  ylab("Deaths") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=6),
    axis.text.x=element_text(size=6),
    axis.title.y=element_text(size=7),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    panel.background = element_blank(),
    plot.title = element_text(size = 9))+
  ggtitle("Czechia")+ 
  annotate(geom="text", x=as.Date("2020-01-13"), y=100, label=expression("R0"["model"]*"=2.05"),  #"R0 model = 1.98\nR0 data = 2.87"
           color="red", size = 3, hjust = 0)

p4 <- ggplot(Czechia_df, aes(timeMonth)) +
  geom_line(aes(y = dsum), size = 1, colour = 'grey') +
  geom_line(aes(y = died.overall), size = 0.5, colour = 'black') +
  ylab("Cumulative deaths") +
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
Hungary_df <- read.csv("ModelFitHungary/FitHungary.csv", header = TRUE, sep = ";")

print(Hungary_df$timeMonth)
Hungary_df$timeMonth <- as.Date(Hungary_df$timeMonth, "%d.%m.%Y")
print(Hungary_df$timeMonth)

p5 <- ggplot(Hungary_df, aes(timeMonth)) +
  geom_line(aes(y = d..data.), size = 0.1, colour = 'grey') +
  geom_line(aes(y = deaths.overall), size = 0.5 , colour = 'black') +
  ylab("Deaths") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=6),
    axis.text.x=element_text(size=6),
    axis.title.y=element_text(size=7),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    panel.background = element_blank(),
    plot.title = element_text(size = 9))+
  ggtitle("Hungary")+ 
  annotate(geom="text", x=as.Date("2020-01-13"), y=90, label=expression("R0"["model"]*"=1.68"),  #"R0 model = 1.98\nR0 data = 2.87"
           color="red", size = 3, hjust = 0)

p6 <- ggplot(Hungary_df, aes(timeMonth)) +
  geom_line(aes(y = dsum..data.), size = 1, colour = 'grey') +
  geom_line(aes(y = died.overall), size = 0.5 , colour = 'black') +
  ylab("Cumulative deaths") +
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
Slovenia_df <- read.csv("ModelFitSlovenia/FitSlovenia.csv", header = TRUE, sep = ";")


Slovenia_df$timeMonth <- as.Date(Slovenia_df$timeMonth, "%d.%m.%Y")


p7 <- ggplot(Slovenia_df, aes(timeMonth)) +
  geom_line(aes(y = d..data.), size = 0.1, colour = 'grey') +
  geom_line(aes(y = deaths.overall), size = 0.5 , colour = 'black') +
  ylab("Deaths per day") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=6),
    axis.text.x=element_text(size=6),
    axis.title.y=element_text(size=7),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    panel.background = element_blank(),
    plot.title = element_text(size = 9))+
  ggtitle("C")+ 
  annotate(geom="text", x=as.Date("2020-03-05"), y=20, label=expression("R0"["model"]*"=2.6"),  #"R0 model = 1.98\nR0 data = 2.87"
           color="red", size = 3, hjust = 0)

p8 <- ggplot(Slovenia_df, aes(timeMonth)) +
  geom_line(aes(y = dsum..data.), size = 1.5, colour = 'grey70') +
  geom_line(aes(y = died.overall), size = 0.5 , colour = 'black') +
  ylab("Cumulative deaths") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=6),
    axis.text.x=element_text(size=6),
    axis.title.y=element_text(size=7),
    axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    panel.background = element_blank(),
    plot.title = element_text(size = 9))+
  ggtitle("D")

#arrange plots
ggarrange(p1, p2, p7, p8, ncol = 2)

