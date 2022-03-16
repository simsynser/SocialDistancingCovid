update.packages("ggplot2")
library(ggplot2)
library(egg)
library(reshape)

Sys.setlocale("LC_TIME", "C")

setwd("C:/Users/b1045498/Nextcloud/01_Salzburg_new/03_Forschung/05_Applications/06_InfectiousDiseaseModelValidation/04_results/01_FinalResults/04_AlternativeMeasures")




FigA_all_df <- read.csv("Lifted1stLockdown/Austria/BehaviorExplanation_Aut.csv", header = TRUE, sep = ";")

FigA_all_df$Type <- as.character(FigA_all_df$Type)

print(FigA_all_df)

p1 <- ggplot(FigA_all_df, aes(x = moderation.lockdown.1...., y=Died.overall, color = Type)) +  
  geom_point(size = 2.5) +
  scale_color_manual(breaks=c('Decrease IFR (Effect 1)','Decrease Infections (Effect 2)','Decrease IFR and Infections', 'Other'), values=c('Decrease IFR'='#ff0000ff', 'Decrease Infections'='#00d100ff', 'Decrease IFR and Infections'='#0000ffff', 'Other'='grey'), name =  "Effect of mobility increase") +    #, name =  "Behaviors"
  ylab("Cumulative deaths") +
  xlab("Mobility increase during spring outbreak [%]") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=10),
    axis.text.x=element_text(size=10),
    axis.title.y=element_text(size=11),
    #axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    #legend.text = element_text(size=9),
    #legend.title = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  ggtitle("A")



FigB_all_df <- read.csv("Lifted1stLockdown/Slovenia/BehaviorExplanation_Slo.csv", header = TRUE, sep = ";")

FigB_all_df$Type <- as.character(FigB_all_df$Type)

p2 <- ggplot(FigB_all_df, aes(x = moderation.lockdown.1...., y=Died.overall, color = Type)) +  
  geom_point(size = 2.5) +
  scale_color_manual(breaks=c('Decrease IFR','Decrease Infections','Decrease IFR and Infections', 'Other'), values=c('Decrease IFR'='#ff0000ff', 'Decrease Infections'='#00d100ff', 'Decrease IFR and Infections'='#0000ffff', 'Other'='grey'), name =  "Effect of mobility increase") +    #, name =  "Behaviors"
  ylab("Cumulative deaths") +
  xlab("Mobility increase during spring outbreak [%]") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=10),
    axis.text.x=element_text(size=10),
    axis.title.y=element_text(size=11),
    #axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    #legend.text = element_text(size=9),
    #legend.title = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  ggtitle("B")

dummy <- ggplot(FigB_all_df, aes(x = moderation.lockdown.1...., y=Died.overall, color = Type)) +  
  geom_point(size = 2.5) +
  scale_color_manual(labels=c('Decrease IFR\n(Effect 1)','Decrease Infections\n(Effect 2)','Decrease IFR and Infections\n(Effect 1 and 2)', 'Other'), values=c('Decrease IFR'='#ff0000ff', 'Decrease Infections'='#00d100ff', 'Decrease IFR and Infections'='#0000ffff', 'Other'='grey'), name =  "Mobility increase during spring outbreak leads to...") +    #, name =  "Behaviors"
  ylab("Cumulative deaths") +
  xlab("Mobility increase [%]") +
  theme_classic()+
  theme(
    axis.text.y=element_text(size=10),
    axis.text.x=element_text(size=10),
    axis.title.y=element_text(size=11),
    #axis.title.x=element_blank(),
    axis.line.y = element_line(colour = 'black', size = 0.1),
    axis.line.x = element_line(colour = 'black', size = 0.1),
    #legend.text = element_text(size=9),
    #legend.title = element_blank(),
    #legend.position = "none",
    panel.background = element_blank(),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    legend.position="bottom")+
  ggtitle("B")

# Create user-defined function, which extracts legends from ggplots
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

# Apply user-defined function to extract legend
shared_legend <- extract_legend(dummy)

# Draw plots with shared legend
grid.arrange(arrangeGrob(p1, p2, ncol = 2),
             shared_legend, nrow = 2, heights = c(10, 1))

#grid.arrange(arrangeGrob(p1, p2, shared_legend, ncol = 3))