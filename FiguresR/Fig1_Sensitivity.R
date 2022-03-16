update.packages("ggplot2")
library(ggplot2)
library(egg)
library(plotly)
library(reshape2)
library(tseries)
library(fields)

setwd("C:/Users/b1045498/Nextcloud/01_Salzburg_new/03_Forschung/05_Applications/06_InfectiousDiseaseModelValidation/04_results/01_FinalResults/01_ModelSensitivity/ModelSensitivityAustria")

###read outbreak size data
outbreaksize_sensitivity_df <- read.csv("outbreakSize_infections.csv", header = TRUE, sep = ";")
#print(outbreaksize_sensitivity_df)

outbreaksize_sensitivity_df$magnitude <- as.character(outbreaksize_sensitivity_df$magnitude)

#scale_color_brewer
#palette="Dark2" "#FF0000", "#00CC00", "blue2"
#"#8e5859", "#3953a4", "#69bd45"
#rgb(237, 32, 36),rgb(105, 189, 69),rgb(57, 83, 164)
#color = sizes


p1 <- ggplot(outbreaksize_sensitivity_df, aes(x = time, y=infect, group = magnitude)) +  
  geom_line(aes(color = magnitude), size = 0.5) +
  scale_color_manual(values=c("#ed2024", "#69bd45", "#3953a4"), name =  expression('Outbreak size I'["(t=0)"])) +  #'Outbreak size '*I[(t=0)]*''
  ylab("Infections/10K people") +
  xlab("Time (days)") +
  theme_classic()+
  theme(
        axis.text.y=element_text(size=7),
        axis.title.y=element_text(size=8),
        axis.text.x=element_text(size=7),
        axis.title.x=element_text(size=8),
        legend.text = element_text(size=7),
        legend.title = element_text(size=8),
        axis.line.y = element_line(colour = 'black', size = 0.1),
        axis.line.x = element_line(colour = 'black', size = 0.1),
        panel.background = element_blank())+
  ggtitle("A")

###read contact base data
ContactBase_sensitivity_df <- read.csv("contactsBase-infections.csv", header = TRUE, sep = ";")
print(ContactBase_sensitivity_df)

ContactBase_sensitivity_df$contactsgroup <- as.character(ContactBase_sensitivity_df$contactsgroup)

p2 <- ggplot(ContactBase_sensitivity_df, aes(x = time, y=infect, group = contactsgroup)) +  
  geom_line(aes(color = contactsgroup), size = 0.5) +
  scale_color_manual(values=c("#ed2024", "#69bd45", "#3953a4", "#FF8C00"), name = expression('Contacts per day c'["d"])) +
  xlab("Time (days)") +
  ylab("Infections/10K people") +
  theme_classic() +
  theme(axis.text.y=element_text(size=7),
        axis.title.y=element_text(size=8),
        axis.text.x=element_text(size=7),
        axis.title.x=element_text(size=8),
        legend.text = element_text(size=7),
        legend.title = element_text(size=8),
        axis.line.y = element_line(colour = 'black', size = 0.1),
        axis.line.x = element_line(colour = 'black', size = 0.1),
        panel.background = element_blank())+
  ggtitle("B")

###read outbreak size data
UVEffect_sensitivity_df <- read.csv("UviEffect_infections.csv", header = TRUE, sep = ";")
print(UVEffect_sensitivity_df)

UVEffect_sensitivity_df$effect <- as.character(UVEffect_sensitivity_df$effect)

p3 <- ggplot(UVEffect_sensitivity_df, aes(x = time, y=infect, group = effect)) +  
  geom_line(aes(color = effect), size = 0.5) +
  scale_color_manual(values=c("#ed2024", "#69bd45", "#3953a4", "#FF8C00"), name = expression('UVI effect UV'["E"])) +
  xlab("Time (days)") +
  ylab("Infections/10K people") +
  theme_classic() +
  theme(axis.text.y=element_text(size=7),
        axis.title.y=element_text(size=8),
        axis.text.x=element_text(size=7),
        axis.title.x=element_text(size=8),
        legend.text = element_text(size=7),
        legend.title = element_text(size=8),
        axis.line.y = element_line(colour = 'black', size = 0.1),
        axis.line.x = element_line(colour = 'black', size = 0.1),
        panel.background = element_blank())+
  ggtitle("C")
    
#create surface function
surf <- function(af_attr, ap_attr){
  1*(1-af_attr+ap_attr*af_attr)/(1-af_attr+0.5*af_attr)
} 

af <- seq(0, 1, 0.01)
ap <- seq(0, 1, 0.01)
R0s <- outer(af, ap, surf)

# Color palette (100 colors)
col.pal<-colorRampPalette(c("blue", "red"))  #"blue", "red"
colors<-col.pal(100)
# height of facets
R0s.facet.center <- (R0s[-1, -1] + R0s[-1, -ncol(R0s)] + R0s[-nrow(R0s), -1] + R0s[-nrow(R0s), -ncol(R0s)])/4
# Range of the facet center on a 100-scale (number of colors)
R0s.facet.range<-cut(R0s.facet.center, 100)

fig <- persp(af, ap, R0s, theta = 45, phi = 18,
             col = colors[R0s.facet.range], shade = 0.01, border = NA, ticktype = "detailed")

## add color bar
image.plot(legend.only=T, zlim=range(R0s.facet.center), col=colors)

#Create perspective plot
fig

#arrange plots
#ggarrange(p1, p2, p3, ncol = 1)

