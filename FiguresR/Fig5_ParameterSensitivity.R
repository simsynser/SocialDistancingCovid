update.packages("ggplot2")
library(ggplot2)
library(egg)
library(plotly)
library(reshape2)
library(tseries)
library(fields)
library("scatterplot3d")
library(rgl)
library(paletteer)
library(rccmisc)
library(scico)


#R.Version()

palettes_c_names

setwd("C:/Users/b1045498/Nextcloud/01_Salzburg_new/03_Forschung/05_Applications/06_InfectiousDiseaseModelValidation/04_results/01_FinalResults/05_ParameterSensitivity")

###read parameter sensitivity data Austria, Slovenia
at_sensitivity_df <- read.csv("ResultsSensitivityRun_Austria_large.csv", header = TRUE, sep = ";")
slo_sensitivity_df <- read.csv("ResultsSensitivityRun_Slovenia_large.csv", header = TRUE, sep = ";")

###cum deaths data Austria
at_cum_deaths_df <- read.csv("dsum2_at.csv", header = TRUE, sep = ";")
slo_cum_deaths_df <- read.csv("dsum2_slo.csv", header = TRUE, sep = ";")


###create df for plot data Austria: cd, UVIE, outbreak size, RMSE
at_plot_df <- data.frame(matrix(ncol = 4, nrow = 100000))
x_at <- c("cd", "uvie", "out", "rmse")
colnames(at_plot_df) <- x_at

slo_plot_df <- data.frame(matrix(ncol = 4, nrow = 100000))
x_slo <- c("cd", "uvie", "out", "rmse")
colnames(slo_plot_df) <- x_slo

# assign values to plot df
at_plot_df$cd <- at_sensitivity_df$contacts.base
at_plot_df$uvie <- at_sensitivity_df$UVI.effect
at_plot_df$out <- at_sensitivity_df$outbreak.size

slo_plot_df$cd <- slo_sensitivity_df$contacts.base
slo_plot_df$uvie <- slo_sensitivity_df$UVI.effect
slo_plot_df$out <- slo_sensitivity_df$outbreak.size

#print(nrow(at_sensitivity_df))
#print(ncol(at_sensitivity_df))
#print(nrow(at_cum_deaths_df))

#calculate RMSE and insert in at_plot_df Austria
for (r in 1:nrow(at_sensitivity_df)){   #get sensitivity runs

  run_vec <- numeric(0)
  squared_deviation <- numeric(0)
  
  
  for (c in 5:ncol(at_sensitivity_df)){
    
    
    run_vec <- c(run_vec, at_sensitivity_df[r , c])
    
    
  }
  
  for (r1 in 1:nrow(at_cum_deaths_df)){
    
    squared_deviation <- c(squared_deviation, (at_cum_deaths_df[r1 , 2] - run_vec[r1])^2)
    
  }
  
  at_plot_df$rmse[r] <- sqrt(sum(squared_deviation) / 393)
  
}

#Calculate RMSE Slovenia
for (r in 1:nrow(slo_sensitivity_df)){   #get sensitivity runs
  
  run_vec <- numeric(0)
  squared_deviation <- numeric(0)
  
  
  for (c in 5:ncol(slo_sensitivity_df)){
    
    
    run_vec <- c(run_vec, slo_sensitivity_df[r , c])
    
    
  }
  
  for (r1 in 1:nrow(slo_cum_deaths_df)){
    
    squared_deviation <- c(squared_deviation, (slo_cum_deaths_df[r1 , 2] - run_vec[r1])^2)
    
  }
  
  slo_plot_df$rmse[r] <- sqrt(sum(squared_deviation) / 383)
  
}

# get top per mill
at_plot_df_sub <- subset(at_plot_df, rmse < quantile(at_plot_df$rmse, probs = c(.001))) 
slo_plot_df_sub <- subset(slo_plot_df, rmse < quantile(slo_plot_df$rmse, probs = c(.001)))

write.csv(at_plot_df_sub, "Top100_at.csv")
write.csv(slo_plot_df_sub, "Top100_slo.csv")

# gets second per mill
at_plot_df_sub_sub <- subset(at_plot_df, rmse > quantile(at_plot_df$rmse, probs = c(.001)) & rmse < quantile(at_plot_df$rmse, probs = c(1))) #.002
slo_plot_df_sub_sub <- subset(slo_plot_df, rmse > quantile(at_plot_df$rmse, probs = c(.001)) & rmse < quantile(at_plot_df$rmse, probs = c(1))) #.002

colors_at=rgb(0, 0, 1, 1)  #(at_plot_df_sub$rmse / max(at_plot_df_sub$rmse))^3

sc3d <- scatterplot3d(at_plot_df_sub$cd, at_plot_df_sub$uvie, at_plot_df_sub$out, 
              pch=16, cex.symbols=0.8,  
              color = colors_at, xlim=c(6.62,12.012),
              ylim=c(7.01,14.28),
              zlim=c(1103,4595),
              xlab = expression("Initial social contacts per day "*"c"["d(t=0)"]),
              ylab = expression("Effect of UVI on infection rate "*"UVI"["E"]),
              zlab = expression("Outbreak size "*"I"["(t=0)"]))  

#add points second per mill Austria
#colors_at2=rgb(0, 0, 0.5, 1)
sc3d$points3d(at_plot_df_sub_sub$cd, at_plot_df_sub_sub$uvie, at_plot_df_sub_sub$out, col = "#0001ff0f", cex = 0.02, type="p") 

#type="h", lty.hplot=1,

#add points first per mill Slovenia
#colors_slo=rgb(1, 0, 0, 1)
sc3d$points3d(slo_plot_df_sub$cd, slo_plot_df_sub$uvie, slo_plot_df_sub$out, col = "#ff0100ff", pch=16, cex = 0.8)

#type="h", lty.hplot=1, pch=16, cex.symbols=1, 

#add points second per mill Slovenia
#colors_slo2=rgb(0.5, 0, 0, 1)
sc3d$points3d(slo_plot_df_sub_sub$cd, slo_plot_df_sub_sub$uvie, slo_plot_df_sub_sub$out, col = "#ff01000a", cex = 0.02, type="p")


 

#s3d_slo <- scatterplot3d(slo_plot_df_sub$cd, slo_plot_df_sub$uvie, slo_plot_df_sub$out, 
#              pch=16, cex.symbols=1, type="h", lty.hplot=1, 
#              color = colors_slo, 
#              xlab = "Initial social contacts per day c_(d(t=0))",
#              ylab = "Effect of UVI on infection rate 〖UVI〗_E",
#              zlab = "Outbreak size I_((t=0))") 

#s3d_slo$points3d()





