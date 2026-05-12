rm(list = ls())

library(ggplot2)

library(tidyverse)
library(lubridate)

library(dplyr)

library(reshape2)

library(ggpubr)  # ggarrange
#library(grid)

# ===========with the help of ChatGPT ===================

setwd("H:/PostJul2024/Drought_study/Propagation&Lag/TransitionalProbability")

# ==========================================
# 1. Read and prepare SPI data
# ==========================================

SPI_datapath <- "H:/PostJul2024/Drought_study/Propagation&Lag/LagAnalysis_SPI_SQI/SPI_LagAnalysis/Alternative_def1_CG/"
spi <- read.csv(paste0(SPI_datapath,"M12_SPI8Cats_datefixed.csv"), stringsAsFactors = FALSE)

spi$YrMonths <- as.Date(spi$YrMonths)

spi_heads <- names(spi)


# ================================
# USER SETTINGS
# ================================
trigger_thresh <- -1.5
start_thresh   <- -0
end_thresh     <- -0
end_consec     <- 3      # number of consecutive months to break drought



#events_path <- "H:/PostJul2024/Drought_study/Propagation&Lag/LagAnalysis_SPI_SQI/SSI_LagAnalysis/Alternative_def2Q/"

events_path <- "H:/PostJul2024/Drought_study/Propagation&Lag/LagAnalysis_SPI_SQI/SPI_LagAnalysis/Alternative_def2/"

events_long <- read.csv(paste0(events_path,"All_Drought_Events",start_thresh,"_",end_thresh,"_th",trigger_thresh,".csv"))



get_drought_spi <- function(start, end) {
  spi_df %>% filter(date >= start & date <= end)
}

p <- list()

for (icat in 1:8){

  catname <- spi_heads[icat+1]
  catname1 <- substr(catname, 2,7)
  spi_df <- data.frame(date = spi$YrMonths, spi = spi[,(icat+1)])


  
  state <- cut(spi_df$spi,
   #            breaks = c(-Inf, -2, -1.5, -0.5, 0.5, 1.5, 2, Inf),
    #           labels = c("SD", "ModD", "MildD", "N", "MildW","ModW", "VW"))
  
               breaks = c(-Inf, -2, -1.5, -1.0, 1, 1.5, 2, Inf),   #https://drought.emergency.copernicus.eu/data/factsheets/factsheet_spi.pdf
               labels = c("ED", "SD", "ModD", "N", "MoW","SW", "EW"))
  
  #SevereDrought, ModerateDrought, Normal, Wet, Very Wet
  
  trans_counts <- table(state[-length(state)], state[-1])
  P <- prop.table(trans_counts, margin = 1)
  P1 <- round(P, 3)
  
  
  
  #colhead <- paste0(catname,paste0("-", colnames(P1)))
  
  #colnames(P1) <- colhead

  if (icat == 1){
  write.table(P1,paste0("TransProb_wetdry",start_thresh,"_",end_thresh,"_th",trigger_thresh,".csv"), sep =",", append = F)
}else  write.table( P1,paste0("TransProb_wetdry",start_thresh,"_",end_thresh,"_th",trigger_thresh,".csv"), sep =",",append = T)
  
  df_plot <- melt(P1)
  
  p[[icat]] <- ggplot(df_plot, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    geom_text(aes(label = round(value, 2)),size = 3) +
    scale_fill_gradient(low = "white", high = "red") +
    labs(x = paste0(catname1, " From"), y = "To", fill = "Probability") +
    theme_minimal()
  
  
}  # icat



filename_s<- paste0("Wetdry_8_trasprob_",start_thresh,"_",end_thresh,"_th",trigger_thresh,".png")
png(file = filename_s,units = "mm", width = 200, height = 200, res = 600)

plot <-ggarrange(
  p[[1]], p[[2]],
  p[[3]], p[[4]],
  p[[5]], p[[6]], 
  p[[7]], p[[8]], 
  ncol = 2, nrow = 4
  )

print(plot)

dev.off()