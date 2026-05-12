rm(list = ls())

library(ggplot2)

library(tidyverse)
library(lubridate)

library(dplyr)

library(reshape2)
library(ggpubr)  # ggarrange

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

pd <- list()

for (icat in 1:8){

  catname <- spi_heads[icat+1]
  catname1 <- substr(catname, 2,7)
  spi_df <- data.frame(date = spi$YrMonths, spi = spi[,(icat+1)])


  drought_spi <- events_long %>%
    rowwise() %>%
#    do(get_drought_spi(.$start, .$end))
  do(get_drought_spi(.$Start, .$End))
  
  drought_spi$state <- cut(drought_spi$spi,
                           breaks = c(-Inf, -2, -1.5, -1, 1),  #https://drought.emergency.copernicus.eu/data/factsheets/factsheet_spi.pdf
                           labels = c("ED", "SD", "ModD","NN")) 
  
  
  
  s <- drought_spi$state
  
  trans_counts_drought <- table(s[-length(s)], s[-1])  # table gives frequency SA
  P_drought <- prop.table(trans_counts_drought, margin = 1)
  
  P_drought1<-  round(P_drought, 3)
  #colhead <- paste0(" ",catname,"-", colnames(P_drought1))
  colhead <- paste0(catname,paste0("-", colnames(P_drought1)))
  
  colnames(P_drought1) <- colhead

  if (icat == 1){
  write.table(P_drought1,paste0("TransProb_dryonly",start_thresh,"_",end_thresh,"_th",trigger_thresh,".csv"), sep =",", append = F)
}else  write.table( P_drought1,paste0("TransProb_dryonly",start_thresh,"_",end_thresh,"_th",trigger_thresh,".csv"), sep =",",append = T)
  

df_plot <- melt(P_drought)

pd[[icat]] <- ggplot(df_plot, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)),size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = paste0(catname1, " From"), y = "To", fill = "Probability") +
  theme_minimal()

}  # icat

filename_d<- paste0("Dry_8_trasprob_",start_thresh,"_",end_thresh,"_th",trigger_thresh,".png")
png(file = filename_d,units = "mm", width = 200, height = 200, res = 600)
plot <-ggarrange(
  pd[[1]], pd[[2]],
  pd[[3]], pd[[4]],
  pd[[5]], pd[[6]], 
  pd[[7]], pd[[8]], 
  ncol = 2, nrow = 4
)

print(plot)

dev.off()
