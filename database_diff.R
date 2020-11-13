##--------------------------------
## Create the database 
## PS Explore the data base 
## Notes: 
##--------------------------------
library(dplyr)

database <- read.csv("database.csv")
database$newname  <- as.factor(database$newname)
database <- database[order(database$AssessmentYear),]
## Substitute where values for reference points are 0 to NA, and recalculate status

database <- database %>%
    mutate(FMSY = replace(FMSY, FMSY == 0, NA)) %>%
    mutate(FFMSY = F/FMSY) %>%
    mutate(MSYBtrigger = replace(MSYBtrigger, MSYBtrigger == 0, NA)) %>%
    mutate(BBMSY = SSB/MSYBtrigger) 

database$newname  <- droplevels(database$newname)


### detect changes and diff estimation for fishing mortality
datadiff <- list()
for (k in 1:length(levels(database$newname)) ){
  
  stock <- levels(database$newname)[k]
  res <- subset(database, newname == stock)
   
    FMSY_df <- unique(res[, c("AssessmentYear", "FMSY")])
    if (nrow( FMSY_df)>1){
    FMSY_df$change <- c(NA, FMSY_df$FMSY[2:nrow(FMSY_df)] != FMSY_df$FMSY[1:(nrow(FMSY_df)-1)])
  
  res <- merge(res, FMSY_df)
  
  res_split <- split(res, res$AssessmentYear)

if (length(res_split)>1){
  
      diff_split <- list()
      for(i in 2:length(res_split)){
          tmp <- merge(res_split[[i]], res_split[[i-1]], by = "Year")
          tmp$diff <- with(tmp, FFMSY.x - FFMSY.y)
          tmp$reldiff <- with(tmp, (FFMSY.x - FFMSY.y)/FFMSY.y)
          tmp$diffF <- with(tmp, F.x - F.y)
          tmp$diffFMSY <- with(tmp, FMSY.x - FMSY.y)
           diff_split <- rbind(diff_split, tmp)
      }

      datadiff<-rbind(datadiff,diff_split)
}
    }
    }


write.csv(datadiff, "database_diff_F.csv")



### detect changes and diff estimation for biomass
datadiff <- list()
for (k in 1:length(levels(database$newname)) ){
  
  stock <- levels(database$newname)[k]
  res <- subset(database, newname == stock)
  
  MSYBtrigger_df <- unique(res[, c("AssessmentYear", "MSYBtrigger")])
  if (nrow( MSYBtrigger_df)>1){
    MSYBtrigger_df$change <- c(NA, MSYBtrigger_df$MSYBtrigger[2:nrow(MSYBtrigger_df)] != MSYBtrigger_df$MSYBtrigger[1:(nrow(MSYBtrigger_df)-1)])
    
    res <- merge(res, MSYBtrigger_df)
    
    res_split <- split(res, res$AssessmentYear)
    
    if (length(res_split)>1){
      
      diff_split <- list()
      for(i in 2:length(res_split)){
        tmp <- merge(res_split[[i]], res_split[[i-1]], by = "Year")
        tmp$diff <- with(tmp, BBMSY.x - BBMSY.y)
        tmp$reldiff <- with(tmp, (BBMSY.x - BBMSY.y)/BBMSY.y)
        tmp$diffSSB <- with(tmp, SSB.x - SSB.y)
        tmp$diffMSYBtrigger <- with(tmp, MSYBtrigger.x - MSYBtrigger.y)
        diff_split <- rbind(diff_split, tmp)
      }
      
      datadiff<-rbind(datadiff,diff_split)
    }
  }
}


write.csv(datadiff, "database_diff_B.csv")





