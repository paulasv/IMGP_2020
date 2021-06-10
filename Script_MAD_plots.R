##--------------------------------
## 
## PS 
## Notes: 
##--------------------------------

library(dplyr)
library(ggplot2)

# 1. CONDISER ALL PAIRS OF SEQUENTIAL ASSESSMENTS FOR THE ANALYSIS. CHECK IF
# CHANGE IN F OR SSB CAN STIL CHANGE EVEN AS REFERENCE POINT DO NOT CHANGE (similar changes?)

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


# Data and cleaning
# Fishing mortality reference points changes data 
data_F <- read.csv("database_diff_F_17April.csv")
## Event filtering for F
redondeo <- c("bss.27.8ab 2019" ,  "cod.21.1 2019"    , "cod.27.24-32 2011", "cod.27.6a 2017" ,   "cod.27.7e-k 2017" , "her.27.nirs 2018" , "ldb.27.8c9a 2018" , "meg.27.8c9a 2013" , "meg.27.8c9a 2014" , "meg.27.8c9a 2018",  "ple.27.7a 2018"   , "sol.27.7e 2017"   ,"sol.27.7e 2019"  ,  "sol.27.7fg 2017")
rts <- c("ple.27.7d 2014" ,  "ple.27.7e 2018"  , "ple.27.7e 2019"  , "ple.27.7h-k 2019", "reb.2127.dp 2019" ,"sol.27.7h-k 2019")
mixed <- c("ank.27.8c9a 2012", "pra.27.3a4a 2015", "ple.27.7d 2015", "sol.27.7d 2019" , "ple.27.7e 2015" , "her.27.6a7bc 2017")
## Substitutions
data_F[data_F$newname.x == "sol.27.7d" & data_F$AssessmentYear.x == 2019, 4] <- 0.192 
data_F[data_F$newname.x == "ple.27.7d" & data_F$AssessmentYear.x == 2015, 21] <- 0.15
data_F[data_F$newname.x == "her.27.6a7bc" & data_F$AssessmentYear.x == 2017, 21] <- 0.25 


# Biomass reference points changes data   
data_B <- read.csv("database_diff_B_17April.csv")
## Event filtering for B
relatives.b <- c("ank.27.8c9a", "pra.27.3a4a", "rng.27.5b6712b", "lez.27.6b", "nop.27.3")
redondeo.b <-c("her.27.nirs 2018" ,     "hom.27.2a4a5b6a7a-ce-k8 2018" ,"meg.27.8c9a 2014", "ple.27.21-23 2017" , "ple.27.7a 2018", "pok.27.3a46 2018" , "pok.27.3a46 2019","sol.27.7e 2017"    ,"sol.27.7e 2019"  ,"whg.27.6a 2018")
rts.b <- c( "ple.27.7e 2019" ,  "ple.27.7e 2018" ,  "ple.27.7h-k 2019", "reb.2127.dp 2019", "sol.27.7h-k 2019")
## Substitutions
data_B[data_B$newname.x == "sol.27.7d" & data_B$AssessmentYear.x == 2019, 4] <- 15072 
data_B[data_B$newname.x == "ple.27.7e" & data_B$AssessmentYear.x == 2015, 4] <- 2400


# Decomposition of status database for all assessments
# Filter by fishing pressure description
absolute <- database %>%
  filter(FishingPressureDescription == "F")%>%
  group_by(newname, AssessmentYear)%>%
  summarise()%>%
  mutate(event = paste(newname, AssessmentYear))

## For F complete timeseries
data_change <- data_F %>%
  filter(!is.na(FFMSY.x), !is.na(FFMSY.y))%>%
  filter(!newname.x %in% nephrops)%>%
  mutate(newname.x =droplevels(newname.x))%>%
  group_by(newname.x, AssessmentYear.x)%>%
  mutate(event = paste(newname.x, AssessmentYear.x))%>%
  filter(!event %in% redondeo)%>%
  filter(!event %in% rts)%>%
  filter(event %in% absolute$event)%>%
  mutate(reldiff = (FFMSY.x-FFMSY.y)/FFMSY.y)%>%
  summarise(n = n(), 
            change = unique(change.x),
            mean.pos = mean(F.x, na.rm = TRUE),
            mean.pre = mean(F.y, na.rm = TRUE),
            pos = unique(FMSY.x), 
            pre = unique(FMSY.y), 
            diff = (unique(FMSY.x)-unique(FMSY.y))/unique(FMSY.y),
            gamma = sum(F.x, na.rm = TRUE)/sum(F.y, na.rm = TRUE), 
            delta = unique(FMSY.x)/ unique(FMSY.y),
            y = gamma/delta - 1)%>%
  mutate(event = paste(newname.x, AssessmentYear.x))


## For F 5 las years
data_change <- data_F %>%
  filter(!is.na(FFMSY.x), !is.na(FFMSY.y))%>%
  filter(!newname.x %in% nephrops)%>%
  mutate(newname.x =droplevels(newname.x))%>%
  group_by(newname.x, AssessmentYear.x)%>% 
  filter(Year>(max(Year)-5)) %>%
  summarize(n_c = n(), 
            change_c = unique(change.x),
            mean.pos_c = mean(F.x, na.rm = TRUE),
            mean.pre_c = mean(F.y, na.rm = TRUE),
            pos = mean(FMSY.x), 
            pre = mean(FMSY.y), 
            diff_c = (unique(FMSY.x)-unique(FMSY.y))/unique(FMSY.y),
            gamma_c = sum(F.x, na.rm = TRUE)/sum(F.y, na.rm = TRUE), 
            delta_c = mean(FMSY.x)/ mean(FMSY.y),
            y_c = gamma_c/delta_c - 1)%>%
  ungroup()%>%
  mutate(event = paste(newname.x, AssessmentYear.x))%>%
  filter(!event %in% redondeo)%>%
  filter(!event %in% rts)%>%
  filter(event %in% absolute$event)%>%
  full_join(data_change)




## For SSB complete timeseries
data_change <- data_B %>%
  filter(!is.na(BBMSY.x), !is.na(BBMSY.y))%>%
  filter(!newname.x %in% nephrops)%>%
  mutate(newname.x =droplevels(newname.x))%>%
  group_by(newname.x, AssessmentYear.x)%>%
  mutate(event = paste(newname.x, AssessmentYear.x))%>%
  filter(!event %in% redondeo.b)%>%
  filter(!event %in% rts.b)%>%
  filter(!newname.x %in% relatives.b)%>%
  filter(event %in% absolute$event)%>%
  mutate(reldiff = (BBMSY.x-BBMSY.y)/BBMSY.y)%>%
  summarise(n_b = n(), 
            change_b = unique(change.x),
            mean.pos_b = mean(SSB.x, na.rm = TRUE),
            mean.pre_b = mean(SSB.y, na.rm = TRUE),
            pos_b = unique(MSYBtrigger.x), 
            pre_b = unique(MSYBtrigger.y), 
            diff_b= (unique(MSYBtrigger.x)-unique(MSYBtrigger.y))/unique(MSYBtrigger.y),
            gamma_b = sum(SSB.x, na.rm = TRUE)/sum(SSB.y, na.rm = TRUE), 
            delta_b = unique(MSYBtrigger.x)/ unique(MSYBtrigger.y),
            y_b = gamma_b/delta_b - 1)%>%
  mutate(event = paste(newname.x, AssessmentYear.x))%>%
  full_join(data_change)

## For SSB last 5 years timeseries
data_change <- data_B %>%
  filter(!is.na(BBMSY.x), !is.na(BBMSY.y))%>%
  filter(!newname.x %in% nephrops)%>%
  mutate(newname.x =droplevels(newname.x))%>%
  group_by(newname.x, AssessmentYear.x)%>%
  mutate(event = paste(newname.x, AssessmentYear.x))%>%
  filter(!event %in% redondeo.b)%>%
  filter(!event %in% rts.b)%>%
  filter(!newname.x %in% relatives.b)%>%
  mutate(reldiff = (BBMSY.x-BBMSY.y)/BBMSY.y)%>%
  filter(Year>(max(Year)-5)) %>%
  summarise(n_b_c = n(), 
            change_b_c = unique(change.x),
            mean.pos_b_c = mean(SSB.x, na.rm = TRUE),
            mean.pre_b_c = mean(SSB.y, na.rm = TRUE),
            pos_b_c = unique(MSYBtrigger.x), 
            pre_b_c = unique(MSYBtrigger.y), 
            gamma_b_c = sum(SSB.x, na.rm = TRUE)/sum(SSB.y, na.rm = TRUE), 
            delta_b_c = unique(MSYBtrigger.x)/ unique(MSYBtrigger.y),
            y_b_c = gamma_b_c/delta_b_c - 1)%>%
  ungroup()%>%
  mutate(event = paste(newname.x, AssessmentYear.x))%>%
  filter(!event %in% redondeo.b)%>%
  filter(!event %in% rts.b)%>%
  filter(event %in% absolute$event)%>%
  full_join(data_change)%>%
  arrange(event)

# Test variation MAD  

# complete timeseries F
tmp <- data_change %>%
  filter(!is.na(change))%>%
  filter(change == TRUE)# rp change
mad(tmp$mean.pos-tmp$mean.pre, tmp$change)
tmp <- data_change %>%
  filter(!is.na(change))
mad(tmp$mean.pos-tmp$mean.pre, tmp$change)
# recentent 5 years F
tmp <- data_change %>%
  filter(!is.na(change_c))%>%
  filter(change == TRUE)# rp change
mad(tmp$mean.pos_c-tmp$mean.pre_c, tmp$change_c)
tmp <- data_change %>%
  filter(!is.na(change_c))
mad(tmp$mean.pos_c-tmp$mean.pre_c, tmp$change_c)
# complete timeseries B
tmp <- data_change %>%
  filter(!is.na(change_b))%>%
  filter(change == TRUE)# rp change
mad(tmp$mean.pos_b-tmp$mean.pre_b, tmp$change_b)
tmp <- data_change %>%
  filter(!is.na(change_b))
mad(tmp$mean.pos_b-tmp$mean.pre_b, tmp$change_b)
# recentent 5 years B
tmp <- data_change %>%
  filter(!is.na(change_b_c))%>%
  filter(change == TRUE)# rp change
mad(tmp$mean.pos_b_c-tmp$mean.pre_b_c, tmp$change_b_c)
tmp <- data_change %>%
  filter(!is.na(change_b_c))
mad(tmp$mean.pos_b_c-tmp$mean.pre_b_c, tmp$change_b_c)



# 2. CHECK IF SIMULTANEOUS CHANGES ARE PAIRED OR DEVIATED FROM EXPECTED DIRECTION

data <- read.csv("data_gdy.csv")
mixed <- c("ank.27.8c9a 2012", "pra.27.3a4a 2015","ple.27.7e 2015")
labels<-read.csv("labels.csv")
data<-cbind(labels, data)

tmpF <- data%>%
  filter(!is.na(delta) & !is.na(delta_b))%>%
  filter(!event %in% mixed)%>%
  select( AssessmentYear.x, newname.x, change, Abbreviated.name)%>%
  mutate(type = "FMSY")
tmpB <- data%>%
  filter(!is.na(delta) & !is.na(delta_b))%>%
  filter(!event %in% mixed)%>%
  select( AssessmentYear.x, newname.x,change_b, Abbreviated.name)%>%
  mutate(change = change_b)%>%
  select( AssessmentYear.x, newname.x,change, Abbreviated.name)%>%
  mutate(type = "MSYBtrigger")

dataT <- rbind(tmpF, tmpB)  

dataT %>%
  mutate(event = paste(newname.x, AssessmentYear.x))%>%
  mutate(event = fct_relevel(event, rev)) %>%
  ggplot( aes(type, event, fill = change))+
  geom_tile()+
  scale_fill_gradient2(low = "#009E73", midpoint = 0,mid =  "white",  high = "#E69F00" ,limits = c(-1.3, 1.3) ,  na.value = "#E69F00" )+
  xlab("")+ylab("")


in_dec <- data%>%
  filter(!is.na(change) & !is.na(change_b))%>%
  filter(change >0 & change_b<0)%>%
  droplevels()%>%
  pull(event)
  
dec_in <- data%>%
  filter(!is.na(change) & !is.na(change_b))%>%
  filter(change <0 & change_b>0)%>%
  droplevels()%>%
  pull(event)
  
dec_dec <- data%>%
  filter(!is.na(change) & !is.na(change_b))%>%
  filter(change <0 & change_b<0)%>%
  droplevels()%>%
  pull(event)

in_in <- data%>%
  filter(!is.na(change) & !is.na(change_b))%>%
  filter(change >0 & change_b>0)%>%
  droplevels()%>%
  pull(event) 

# Figure Suplementary relation between fmsy and msybtrigger
data%>%
  filter(!is.na(change) & !is.na(change_b))%>%
  ggplot(aes(change, change_b))+
  geom_hline(yintercept = 0, linetype = 3,colour = "gray")+
  geom_vline(xintercept = 0, linetype = 3,colour = "gray")+
  #geom_text(aes(label = event))+
  geom_text(aes(label = Ind), size = 2.6)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = scales::percent)+
  xlab(expression("Change in F"[MSY]))+ylab(expression("Change in MSYB"[trigger]))

