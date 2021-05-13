##--------------------------------
## Analysis of the changes in reference points
## Status changes decompostion
## PS
## Notes: 
##--------------------------------

library(ggplot2)
library(forcats)
library(dplyr)
library(tidyr)
library(broom)
library(knitr)
library(tidytext)
library(stringr)
library(ggpubr)

# # # # # # # # Databases cleaning and filtering # # # # # # # # # # # 

# Nephrops stocks
nephrops <- c("nep.fu.11", "nep.fu.12", "nep.fu.13" ,  "nep.fu.14", "nep.fu.15",  "nep.fu.16", "nep.fu.17", 
              "nep.fu.19", "nep.fu.2021",  "nep.fu.22",  "nep.fu.2324",  "nep.fu.2627", "nep.fu.2829",  "nep.fu.3-4" , 
              "nep.fu.5" ,  "nep.fu.6" ,  "nep.fu.7",  "nep.fu.8" , "nep.fu.9"  )

# Fishing mortality reference points changes data 
data_F <- read.csv("database_diff_F_17April.csv")
## Event filtering for F
redondeo <- c("bss.27.8ab 2019" ,  "cod.21.1 2019"    , "cod.27.24-32 2011", "cod.27.6a 2017" ,   "cod.27.7e-k 2017" , "her.27.nirs 2018" , "ldb.27.8c9a 2018" , "meg.27.8c9a 2013" , "meg.27.8c9a 2014" , "meg.27.8c9a 2018",  "ple.27.7a 2018"   , "sol.27.7e 2017"   ,"sol.27.7e 2019"  ,  "sol.27.7fg 2017")
rts <- c("ple.27.7d 2014" ,  "ple.27.7e 2018"  , "ple.27.7e 2019"  , "ple.27.7h-k 2019", "reb.2127.dp 2019" ,"sol.27.7h-k 2019")
mixed <- c("ank.27.8c9a 2012", "pra.27.3a4a 2015", "ple.27.7d 2015", "sol.27.7d 2019" , "ple.27.7e 2015" , "her.27.6a7bc 2017")
## Substitutions
data_F[data_F$newname.x == "sol.27.7d" & data_F$AssessmentYear.x == 2019, 4] <- 0.192 
data_F[data_F$newname.x == "ple.27.7e" & data_F$AssessmentYear.x == 2015, 21] <- 0.15
data_F[data_F$newname.x == "her.27.6a7bc" & data_F$AssessmentYear.x == 2017, 21] <- 0.25 


# Biomass reference points changes data   
data_B <- read.csv("database_diff_B_17April.csv")
## Event filtering for B
relatives.b <- c("ank.27.8c9a", "pra.27.3a4a", "rng.27.5b6712b")
redondeo.b <-c("her.27.nirs 2018" ,     "hom.27.2a4a5b6a7a-ce-k8 2018" ,"meg.27.8c9a 2014", "ple.27.21-23 2017" , "ple.27.7a 2018", "pok.27.3a46 2018" , "pok.27.3a46 2019","sol.27.7e 2017"    ,"sol.27.7e 2019"  ,"whg.27.6a 2018")
rts.b <- c( "ple.27.7e 2019" ,  "ple.27.7e 2018" ,  "ple.27.7h-k 2019", "reb.2127.dp 2019", "sol.27.7h-k 2019")
## Substitutions
data_B[data_B$newname.x == "sol.27.7d" & data_B$AssessmentYear.x == 2019, 4] <- 15072 
data_B[data_B$newname.x == "ple.27.7e" & data_B$AssessmentYear.x == 2015, 4] <- 2400



# # # # # # # # Change Database creation # # # # # # # 

# Decomposition of status database
## For F complete timeseries
data_change <- data_F %>%
  filter(!is.na(FFMSY.x), !is.na(FFMSY.y))%>%
  filter(change.x ==TRUE)%>%
  filter(!newname.x %in% nephrops)%>%
  mutate(newname.x =droplevels(newname.x))%>%
  group_by(newname.x, AssessmentYear.x)%>%
  mutate(event = paste(newname.x, AssessmentYear.x))%>%
  filter(!event %in% redondeo)%>%
  filter(!event %in% rts)%>%
  mutate(reldiff = (FFMSY.x-FFMSY.y)/FFMSY.y)%>%
  summarise(n = n(), 
            mean.pos = mean(FFMSY.x, na.rm = TRUE),
            mean.pre = mean(FFMSY.y, na.rm = TRUE),
            pos = unique(FMSY.x), 
            pre = unique(FMSY.y), 
            change = (unique(FMSY.x)-unique(FMSY.y))/unique(FMSY.y),
            gamma = sum(F.x, na.rm = TRUE)/sum(F.y, na.rm = TRUE), 
            delta = unique(FMSY.x)/ unique(FMSY.y),
            y = gamma/delta - 1)%>%
  mutate(event = paste(newname.x, AssessmentYear.x))

## For F 5 las years
data_change <- data_F %>%
  filter(!is.na(FFMSY.x), !is.na(FFMSY.y))%>%
  filter(change.x ==TRUE)%>%
  filter(!newname.x %in% nephrops)%>%
  mutate(newname.x =droplevels(newname.x))%>%
  group_by(newname.x, AssessmentYear.x)%>% 
  filter(Year>(max(Year)-5)) %>%
  mutate(reldiff = (FFMSY.x-FFMSY.y)/FFMSY.y)%>%
  summarize(n_c = n(), 
            mean.pos_c = mean(FFMSY.x, na.rm = TRUE),
            mean.pre_c = mean(FFMSY.y, na.rm = TRUE),
            pos = mean(FMSY.x), 
            pre = mean(FMSY.y), 
            gamma_c = sum(F.x, na.rm = TRUE)/sum(F.y, na.rm = TRUE), 
            delta_c = mean(FMSY.x)/ mean(FMSY.y),
            y_c = gamma_c/delta_c - 1)%>%
  ungroup()%>%
  mutate(event = paste(newname.x, AssessmentYear.x))%>%
  filter(!event %in% redondeo)%>%
  filter(!event %in% rts)%>%
  full_join(data_change)


## For SSB complete timeseries
data_change <- data_B %>%
  filter(!is.na(BBMSY.x), !is.na(BBMSY.y))%>%
  filter(change.x ==TRUE)%>%
  filter(!newname.x %in% nephrops)%>%
  mutate(newname.x =droplevels(newname.x))%>%
  group_by(newname.x, AssessmentYear.x)%>%
  mutate(event = paste(newname.x, AssessmentYear.x))%>%
  filter(!event %in% redondeo.b)%>%
  filter(!event %in% rts.b)%>%
  filter(!newname.x %in% relatives.b)%>%
  mutate(reldiff = (BBMSY.x-BBMSY.y)/BBMSY.y)%>%
  summarise(n_b = n(), 
            mean.pos_b = mean(BBMSY.x, na.rm = TRUE),
            mean.pre_b = mean(BBMSY.y, na.rm = TRUE),
            pos_b = unique(MSYBtrigger.x), 
            pre_b = unique(MSYBtrigger.y), 
            change_b= (unique(MSYBtrigger.x)-unique(MSYBtrigger.y))/unique(MSYBtrigger.y),
            gamma_b = sum(SSB.x, na.rm = TRUE)/sum(SSB.y, na.rm = TRUE), 
            delta_b = unique(MSYBtrigger.x)/ unique(MSYBtrigger.y),
            y_b = gamma_b/delta_b - 1)%>%
  mutate(event = paste(newname.x, AssessmentYear.x))%>%
  full_join(data_change)

## For SSB last 5 years timeseries
data_change <- data_B %>%
  filter(!is.na(BBMSY.x), !is.na(BBMSY.y))%>%
  filter(change.x ==TRUE)%>%
  filter(!newname.x %in% nephrops)%>%
  filter(!newname.x %in% relatives.b)%>%
  mutate(newname.x =droplevels(newname.x))%>%
  group_by(newname.x, AssessmentYear.x)%>% 
  filter(Year>(max(Year)-5)) %>%
  mutate(reldiff = (BBMSY.x-BBMSY.y)/BBMSY.y)%>%
  summarise(n_b_c = n(), 
            mean.pos_b_c = mean(BBMSY.x, na.rm = TRUE),
            mean.pre_b_c = mean(BBMSY.y, na.rm = TRUE),
            pos_b_c = unique(MSYBtrigger.x), 
            pre_b_c = unique(MSYBtrigger.y), 
            gamma_b_c = sum(SSB.x, na.rm = TRUE)/sum(SSB.y, na.rm = TRUE), 
            delta_b_c = unique(MSYBtrigger.x)/ unique(MSYBtrigger.y),
            y_b_c = gamma_b_c/delta_b_c - 1)%>%
  ungroup()%>%
  mutate(event = paste(newname.x, AssessmentYear.x))%>%
  filter(!event %in% redondeo.b)%>%
  filter(!event %in% rts.b)%>%
  full_join(data_change)%>%
  arrange(event)

write.csv(data_change, "data_gdy.csv")


# # # # # # # # Plots # # # # # # # # # # # # # 

# Changes in reference points for stock assessments (Fig. 1)

mixed <- c("ank.27.8c9a 2012", "pra.27.3a4a 2015","ple.27.7e 2015")
labels<-read.csv("labels.csv")
data_change<-cbind(labels, data_change)

tmpF <- data_change%>%
  filter(!event %in% mixed)%>%
  select( AssessmentYear.x, newname.x,  change, Abbreviated.name)%>%
  filter(!is.na(change))%>%
  mutate(type = "FMSY")
tmpB <- data_change%>%
  filter(!event %in% mixed)%>%
  mutate(change = change_b)%>%
  select( AssessmentYear.x, newname.x,  change, Abbreviated.name)%>%
  filter(!is.na(change))%>%
  mutate(type = "MSYBtrigger")

dataT <- rbind(tmpF, tmpB)

titles <- c('FMSY'="F[MSY]", 'MSYBtrigger'="MSY*B[trigger]")
theme_set(theme_classic())

dataT %>%
  # mutate(newname.x = fct_reorder(newname.x, reldiff, max, .desc = FALSE)) %>%
  mutate(newname.x = fct_relevel(newname.x, rev)) %>%
  mutate(la = Abbreviated.name)%>%
  mutate(la = fct_relevel(la, rev)) %>%
  ggplot(aes(x = AssessmentYear.x, y = la, fill = change))+
  geom_tile()+
  scale_fill_gradient2(low = "#009E73", midpoint = 0,mid =  "white",  high = "#E69F00" ,limits = c(-1.3, 1.3) ,  na.value = "#E69F00" )+
  labs(x ="Assessment Year", y = "")+
  scale_x_continuous(breaks = c(2012,2013, 2014, 2015, 2016, 2017, 2018, 2019))+
  geom_text(aes(label = paste(c(round(change, 2))*100, "%")), size = 2.5)+
  theme(legend.position = "", panel.grid.major.y = element_line(colour = "lightgrey", linetype = 3, size = .3))+
  facet_wrap(vars(type), labeller = labeller(type = as_labeller(titles, label_parsed)))

  

# Timelines of changes in status (Extended materia Fig. SI1, SI2)

data_F %>%
  filter(!is.na(FMSY.x), !is.na(FMSY.y))%>%
  filter(change.x ==TRUE)%>%
  filter(!newname.x %in% nephrops)%>%
  mutate(newname.x =droplevels(newname.x))%>%
  group_by(newname.x, AssessmentYear.x)%>%
  mutate(event = paste(newname.x, AssessmentYear.x))%>%
  filter(!event %in% redondeo)%>%
  filter(!event %in% rts)%>%
  filter(!event %in% mixed)%>%
  mutate(reldiff = (FFMSY.x-FFMSY.y)/FFMSY.y)%>%
  ggplot(aes(Year,reldiff))+
  geom_line(aes(colour = factor(AssessmentYear.x)))+
  geom_hline(yintercept = 0, linetype = 2, colour = "lightgrey")+
  facet_wrap(vars(newname.x))+
  scale_color_viridis_d()+
  scale_y_continuous(labels = scales::percent)+
  theme_classic()+
  labs(x="",y = expression("Changes in F/F"[MSY]), colour = "Assessment\nYear" )

data_B %>%
  filter(!is.na(MSYBtrigger.x), !is.na(MSYBtrigger.y))%>%
  filter(change.x ==TRUE)%>%
  filter(!newname.x %in% nephrops)%>%
  mutate(newname.x =droplevels(newname.x))%>%
  group_by(newname.x, AssessmentYear.x)%>%
  mutate(event = paste(newname.x, AssessmentYear.x))%>%
  filter(!event %in% redondeo.b)%>%
  filter(!event %in% rts.b)%>%
  filter(!event %in% relatives.b)%>%
  mutate(reldiff = (BBMSY.x-BBMSY.y)/BBMSY.y)%>%
  ggplot(aes(Year,reldiff))+
  geom_line(aes(colour = factor(AssessmentYear.x)))+
  geom_hline(yintercept = 0, linetype = 2, colour = "lightgrey")+
  facet_wrap(vars(newname.x))+
  scale_color_viridis_d()+
  scale_y_continuous(labels = scales::percent)+
  theme_classic()+
  labs(x="",y = expression("Changes in SSB/MSYB"[trigger]), colour = "Assessment\nYear" )


# Timelines of status  (Figure SI3, SI4)
data_F %>%
  filter(!is.na(FMSY.x), !is.na(FMSY.y))%>%
  filter(change.x ==TRUE)%>%
  filter(!newname.x %in% nephrops)%>%
  mutate(newname.x =droplevels(newname.x))%>%
  group_by(newname.x, AssessmentYear.x)%>%
  mutate(event = paste(newname.x, AssessmentYear.x))%>%
  filter(!event %in% redondeo)%>%
  filter(!event %in% rts)%>%
  filter(!event %in% mixed)%>%
  mutate(reldiff = (FFMSY.x-FFMSY.y)/FFMSY.y)%>%
  ggplot(aes(Year,FFMSY.x))+
  geom_line(aes(colour = factor(AssessmentYear.x)))+
  geom_line(aes(Year, FFMSY.y, colour = factor(AssessmentYear.y)))+
  geom_hline(yintercept = 1, linetype = 2, colour = "lightgrey")+
  facet_wrap(vars(newname.x))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x="",y = expression("F/F"[MSY]), colour = "Assessment\nYear" )

data_B %>%
  filter(!is.na(MSYBtrigger.x), !is.na(MSYBtrigger.y))%>%
  filter(change.x ==TRUE)%>%
  filter(!newname.x %in% nephrops)%>%
  mutate(newname.x =droplevels(newname.x))%>%
  group_by(newname.x, AssessmentYear.x)%>%
  mutate(event = paste(newname.x, AssessmentYear.x))%>%
  filter(!event %in% redondeo.b)%>%
  filter(!event %in% rts.b)%>%
  filter(!event %in% relatives.b)%>%
  mutate(reldiff = (BBMSY.x-BBMSY.y)/BBMSY.y)%>%
  ggplot(aes(Year,BBMSY.x))+
  geom_line(aes(colour = factor(AssessmentYear.x)))+
  geom_line(aes(Year, BBMSY.y, colour = factor(AssessmentYear.y)))+
  geom_hline(yintercept = 1, linetype = 2, colour = "lightgrey")+
  facet_wrap(vars(newname.x))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x="",y = expression("SSB/MSYB"[trigger]), colour = "Assessment\nYear" )


# Example timelines of changes in status (Fig. 3)

stcks <- c("pil.27.8c9a", "cod.27.6a", "sol.27.20-24")

stock_labs <-c("pil.27.8c9a" = "Cantabrian Sea and Atlantic\nIberian waters sardine", "cod.27.6a" = "West of Scotland cod", "sol.27.20-24" = "Skagerrak and Kattegat\n western Baltic Sea sole")
e1 <- data_F %>%
  filter(change.x == TRUE)%>%
  filter(newname.x %in% stcks)%>%
  group_by(newname.x, AssessmentYear.x)%>%
  mutate(event = paste(newname.x, AssessmentYear.x))%>%
  filter(!event %in% redondeo)%>%
  filter(!event %in% rts)%>%
  filter(!event %in% mixed)%>%
  mutate(reldiff = (FFMSY.x-FFMSY.y)/FFMSY.y)%>%
  ggplot(aes(Year,FFMSY.x))+
  geom_line(aes(colour = factor(AssessmentYear.x)))+
  geom_line(aes(Year, FFMSY.y, colour = factor(AssessmentYear.y)))+
  geom_hline(yintercept = 1, linetype = 2, colour = "lightgrey")+
  facet_wrap(vars(newname.x), labeller = as_labeller(stock_labs))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x="",y = expression("F/F"[MSY]), colour = "Assessment\nYear (y)" )



stcks <- c("hke.27.3a46-8abd", "her.27.irls","had.27.6b")

stock_labs <- c("hke.27.3a46-8abd" = "Greater North Sea, Celtic Seas,\nand the northern Bay of Biscay hake", "her.27.irls" = "Irish Sea, Celtic Sea, and\nsouthwest of Ireland herring","had.27.6b" = "Rockhall Haddock" )

e2 <- data_B %>%
  filter(change.x==TRUE)%>%
  filter(newname.x %in% stcks)%>%
  group_by(newname.x, AssessmentYear.x)%>%
  mutate(event = paste(newname.x, AssessmentYear.x))%>%
  filter(!event %in% redondeo.b)%>%
  filter(!event %in% rts.b)%>%
  filter(!event %in% relatives.b)%>%
  mutate(reldiff = (BBMSY.x-BBMSY.y)/BBMSY.y)%>%
  ungroup()%>%
  mutate(newname.x = fct_relevel(newname.x, rev))%>%
  ggplot(aes(Year,BBMSY.x))+
  geom_line(aes(colour = factor(AssessmentYear.x)))+
  geom_line(aes(Year, BBMSY.y, colour = factor(AssessmentYear.y)))+
  geom_hline(yintercept = 1, linetype = 2, colour = "lightgrey")+ 
  facet_wrap(vars(newname.x),  labeller = as_labeller(stock_labs))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(x="",y = expression("SSB/MSYB"[trigger]), colour = "Assessment\nYear" )

ggarrange(e1,e2, nrow = 2,common.legend = TRUE, legend = "right")    


# Mean status before and after at changes in reference points and difference distribution (Fig. 4)

data_m <-data.frame (rbind(cbind(mstatus.y = data_change$mean.pre, mstatus.x = data_change$mean.pos, def = rep("a2. Change in complete time-series\naverage relative fishing mortality", nrow(data_change))),
cbind(mstatus.y = data_change$mean.pre_c, mstatus.x = data_change$mean.pos_c, def = rep("a1. Change in recent average\nrelative fishing mortality", nrow(data_change))),
cbind(mstatus.y = data_change$mean.pre_b, mstatus.x = data_change$mean.pos_b, def = rep("b2. Change in complete time-series\naverage relative biomass", nrow(data_change))),
cbind(mstatus.y = data_change$mean.pre_b_c, mstatus.x = data_change$mean.pos_b_c, def = rep("b1. Change in recent average\nrelative biomass", nrow(data_change))))
)
data_m$mstatus.y<-as.numeric(as.character(data_m$mstatus.y))
data_m$mstatus.x<-as.numeric(as.character(data_m$mstatus.x))


data_m <- data_m%>%
  filter(!is.na(mstatus.y))%>%
  group_by(def)%>%
  mutate(median.x = median(mstatus.x),
         median.y = median(mstatus.y))%>%
  mutate(d = mstatus.x- mstatus.y,
         median = median(d))

p1 <- data_m%>%
  ggplot(aes(x = .5, xend = 1.5, y = mstatus.y, yend = mstatus.x), color = "grey")+
  geom_segment(  alpha = .3, color = "grey")+
  geom_point( alpha = .3, color = "grey")+
  geom_point(aes (1.5, mstatus.x), alpha = .3, color = "grey")+
  geom_point(aes (0.5, median.y), alpha = .3, color = "black")+
  geom_point(aes (1.5, median.x), alpha = .3, color = "black")+
  geom_segment(aes(y = median.y, yend = median.x), color = "black", linetype = 2, size = .35)+
  geom_hline(yintercept = 1, linetype = 3, colour = "grey")+
  scale_y_log10()+
  labs(y ="Average status", x = "")+
  scale_x_continuous(limits=c(0,2), breaks = c(.5,1.5), labels = c("Before", "After"), position = "bottom")+
  facet_wrap(vars(def), nrow = 1, strip.position = "top")+
  theme(legend.position = "", panel.grid = element_blank(), strip.background = element_blank(), strip.placement = "outside")


p2 <- data_m %>%
ggplot(aes(d))+
  geom_histogram( breaks = c(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8), position = "identity", fill = "grey")+
  theme_light()+
  scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8))+
  geom_vline(aes(xintercept = median), colour = "black", linetype = 2)+
  theme(panel.grid = element_blank(), strip.background.x = element_rect(fill = "white"))+
  labs(x = "Difference (after-before)", y = "Frequency")+
  facet_wrap(vars(def), nrow = 1) 

ggarrange(p1, p2,  nrow = 2, heights = c(2.25,1))  


## pairwise t-test

datamm1 <- data_m %>%
  gather(status, value, mstatus.x:mstatus.y)%>%
  filter(def == "a2. Change in long-term average\nrelative fishing mortality")

datamm2 <- data_m %>%
  gather(status, value, mstatus.x:mstatus.y)%>%
  filter(def == "a1. Change in recent average\nrelative fishing mortality")

datamm3 <- data_m %>%
  gather(status, value, mstatus.x:mstatus.y)%>%
  filter(def == "b1. Change in recent average\nrelative biomass")

datamm4 <- data_m %>%
  gather(status, value, mstatus.x:mstatus.y)%>%
  filter(def == "b2. Change in long-term average\nrelative biomass")

pairwise.t.test(datamm1$value, datamm1$status, p.adj = "none")
pairwise.t.test(datamm2$value, datamm2$status, p.adj = "none")
pairwise.t.test(datamm3$value, datamm3$status, p.adj = "none")
pairwise.t.test(datamm4$value, datamm4$status, p.adj = "none")



# Change in sustainability status decomposition (Fig. 5)


## Filtering
trans <- c("sol.27.7d 2019" , "ple.27.7d 2015" , "her.27.6a7bc 2017", "ple.27.7e 2015")

datap<- data_change%>%  
  mutate(lab = labels$Ind)%>%
  filter(!event %in% mixed)%>%
  filter(!event %in% trans)

##### only points plot

expected <- data.frame (d = seq(0.2, 2.5, by  = 0.01), g =1/seq(0.2, 2.5, by  = 0.01)-1)


efe <- ggplot(data = datap, aes(delta, log1p(y))) +
  geom_vline(xintercept = 1, linetype = 3, color = "lightgrey")+
  geom_hline(yintercept = 0, linetype = 3, color = "lightgrey")+
  geom_line(data = expected, aes(d, log1p(g)), linetype = 2, color = "darkgrey", size = 1, alpha = 0.7)+
  geom_point(size = 0.5)+
  scale_colour_viridis_d()+
  scale_y_continuous(labels = scales::percent)+
  labs( y = expression("Change in F/F"[MSY]), x =expression(paste(delta, " -Proportional change in F"[MSY])), colour ="Assessment\nYear")+
  theme(panel.grid = element_blank())


ebe <- ggplot(data = datap, aes(delta_b, y_b)) +
  geom_vline(xintercept = 1, linetype = 3, color = "lightgrey")+
  geom_hline(yintercept = 0, linetype = 3, color = "lightgrey")+
  geom_line(data = expected, aes(d, g), linetype = 2, color = "darkgrey", size = 1, alpha = 0.7)+
  geom_point(size = 0.5)+
  scale_colour_viridis_d()+
  scale_y_continuous(labels = scales::percent)+
  labs( y = expression("Change in SSB/MSYB"[trigger]), x =expression(paste(delta, " -Proportional change in MSYB"[trigger])), colour ="Assessment\nYear")+
  theme(panel.grid = element_blank())


expected <- data.frame (ga = seq(0.5, 1.65, by  = 0.01), de = seq(0.5, 1.65, by  = 0.01)/1-1)

efeg <- ggplot(data = datap, aes(gamma, y)) +
  geom_vline(xintercept = 1, linetype = 3, color = "lightgrey")+
  geom_hline(yintercept = 0, linetype = 3, color = "lightgrey")+
  geom_line(data = expected, aes(ga, de), linetype = 2, color = "darkgrey", size = 1, alpha = 0.7)+
  geom_point(size = 0.5)+
  scale_colour_viridis_d()+
  scale_y_continuous(labels = scales::percent)+
  labs(y = expression("Change in F/F"[MSY]), x =expression(paste(gamma, " -Proportional change in F")), colour ="Assessment\nYear")+
  theme(panel.grid = element_blank())

ebeg <- ggplot(data = datap, aes(gamma_b, y_b)) +
  geom_vline(xintercept = 1, linetype = 3, color = "lightgrey")+
  geom_hline(yintercept = 0, linetype = 3, color = "lightgrey")+
  geom_line(data =  expected, aes(ga, de), linetype = 2, color = "darkgrey", size = 1, alpha = 0.7)+
  geom_point(size = 0.5)+
  scale_colour_viridis_d()+
  scale_y_continuous(labels = scales::percent)+
  labs(y = expression("Change in SSB/MSYB"[trigger]), x =expression(paste(gamma, " -Proportional change in SSB")), colour ="Assessment\nYear")+
  theme(panel.grid = element_blank())

ggarrange(efe, ebe, efeg, ebeg, nrow = 2, ncol = 2, common.legend = TRUE, legend = "right")


# Marginal relationship between average proportional change in status and proportional change in reference point for 5 recent years (SI figure 5) 
expected <- data.frame (d = seq(0.2, 2.5, by  = 0.01), g =1/seq(0.2, 2.5, by  = 0.01)-1)

efe <- datap %>%
  ggplot(aes(delta_c, y_c)) +
  geom_vline(xintercept = 1, linetype = 3, color = "lightgrey")+
  geom_hline(yintercept = 0, linetype = 3, color = "lightgrey")+
  geom_line(data = expected, aes(d, g), linetype = 2, color = "darkgrey", size = 1, alpha = 0.7)+
  geom_point(size = 0.5)+
  scale_colour_viridis_d()+
  scale_y_continuous(labels = scales::percent)+
  labs( y = expression("Change in F/F"[MSY]), x =expression(paste(delta, " -Proportional change in F"[MSY])), colour ="Assessment\nYear")+
  theme(panel.grid = element_blank())


ebe <- datap %>%
  ggplot(aes(delta_b_c, y_b_c)) +
  geom_vline(xintercept = 1, linetype = 3, color = "lightgrey")+
  geom_hline(yintercept = 0, linetype = 3, color = "lightgrey")+
  geom_line(data = expected, aes(d, g), linetype = 2, color = "darkgrey", size = 1, alpha = 0.7)+
  geom_point(size = 0.5)+
  scale_colour_viridis_d()+
  scale_y_continuous(labels = scales::percent)+
  labs( y = expression("Change in SSB/MSYB"[trigger]), x =expression(paste(delta, " -Proportional change in MSYB"[trigger])), colour ="Assessment\nYear")+
  theme(panel.grid = element_blank())


expected <- data.frame (ga = seq(0.5, 1.65, by  = 0.01), de = seq(0.5, 1.65, by  = 0.01)/1-1)

efeg <- datap %>%
  ggplot(aes(gamma_c, y_c)) +
  geom_vline(xintercept = 1, linetype = 3, color = "lightgrey")+
  geom_hline(yintercept = 0, linetype = 3, color = "lightgrey")+
  geom_line(data = expected, aes(ga, de), linetype = 2, color = "darkgrey", size = 1, alpha = 0.7)+
  geom_point(size = 0.5)+
  scale_colour_viridis_d()+
  scale_y_continuous(labels = scales::percent)+
  labs(y = expression("Change in F/F"[MSY]), x =expression(paste(gamma, " -Proportional change in F")), colour ="Assessment\nYear")+
  theme(panel.grid = element_blank())

ebeg <- datap %>%
  ggplot(aes(gamma_b_c, y_b_c)) +
  geom_vline(xintercept = 1, linetype = 3, color = "lightgrey")+
  geom_hline(yintercept = 0, linetype = 3, color = "lightgrey")+
  geom_line(data =  expected, aes(ga, de), linetype = 2, color = "darkgrey", size = 1, alpha = 0.7)+
  geom_point(size = 0.5)+
  scale_colour_viridis_d()+
  scale_y_continuous(labels = scales::percent)+
  labs(y = expression("Change in SSB/MSYB"[trigger]), x =expression(paste(gamma, " -Proportional change in SSB")), colour ="Assessment\nYear")+
  theme(panel.grid = element_blank())

ggarrange(efe, ebe, efeg, ebeg, nrow = 2, ncol = 2, common.legend = TRUE, legend = "right")


