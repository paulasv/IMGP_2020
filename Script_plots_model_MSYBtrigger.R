##--------------------------------
## Plots and model with covariates
## for changes in MSYBtrigger
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
library(reshape2)


data <- read.csv("data_gdy.csv")


###  Biomass reference point changes
## Database for B reference point
datab <- data %>%
  filter(!is.na(y_b))

## Add the covariates to the database
data <- read.csv("covariates_B.csv")
dos <- data %>%
  mutate(event = as.character(event))%>%
  select(event, event:Revision_Assessment_M)

datab <- data.frame(inner_join(datab, dos))


### Frequency of occurence
datab%>%
  filter(Redondeo != 1,
  Revision_RP_torelative !=1,
   RP_relativetotimeseries!=1)%>%
  select(newname.x:year, Revision_Assessesment_type:Revision_Assessment_M)%>%
  gather(revision, occur, Revision_Assessesment_type:Revision_Assessment_M)%>%
  filter(occur ==1) %>%
  group_by(revision)%>%
  summarize(n = n(), per = n()/78) %>%
  arrange(desc(n))



### Density plots by covariate

library(ggridges)

datab %>%
  filter(Redondeo != 1,
         Revision_RP_torelative !=1,
         RP_relativetotimeseries!=1)%>%
  select(newname.x:event,delta_b, Revision_Assessesment_type, Revision_RP_Blim_tb, Revision_RP_Bpa_tb, Revision_RP_MSYBtrigger_tb, Revision_Assessment_Stock_definition:Revision_Assessment_M)%>%
  gather(modifications, value, Revision_Assessesment_type:Revision_Assessment_M)%>%
  filter(value!=0)%>%
  group_by(newname.x, AssessmentYear.x)%>%
  mutate(n = n(), m= mean(delta_b, na.rm = TRUE))%>%
  ungroup()%>%
  group_by(modifications)%>%
  mutate(nmod = n())%>%
  ungroup()%>%
  mutate(modifications = fct_reorder(modifications, nmod, .desc = TRUE))%>%
  ggplot(aes(delta_b,  modifications, fill = ..x..))+
  geom_density_ridges_gradient(scale = 0.85, jittered_points = TRUE, point_size = .5, point_alpha = 1, alpha = 0.3, show.legend = FALSE)+
  scale_fill_gradient2(low = "#009E73", midpoint = 1,mid =  "lemonchiffon1",  high = "#E69F00" ,limits = c(-.5, 2.5) ,  na.value = "#E69F00" )+
  theme_ridges()+
  theme(axis.text.y = element_text(size = 11))+ 
  theme( panel.grid = element_blank(), strip.background.x = element_rect(fill = "white"))+
  labs(y = "", x =  expression(paste(delta, "- Proportional change in MSYB"[trigger])))



## Plotting probabilities of co-occurrence of revisions relative to F 
##### Ghassen

data2 <- datab %>%
  filter(Redondeo != 1,
         Revision_RP_torelative !=1,
         RP_relativetotimeseries!=1)%>%
  select(Revision_Assessesment_type, Revision_RP_Blim_tb, Revision_RP_Bpa_tb,Revision_RP_MSYBtrigger_tb, Revision_Assessment_Stock_definition:Revision_Assessment_M)

#### calculating probabilities
occurence <- apply(data2, 2, sum)
data3 <- data.frame(matrix(nrow = dim(data2)[2], ncol = dim(data2)[2]))

for (i in 1: dim(data2)[2]) { 
  for (j in 1: dim(data2)[2]) {
    a <- data2[,i] + data2[,j]   
    match <- sum(a == 2)
    data3[i,j] <- match/occurence[j]
    print(i)
  }}

### Conecting plot 
mix<-c("ank.27.8c9a 2012", "pra.27.3a4a 2015", "ple.27.7e 2015")
data <- datab %>%
  filter(Redondeo != 1,
         Revision_RP_torelative !=1,
         RP_relativetotimeseries!=1) %>%
  filter(!event %in% mix)%>%
  group_by(Former_MSYBtrigger_)%>%
  mutate(former = mean(delta_b))%>%
  ungroup()%>%
  group_by(Current_MSYBtrigger)%>%
  mutate(current =mean(delta_b))%>%
  ungroup()%>%
  group_by(Former_MSYBtrigger_, Current_MSYBtrigger)%>%
  summarize(n = n(), 
            meand = mean(delta_b, na.rm = TRUE), 
            refy = -mean(former),
            refx = mean(current))%>%
  filter(Former_MSYBtrigger_ != 0 )%>%
  ungroup()%>%
  mutate(refy = rank(refy))%>%
  mutate(refx = rank(refx))
theme_set(theme_minimal())


data%>%
  ggplot(aes(x =1 , xend = 2, y = refy, yend = refx, colour = meand, size =n),show.legend = T)+
  geom_segment()+
  geom_point(aes(x =1 , y = refy), size = 12, colour = "papayawhip", alpha = .7)+
  geom_point(aes(x = 2, y = refx), size = 12, colour = "papayawhip", alpha = .7)+
  scale_colour_gradient2(low = "#009E73", midpoint = 1,mid =  "lemonchiffon1",  high = "#E69F00" ,limits = c(0, 2) ,  na.value = "#E69F00" )+
  geom_text(label=data$Former_MSYBtrigger_, x=rep(1, NROW(data)), colour = "black",  check_overlap = F, hjust=1.05, size=2.5)+
  geom_text(label=data$Current_MSYBtrigger, y = data$refx, x=rep(2, NROW(data)), check_overlap = TRUE,colour = "black", hjust=-0.05, size=2.5)+
  scale_size(breaks = c(1, 2,4,8))+
  theme( panel.grid = element_blank(), strip.background.x = element_rect(fill = "white"),axis.text.y=element_blank())+
  scale_x_continuous(limits=c(0.7,2.2), breaks = c(1,2), labels = c("Previous reference point", "Subsequent reference point"), position = "top")+
  labs( x = "", y = "", colour = expression(paste("Mean ", delta)), size = "n")

#### Plotting
names(data3) <- names(data2)
data3$revision_type <- names(data2)
data3_melt <- melt(data3, id.vars = "revision_type")

B_rev <- ggplot(data3_melt, aes(x=factor(revision_type, level=names(data)) , y=variable, fill = value)) +
  geom_raster() + 
  scale_fill_viridis_c(name = "Probability of \nco-occurence", option = "D") +
  labs(x="A", y="B") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=25, hjust = 1)) 
B_rev
ggsave(B_rev, filename = "Extended_Data_Fig4.png", units = "cm", width = 28, height = 16, dpi = 800)

# # # # # # # # # # #  Model   # # # # # # # # # # # # # #
mix<-c("ank.27.8c9a 2012", "pra.27.3a4a 2015", "ple.27.7e 2015")
library(glmulti)
library(rJava)
library(broom)


theme_set(theme_classic())
data_mod <- datab %>%
  filter(Redondeo != 1,
         Revision_RP_torelative !=1,
         RP_relativetotimeseries!=1) %>%
  filter(!event %in% mix)%>%
  mutate(Revision_Assessesment_type = factor(paste(Former_Assesment_method_, Current_Assesment_method_, sep = "to")),
         Revision_RP_Blim_tb = factor(paste(Former_Blim_, Current_Blim_, sep = "to")),
         Revision_RP_MSYBtrigger_tb = factor(paste(Former_MSYBtrigger_, Current_MSYBtrigger, sep = "to")),
         Revision_RP_Bpa_tb = Revision_RP_Bpa_tb)%>%
  select(delta_b,event,change, Revision_Assessesment_type, Revision_RP_Blim_tb, Revision_RP_MSYBtrigger_tb, Revision_RP_Bpa_tb, Revision_Assessment_Stock_definition:Revision_Assessment_M)%>%
  mutate(x = delta_b)%>%
  ungroup()
tmp<-data_mod[,-c(1, 2,3)]

# Linear model

fit_1way <- glmulti(x ~., data = tmp, fitfunc = lm, crit = aic, method = "h", level = 1, plotty = FALSE, report = FALSE)# Selection

fit_1way@formulas[[1]]
fit.c <- lm(fit_1way@formulas[[1]], tmp)
(res.c<- glance(fit.c))

plot(predict(fit.c), tmp$x)
abline(c(0, 1))

anova(fit.c)


library(lsr)
round(etaSquared(fit.c, anova = TRUE), 2)
dddd<-as.data.frame((etaSquared(fit.c,anova = TRUE)))
dddd$Factor<-row.names(dddd)
mf <- dddd%>%
  mutate(Factor = fct_reorder(Factor, eta.sq))%>%
  ggplot( aes(x = Factor,y= eta.sq,fill=Factor)) +
  geom_col(fill = "grey") +
  geom_text(aes(y = eta.sq, label = scales::percent(eta.sq)), hjust = -.01) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.5)) +
  scale_fill_viridis_d()+
  ylab("Percentage of the variance explained by covariate") +
  xlab("") +
  ggtitle("a")+
  guides(fill=FALSE) +
  theme_classic()+
  coord_flip()
mf


