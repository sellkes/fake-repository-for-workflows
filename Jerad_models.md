#Load data etc...
Monthly <- Kelp_Chl_Monthly
Weekly <- Kelp_Chl_Weekly
library(tidyverse)
library(lme4)
library(glmmTMB)
remove.packages(glmmTMB)
install.packages("glmmTMB", type = "source")

#glm for monthly data all species
monthly_glm_allsp <- glmmTMB(average_value ~ average_chl 
  + Species
  + cape.x
  + (1|SiteCode:cape.x),
  data = Monthly, 
  family = tweedie(link="log"))
  
Anova(monthly_glm_allsp)
summary(monthly_glm_allsp)
library(tidyverse)  

weekly_glm_allsp <- glmmTMB(average_value ~ average_chl 
  + Species
  + cape.x
  + (1|SiteCode:cape.x),
  data = Weekly, 
  family = tweedie(link="log"))
  
  Anova(weekly_glm_allsp)
summary(weekly_glm_allsp)

#Subset data for each sp by week and month

Hedophyllum_Month <- subset(Monthly, Species =="Hedophyllum sessile")
Alaria_Month <- subset(Monthly, Species == "Alaria marginata")
Egregia_Month <- subset(Monthly, Species == "Egregia menziesii")  
Postelsia_Month <- subset(Monthly, Species == "Postelsia palmaeformis")  
Lessoniopsis_Month <- subset(Monthly, Species == "Lessoniopsis littoralis")  
Laminaria_Month <- subset(Monthly, Species == "Laminaria sinclairii")  
  
Hedophyllum_Week <- subset(Weekly, Species =="Hedophyllum sessile")
Alaria_Week <- subset(Weekly, Species == "Alaria marginata")
Egregia_Week <- subset(Weekly, Species == "Egregia menziesii")  
Postelsia_Week <- subset(Weekly, Species == "Postelsia palmaeformis")  
Lessoniopsis_Week <- subset(Weekly, Species == "Lessoniopsis littoralis")  
Laminaria_Week <- subset(Weekly, Species == "Laminaria sinclairii")

#glmm for hedo weekly 

weekly_glm_hedophyllum <- glmmTMB(average_value ~ average_chl 
  + cape.x
  + (1|SiteCode:cape.x),
  data = Hedophyllum_Week, 
  family = tweedie(link="log"))
  
Anova(weekly_glm_hedophyllum)
summary(weekly_glm_hedophyllum)

#glm for hedo monthly 

monthly_glm_hedophyllum <- glmmTMB(average_value ~ average_chl 
  + cape.x
  + (1|SiteCode:cape.x),
  data = Hedophyllum_Month, 
  family = tweedie(link="log"))
  
Anova(monthly_glm_hedophyllum)
summary(monthly_glm_hedophyllum)

#glm for alaria weekly 

weekly_glm_alaria <- glmmTMB(average_value ~ average_chl 
  + cape.x
  + (1|SiteCode:cape.x),
  data = Alaria_Week, 
  family = tweedie(link="log"))
  
Anova(weekly_glm_alaria)
summary(weekly_glm_alaria)

#glm for alaria monthly 

monthly_glm_alaria <- glmmTMB(average_value ~ average_chl 
  + cape.x
  + (1|SiteCode:cape.x),
  data = Alaria_Month, 
  family = tweedie(link="log"))
  
Anova(monthly_glm_alaria)
summary(monthly_glm_alaria)

#glm for egregia weekly 

weekly_glm_egregia <- glmmTMB(average_value ~ average_chl 
  + cape.x
  + (1|SiteCode:cape.x),
  data = Egregia_Week, 
  family = tweedie(link="log"))
  
Anova(weekly_glm_egregia)
summary(weekly_glm_egregia)

#glm for egregia monthly 

monthly_glm_egregia <- glmmTMB(average_value ~ average_chl 
  + cape.x
  + (1|SiteCode:cape.x),
  data = Egregia_Month, 
  family = tweedie(link="log"))
  
Anova(monthly_glm_egregia)
summary(monthly_glm_egregia)

#glm for postelsia weekly 

weekly_glm_postelsia <- glmmTMB(average_value ~ average_chl 
  + cape.x
  + (1|SiteCode:cape.x),
  data = Postelsia_Week, 
  family = tweedie(link="log"))
  
Anova(weekly_glm_postelsia)
summary(weekly_glm_postelsia)

#glm for postelsia monthly 

monthly_glm_postelsia<- glmmTMB(average_value ~ average_chl 
  + cape.x
  + (1|SiteCode:cape.x),
  data = Postelsia_Month, 
  family = tweedie(link="log"))
  
Anova(monthly_glm_postelsia)
summary(monthly_glm_postelsia)

#glm for lessoniopsis weekly 

weekly_glm_lessoniopsis <- glmmTMB(average_value ~ average_chl 
  + cape.x
  + (1|SiteCode:cape.x),
  data = Lessoniopsis_Week, 
  family = tweedie(link="log"))
  
Anova(weekly_glm_lessoniopsis)
summary(weekly_glm_lessoniopsis)

#glm for lessoniopsis monthly 

monthly_glm_lessoniopsis<- glmmTMB(average_value ~ average_chl 
  + cape.x
  + (1|SiteCode:cape.x),
  data = Lessoniopsis_Month, 
  family = tweedie(link="log"))
  
Anova(monthly_glm_lessoniopsis)
summary(monthly_glm_lessoniopsis)

#glm for laminaria weekly 

weekly_glm_laminaria <- glmmTMB(average_value ~ average_chl 
  + (1|SiteCode),
  data = Laminaria_Week, 
  family = tweedie(link="log"))
  
Anova(weekly_glm_laminaria)
summary(weekly_glm_laminaria)

#glm for laminaria monthly 

monthly_glm_laminaria<- glmmTMB(average_value ~ average_chl 
  + (1|SiteCode),
  data = Laminaria_Month, 
  family = tweedie(link="log"))
  
Anova(monthly_glm_laminaria)
summary(monthly_glm_laminaria)

library(ggplot2)

#hedo monthly
ggplot(data=Hedophyllum_Month, aes(x=average_value, y=average_chl, group=1)) +
 # geom_line(linetype = "dashed")+
  geom_point() +
  geom_smooth(method = 'lm')
  
#postelsia monthly
  ggplot(data=Postelsia_Month, aes(x=average_value, y=average_chl, group=1)) +
 # geom_line(linetype = "dashed")+
  geom_point() +
  geom_smooth(method = 'lm')
  
  
#lessoniopsis monthly
  ggplot(data=Lessoniopsis_Month, aes(x=average_value, y=average_chl, group=1)) +
 # geom_line(linetype = "dashed")+
  geom_point() +
  geom_smooth(method = 'lm')
  
#laminaria monthly
  ggplot(data=Laminaria_Month, aes(x=average_value, y=average_chl, group=1)) +
 # geom_line(linetype = "dashed")+
  geom_point() +
  geom_smooth(method = 'lm')
  
#egregia monthly
  ggplot(data=Egregia_Month, aes(x=average_value, y=average_chl, group=1)) +
 # geom_line(linetype = "dashed")+
  geom_point() +
  geom_smooth(method = 'lm')
  
#alaria monthly
  ggplot(data=Alaria_Month, aes(x=average_value, y=average_chl, group=1)) +
 # geom_line(linetype = "dashed")+
  geom_point() +
  geom_smooth(method = 'lm')
  
  
#hedo weekly
ggplot(data=Hedophyllum_Week, aes(x=average_value, y=average_chl, group=1)) +
 # geom_line(linetype = "dashed")+
  geom_point() +
  geom_smooth(method = 'lm')
  
#postelsia weekly
  ggplot(data=Postelsia_Week, aes(x=average_value, y=average_chl, group=1)) +
 # geom_line(linetype = "dashed")+
  geom_point() +
  geom_smooth(method = 'lm')
  
  
#lessoniopsis weekly
  ggplot(data=Lessoniopsis_Week, aes(x=average_value, y=average_chl, group=1)) +
 # geom_line(linetype = "dashed")+
  geom_point() +
  geom_smooth(method = 'lm')
  
#laminaria weekly
  ggplot(data=Laminaria_Week, aes(x=average_value, y=average_chl, group=1)) +
 # geom_line(linetype = "dashed")+
  geom_point() +
  geom_smooth(method = 'lm')
  
#egregia weekly
  ggplot(data=Egregia_Week, aes(x=average_value, y=average_chl, group=1)) +
 # geom_line(linetype = "dashed")+
  geom_point() +
  geom_smooth(method = 'lm')
  
#alaria monthly
  ggplot(data=Alaria_Week, aes(x=average_value, y=average_chl, group=1)) +
 # geom_line(linetype = "dashed")+
  geom_point() +
  geom_smooth(method = 'lm')