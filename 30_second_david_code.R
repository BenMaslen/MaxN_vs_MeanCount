library(DHARMa)
library(glmmTMB)
library(tidyverse)


goal_4_dat_kf <- read.csv("Kakadu_30_second.csv")
goal_4_dat_tb <- read.csv("Tassie_30_second.csv")

goal_4_dat_tb <- goal_4_dat_tb %>% mutate(Class = as.factor(Class), 
                                          Video_name = as.factor(Video_name),
                                          location = as.factor(location),
                                          block = as.factor(block),
                                          treatment = as.factor(treatment),
                                          bait = as.factor(bait),
                                          Mobility = as.factor(Mobility),
                                          Schooling = as.factor(Schooling))
goal_4_dat_kf <- goal_4_dat_kf %>% mutate(Class = as.factor(Class), 
                                          Video_name = as.factor(Video_name),
                                          river = as.factor(river),
                                          transect = as.factor(transect),
                                          location = as.factor(location),
                                          year = as.factor(year),
                                          Mobility = as.factor(Mobility),
                                          Schooling = as.factor(Schooling))


#Kakadu Fish

kf_avgn_anov_list <- list()
species_kf <- levels(goal_4_dat_kf$Class)


table(test$river:test$location:test$transect)

i=1
goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN") %>%
  with(table(river:location:transect))

#sumcount approach

for (i in c(1:length(species_kf))){
  kf_avgN_mod_full_temp <- glmmTMB(value_sum~year*river+(1|river:location)+(1|river:location:transect),
                                   data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                   family=nbinom2,offset=log(rep(56*2,286)))
  kf_avgn_mod_null_temp <- glmmTMB(value_sum~(1|river:location)+(1|river:location:transect),
                                   data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                   family=nbinom2,offset=log(rep(56*2,286)))
  kf_avgn_anov_list[[i]] <- anova(kf_avgn_mod_null_temp,kf_avgN_mod_full_temp)
  
  rm(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp,kf_avgN_mod_full_temp,kf_avgn_mod_null_temp)
}



tb_avgn_anov_list <- list()
species_tb <- levels(goal_4_dat_tb$Class)


for (i in c(1:length(species_kf))){
  tb_avgN_mod_full_temp <- glmmTMB(value_sum~bait*treatment+location+(1|location:block),
                                   data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="AvgN"),
                                   family=nbinom2,offset=log(rep(42*2,42)))
  tb_avgn_mod_null_temp <- glmmTMB(value_sum~location+(1|location:block),
                                   data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="AvgN"),
                                   family=nbinom2,offset=log(rep(42*2,42)))
  tb_avgn_anov_list[[i]] <- anova(tb_avgn_mod_null_temp,tb_avgN_mod_full_temp)
  
  rm(tb_maxn_mod_full_temp,tb_maxn_mod_null_temp,tb_avgN_mod_full_temp,tb_avgn_mod_null_temp)
}


#tweedie approach


kf_avgn_anov_list <- list()
species_kf <- levels(goal_4_dat_kf$Class)

for (i in c(1:length(species_kf))){
  kf_avgN_mod_full_temp <- glmmTMB(value~year*river+(1|river:location)+(1|river:location:transect),
                                   data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                   family=tweedie)
  kf_avgn_mod_null_temp <- glmmTMB(value~(1|river:location)+(1|river:location:transect),
                                   data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                   family=tweedie)
  kf_avgn_anov_list[[i]] <- anova(kf_avgn_mod_null_temp,kf_avgN_mod_full_temp)
  
  rm(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp,kf_avgN_mod_full_temp,kf_avgn_mod_null_temp)
}


tb_avgn_anov_list <- list()
species_tb <- levels(goal_4_dat_tb$Class)


for (i in c(1:length(species_kf))){
  tb_avgN_mod_full_temp <- glmmTMB(value~bait*treatment+location+(1|location:block),
                                   data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="AvgN"),
                                   family=tweedie)
  tb_avgn_mod_null_temp <- glmmTMB(value~location+(1|location:block),
                                   data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="AvgN"),
                                   family=tweedie)
  tb_avgn_anov_list[[i]] <- anova(tb_avgn_mod_null_temp,tb_avgN_mod_full_temp)
  
  rm(tb_maxn_mod_full_temp,tb_maxn_mod_null_temp,tb_avgN_mod_full_temp,tb_avgn_mod_null_temp)
}


#check residual code e.g:

i=2
tb_avgN_mod_full_temp <- glmmTMB(value~bait*treatment+location+(1|location:block),
                                 data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="AvgN"),
                                 family=tweedie)
sim= simulateResiduals(tb_avgN_mod_full_temp)
resid = residuals(sim, quantileFunction = qnorm, outlierValues = c(-4,4))
plot(resid~predict(tb_avgN_mod_full_temp, re.form = NA,type="response"))




