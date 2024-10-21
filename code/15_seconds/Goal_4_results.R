#Load libraries
library(tidyverse)


#read in data (generated from Goal_1_data_prep)
goal_4_dat_tb <- read_csv("results/60_seconds/goal_1_2_4_dat_tb_long.csv")
goal_4_dat_kf <- read_csv("results/60_seconds/goal_1_2_4_dat_kf_long.csv")

#read in metadata
vid_metadata_tb         <- read_csv("data/BRUV_RUV_metadata.csv")
vid_metadata_kf         <- read_csv("data/KF_video_meta_data.csv")
species_metadata_tb     <- read_csv("data/TB_species_metadata.csv")
species_metadata_kf     <- read_csv("data/KF_species_metadata.csv")
#species_metadata_kf     <- read_csv("data/KF_species_metadata_check.csv")

#clean metadata
colnames(vid_metadata_tb)[1] <- "Video_name"
colnames(vid_metadata_kf)[1] <- "Video_name"


#join with metadata
goal_4_dat_tb <- left_join(goal_4_dat_tb,vid_metadata_tb,by="Video_name")
goal_4_dat_kf <- left_join(goal_4_dat_kf,vid_metadata_kf,by="Video_name")

goal_4_dat_tb <- left_join(goal_4_dat_tb,species_metadata_tb,by="Class")
goal_4_dat_kf <- left_join(goal_4_dat_kf,species_metadata_kf,by="Class")


#convert into factors
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


# levels(goal_4_dat_tb$Video_name)
# goal_4_dat_tb <- goal_4_dat_tb %>% mutate(
#   EDA_temp = case_when(
#     EDA == "yes" ~ "yes",
#     Video_name %in% c("20211116_RUV_SB_B2_EFM_OLD_E663",
#                       "20211217_RUV_SB_B4_Chain_D093",
#                       "20211209_RUV_NWB_B2_Chain_9619") ~ "yes",
#     TRUE ~ "no"
#   )
# )
#Start with EDA

#goal_4_dat_tb <- goal_4_dat_tb %>% filter(EDA=="yes") %>% droplevels()
#goal_4_dat_kf <- goal_4_dat_kf %>% filter(EDA=="yes") %>% droplevels()
goal_4_dat_tb <- goal_4_dat_tb %>% filter(EDA=="no",QC=="PASS") %>% droplevels()
goal_4_dat_kf <- goal_4_dat_kf %>% filter(EDA=="no") %>% droplevels()


#number of non-zero observations
non_zero_comp =goal_4_dat_kf  %>% group_by(Class) %>% summarise(whole_video=sum(value>0 & measure=="MaxN"),
                                                 seconds_30=sum(value>0 & measure=="AvgN"))

non_zero_comp
goal_4_dat_kf %>% filter(measure=="MaxN") %>% group_by(Class) %>% summarise(sum(value>0))
goal_4_dat_kf %>% filter(measure=="MaxN") %>% group_by(Class) %>% summarise(sum(value>0))


#Plotting

#plot params
#maxn_tol = 1
#avgn_tol = 0.001 #half the minimum non-zero - paper 
maxn_tol = 0.5
avgn_tol_kf = min(goal_4_dat_kf$value[goal_4_dat_kf$value!=0&goal_4_dat_kf$measure=="AvgN"])/2 #half the minimum non-zero - paper 
avgn_tol_tb = min(goal_4_dat_tb$value[goal_4_dat_tb$value!=0&goal_4_dat_tb$measure=="AvgN"])/2 #half the minimum non-zero - paper 

#abundant_species_list_kf_dat <- goal_4_dat_kf %>% group_by(Class) %>% summarise(AvgN=mean(AvgN),MaxN=mean(MaxN)) %>% 
#  arrange(AvgN) %>% filter(AvgN > 0.001,MaxN >0.55) 

abundant_species_list_kf_dat <- goal_4_dat_kf %>% group_by(Class) %>% filter(measure=="AvgN") %>%
  summarise(AvgN=mean(value)) %>% 
  arrange(AvgN) %>% filter(AvgN > 0.0014)%>% droplevels()
abundant_species_list_kf <- abundant_species_list_kf_dat$Class

#apply the tol
goal_4_dat_kf$value_tol <- goal_4_dat_kf$value+(goal_4_dat_kf$measure=="AvgN")*avgn_tol_kf +
  (goal_4_dat_kf$measure=="MaxN")*maxn_tol

goal_4_dat_tb$value_tol <- goal_4_dat_tb$value+(goal_4_dat_tb$measure=="AvgN")*avgn_tol_tb +
  (goal_4_dat_tb$measure=="MaxN")*maxn_tol


#analysis
library(DHARMa)
library(glmmTMB)

#Kakadu Fish

kf_maxn_anov_list <- list()
kf_avgn_anov_list <- list()
species_kf <- levels(goal_4_dat_kf$Class)

#change to below structure for full dataset
#kf_joint_glmm_MaxN <- glmmTMB(MaxN~year*river +(1|river:transect)+ (1|river:transect:location),
#                              data=goal_4_dat_kf %>% filter(Class == "Amniataba percoides"),family=nbinom2)
#do likilihood ratio tests for both effects as once
#then do transfrmation and repeat assuming normal to check its not a distribution thing

for (i in c(1:length(species_kf))){
  kf_maxn_mod_full_temp <- glmmTMB(value~year*river+(1|river:location)+(1|river:location:transect),
                                   data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                   family=nbinom2)
  kf_maxn_mod_null_temp <- glmmTMB(value~(1|river:location)+(1|river:location:transect),
                                   data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                   family=nbinom2)
  kf_maxn_anov_list[[i]] <- anova(kf_maxn_mod_null_temp,kf_maxn_mod_full_temp)
  
  kf_avgN_mod_full_temp <- glmmTMB(value~year*river+(1|river:location)+(1|river:location:transect),
                                   data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                   family=tweedie)
  kf_avgn_mod_null_temp <- glmmTMB(value~(1|river:location)+(1|river:location:transect),
                                   data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                   family=tweedie)
  kf_avgn_anov_list[[i]] <- anova(kf_avgn_mod_null_temp,kf_avgN_mod_full_temp)
  
  rm(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp,kf_avgN_mod_full_temp,kf_avgn_mod_null_temp)
}

#note the degree of freedoms are different?
# kf_avgn_anov_list[[1]]
# kf_maxn_anov_list[[1]]
# 
# #check model assumptions
# i=2
# kf_maxn_mod_full_temp <- glmmTMB(value~year*river+(1|river:location)+(1|river:location:transect),
#                                  data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
#                                  family=nbinom2)
# 
# #plot(simulateResiduals(tb_maxn_mod_full_temp))
# sim= simulateResiduals(kf_maxn_mod_full_temp)
# resid = residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
# plot(resid~predict(kf_maxn_mod_full_temp, re.form = NA,type="response"))
# 
# kf_avgN_mod_full_temp <- glmmTMB(value~year*river+(1|river:location)+(1|river:location:transect),
#                                  data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
#                                  family=tweedie)
# 
# #plot(simulateResiduals(tb_avgN_mod_full_temp))
# sim= simulateResiduals(kf_avgN_mod_full_temp)
# resid = residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
# plot(resid~predict(kf_avgN_mod_full_temp, re.form = NA,type="response"))
# 



#extract the test statistics
test_statistics_maxN_kf <- vector()
test_statistics_avgn_kf <- vector()
p_values_avgn_kf <- vector()
p_values_maxn_kf <- vector()

for (i in c(1:length(species_kf))){
  test_statistics_avgn_kf[i] <- kf_avgn_anov_list[[i]]$Chisq[2][1]
  test_statistics_maxN_kf[i] <- kf_maxn_anov_list[[i]]$Chisq[2][1]
  p_values_avgn_kf[i] <- kf_avgn_anov_list[[i]]$`Pr(>Chisq)`[2][1]
  p_values_maxn_kf[i] <- kf_maxn_anov_list[[i]]$`Pr(>Chisq)`[2][1]
}

test_statistic_dat_kf <- tibble(species=species_kf,MaxN=test_statistics_maxN_kf,AvgN=test_statistics_avgn_kf)
p_values_dat_kf <- tibble(species=species_kf,MaxN=p_values_maxn_kf,AvgN=p_values_avgn_kf)


#try and fix the NA's


i=3
kf_maxn_mod_full_temp <- glmmTMB(value~year*river+(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=nbinom1,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
kf_maxn_mod_null_temp <- glmmTMB(value~(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=nbinom1,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
test_statistic_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$Chisq[2][1]
p_values_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$`Pr(>Chisq)`[2][1]


# sim= simulateResiduals(kf_maxn_mod_full_temp)
# resid = residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
# plot(resid~predict(kf_maxn_mod_full_temp, re.form = NA,type="response"))

i=5 #can't fix wiht nbinom - need to try different starting values
kf_maxn_mod_full_temp <- glmmTMB(value~year*river+(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=poisson,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
kf_maxn_mod_null_temp <- glmmTMB(value~(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=poisson)
test_statistic_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$Chisq[2][1]
p_values_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$`Pr(>Chisq)`[2][1]

# 
# sim= simulateResiduals(kf_maxn_mod_full_temp)
# resid = residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
# plot(resid~predict(kf_maxn_mod_full_temp, re.form = NA,type="response"))


#model wont converge
test_statistic_dat_kf$AvgN[i] <- 0
p_values_dat_kf$AvgN[i] <- 1


# sim= simulateResiduals(kf_avgn_mod_full_temp)
# resid = residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
# plot(resid~predict(kf_avgn_mod_full_temp, re.form = NA,type="response"))





i=6 
kf_maxn_mod_full_temp <- glmmTMB(value~year*river+(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=nbinom1,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
kf_maxn_mod_null_temp <- glmmTMB(value~(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=nbinom1,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
test_statistic_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$Chisq[2][1]
p_values_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$`Pr(>Chisq)`[2][1]


# sim= simulateResiduals(kf_maxn_mod_full_temp)
# resid = residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
# plot(resid~predict(kf_maxn_mod_full_temp, re.form = NA,type="response"))

i=7
kf_maxn_mod_full_temp <- glmmTMB(value~year*river+(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
kf_maxn_mod_null_temp <- glmmTMB(value~(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
test_statistic_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$Chisq[2][1]
p_values_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$`Pr(>Chisq)`[2][1]



#convert to sumcount with offset

kf_avgn_mod_full_temp <- glmmTMB(value*56~year*river+(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                 family=poisson,offset=rep(56,286))

kf_avgn_mod_null_temp <- glmmTMB(value*56~(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                 family=poisson,offset=rep(56,286),
                                 control=glmmTMBControl(optimizer=optim,optArgs = list(method="L-BFGS-B")))


test_statistic_dat_kf$AvgN[i] <- anova(kf_avgn_mod_null_temp,kf_avgn_mod_full_temp)$Chisq[2][1]
p_values_dat_kf$AvgN[i] <- anova(kf_avgn_mod_null_temp,kf_avgn_mod_full_temp)$`Pr(>Chisq)`[2][1]




# sim= simulateResiduals(kf_maxn_mod_full_temp)
# resid = residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
# plot(resid~predict(kf_maxn_mod_full_temp, re.form = NA,type="response"))

i=8
kf_maxn_mod_full_temp <- glmmTMB(value~year*river+(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
kf_maxn_mod_null_temp <- glmmTMB(value~(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
test_statistic_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$Chisq[2][1]
p_values_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$`Pr(>Chisq)`[2][1]


# sim= simulateResiduals(kf_maxn_mod_full_temp)
# resid = residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
# plot(resid~predict(kf_maxn_mod_full_temp, re.form = NA,type="response"))

#convert to sumcount with offset

goal_4_dat_kf_temp = goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN")
table(goal_4_dat_kf_temp$value>0,goal_4_dat_kf_temp$year:goal_4_dat_kf_temp$river)




kf_avgn_mod_null_temp <- glmmTMB(value*56~(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                 family=poisson,offset=rep(56,286))

kf_avgn_mod_full_temp <- glmmTMB(value*56~year*river+(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                 family=poisson,offset=rep(56,286),
                                 control=glmmTMBControl(optimizer=optim,optArgs = list(method="L-BFGS-B")))



test_statistic_dat_kf$AvgN[i] <- anova(kf_avgn_mod_null_temp,kf_avgn_mod_full_temp)$Chisq[2][1]
p_values_dat_kf$AvgN[i] <- anova(kf_avgn_mod_null_temp,kf_avgn_mod_full_temp)$`Pr(>Chisq)`[2][1]




# 
# kf_avgn_mod_full_temp <- glmmTMB(value~year*river+(1|river:location)+(1|river:location:transect),
#                                  data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
#                                  family=tweedie,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
# 
# 
# kf_avgn_mod_null_temp <- glmmTMB(value~(1|river:location)+(1|river:location:transect),
#                                  data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
#                                  family=tweedie)
# 
# test_statistic_dat_kf$AvgN[i] <- anova(kf_avgn_mod_null_temp,kf_avgn_mod_full_temp)$Chisq[2][1]
# p_values_dat_kf$AvgN[i] <- anova(kf_avgn_mod_null_temp,kf_avgn_mod_full_temp)$`Pr(>Chisq)`[2][1]


i=9

goal_4_dat_kf_temp = goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN")
goal_4_dat_kf_temp$value
table(goal_4_dat_kf_temp$value>0,goal_4_dat_kf_temp$year:goal_4_dat_kf_temp$river)




kf_avgn_mod_null_temp <- glmmTMB(value*56~(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                 family=poisson,offset=rep(56,286),
                                 control=glmmTMBControl(optimizer=optim,optArgs = list(method="Nelder-Mead")))

kf_avgn_mod_full_temp <- glmmTMB(value*56~year*river+(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                 family=poisson,offset=rep(56,286))



test_statistic_dat_kf$AvgN[i] <- anova(kf_avgn_mod_null_temp,kf_avgn_mod_full_temp)$Chisq[2][1]
p_values_dat_kf$AvgN[i] <- anova(kf_avgn_mod_null_temp,kf_avgn_mod_full_temp)$`Pr(>Chisq)`[2][1]






i=10
kf_maxn_mod_full_temp <- glmmTMB(value~year*river+(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
kf_maxn_mod_null_temp <- glmmTMB(value~(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
test_statistic_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$Chisq[2][1]
p_values_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$`Pr(>Chisq)`[2][1]


#not enough observations
test_statistic_dat_kf$AvgN[i] <- 0
p_values_dat_kf$AvgN[i] <- 1


i=11
kf_maxn_mod_full_temp <- glmmTMB(value~year*river+(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
kf_maxn_mod_null_temp <- glmmTMB(value~(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
test_statistic_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$Chisq[2][1]
p_values_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$`Pr(>Chisq)`[2][1]




i=12
kf_maxn_mod_full_temp <- glmmTMB(value~year*river+(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
kf_maxn_mod_null_temp <- glmmTMB(value~(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
test_statistic_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$Chisq[2][1]
p_values_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$`Pr(>Chisq)`[2][1]


#no data set test stat to 0 and p-value to 1
test_statistic_dat_kf$AvgN[i] <- 0
p_values_dat_kf$AvgN[i] <- 1

i=13


#convert to sumcount with offset

kf_avgn_mod_full_temp <- glmmTMB(value*56~year*river+(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                 family=nbinom2,offset=rep(56,286))


kf_avgn_mod_null_temp <- glmmTMB(value*56~(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                 family=nbinom2,offset=rep(56,286),
                                 start=list(theta=exp(kf_avgn_mod_full_temp$sdr$par.fixed[c(8,9)]),
                                 betad=kf_avgn_mod_full_temp$sdr$par.fixed[c(7)]),
                                 control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))


test_statistic_dat_kf$AvgN[i] <- anova(kf_avgn_mod_null_temp,kf_avgn_mod_full_temp)$Chisq[2][1]
p_values_dat_kf$AvgN[i] <- anova(kf_avgn_mod_null_temp,kf_avgn_mod_full_temp)$`Pr(>Chisq)`[2][1]


i=14
kf_maxn_mod_full_temp <- glmmTMB(value~year*river+(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
kf_maxn_mod_null_temp <- glmmTMB(value~(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
test_statistic_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$Chisq[2][1]
p_values_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$`Pr(>Chisq)`[2][1]


#convert to sumcount with offset

 kf_avgn_mod_full_temp <- glmmTMB(value*56~year*river+(1|river:location)+(1|river:location:transect),
                                  data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                  family=poisson,offset=rep(56,286))
 


kf_avgn_mod_null_temp <- glmmTMB(value*56~(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                 family=poisson,offset=rep(56,286),
                                 control=glmmTMBControl(optimizer=optim,optArgs = list(method="Nelder-Mead")))



test_statistic_dat_kf$AvgN[i] <- anova(kf_avgn_mod_null_temp,kf_avgn_mod_full_temp)$Chisq[2][1]
p_values_dat_kf$AvgN[i] <- anova(kf_avgn_mod_null_temp,kf_avgn_mod_full_temp)$`Pr(>Chisq)`[2][1]


# sim= simulateResiduals(kf_maxn_mod_full_temp)
# resid = residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
# plot(resid~predict(kf_maxn_mod_full_temp, re.form = NA,type="response"))

i=16

#not enough data
test_statistic_dat_kf$AvgN[i] <- 0
p_values_dat_kf$AvgN[i] <- 1


i=17

#convert to sumcount with offset

kf_avgn_mod_full_temp <- glmmTMB(value*56~year*river+(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                 family=poisson,offset=rep(56,286))



kf_avgn_mod_null_temp <- glmmTMB(value*56~(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                 family=poisson,offset=rep(56,286),
                                 control=glmmTMBControl(optimizer=optim,optArgs = list(method="L-BFGS-B")))



test_statistic_dat_kf$AvgN[i] <- anova(kf_avgn_mod_null_temp,kf_avgn_mod_full_temp)$Chisq[2][1]
p_values_dat_kf$AvgN[i] <- anova(kf_avgn_mod_null_temp,kf_avgn_mod_full_temp)$`Pr(>Chisq)`[2][1]


# sim= simulateResiduals(kf_maxn_mod_full_temp)
# resid = residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
# plot(resid~predict(kf_maxn_mod_full_temp, re.form = NA,type="response"))


i=18
kf_maxn_mod_full_temp <- glmmTMB(value~year*river+(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=poisson,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
kf_maxn_mod_null_temp <- glmmTMB(value~(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=poisson,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
test_statistic_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$Chisq[2][1]
p_values_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$`Pr(>Chisq)`[2][1]



i=19
kf_maxn_mod_full_temp <- glmmTMB(value~year*river+(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
kf_maxn_mod_null_temp <- glmmTMB(value~(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
test_statistic_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$Chisq[2][1]
p_values_dat_kf$MaxN[i] <- anova(kf_maxn_mod_full_temp,kf_maxn_mod_null_temp)$`Pr(>Chisq)`[2][1]


# sim= simulateResiduals(kf_maxn_mod_full_temp)
# resid = residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
# plot(resid~predict(kf_maxn_mod_full_temp, re.form = NA,type="response"))

#convert to sumcount with offset

kf_avgn_mod_full_temp <- glmmTMB(value*56~year*river+(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                 family=poisson,offset=rep(56,286))

kf_avgn_mod_null_temp <- glmmTMB(value*56~(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                 family=poisson,offset=rep(56,286),
                                 control=glmmTMBControl(optimizer=optim,optArgs = list(method="L-BFGS-B")))


test_statistic_dat_kf$AvgN[i] <- anova(kf_avgn_mod_null_temp,kf_avgn_mod_full_temp)$Chisq[2][1]
p_values_dat_kf$AvgN[i] <- anova(kf_avgn_mod_null_temp,kf_avgn_mod_full_temp)$`Pr(>Chisq)`[2][1]


i=20

kf_avgn_mod_full_temp <- glmmTMB(value*56~year*river+(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                 family=poisson,offset=rep(56,286))

kf_avgn_mod_null_temp <- glmmTMB(value*56~(1|river:location)+(1|river:location:transect),
                                 data=goal_4_dat_kf %>% filter(Class == species_kf[i],measure=="AvgN"),
                                 family=poisson,offset=rep(56,286),
                                 control=glmmTMBControl(optimizer=optim,optArgs = list(method="Nelder-Mead")))


test_statistic_dat_kf$AvgN[i] <- anova(kf_avgn_mod_null_temp,kf_avgn_mod_full_temp)$Chisq[2][1]
p_values_dat_kf$AvgN[i] <- anova(kf_avgn_mod_null_temp,kf_avgn_mod_full_temp)$`Pr(>Chisq)`[2][1]


#Tassie BRUV


tb_maxn_anov_list <- list()
tb_avgn_anov_list <- list()
species_tb <- levels(goal_4_dat_tb$Class)

#change to below structure for full dataset
#kf_joint_glmm_MaxN <- glmmTMB(MaxN~year*river +(1|river:transect)+ (1|river:transect:location),
#                              data=goal_4_dat_kf %>% filter(Class == "Amniataba percoides"),family=nbinom2)
#do likilihood ratio tests for both effects as once
#then do transfrmation and repeat assuming normal to check its not a distribution thing

for (i in c(1:length(species_kf))){
  tb_maxn_mod_full_temp <- glmmTMB(value~bait*treatment+location+(1|location:block),
                                   data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="MaxN"),
                                   family=nbinom2)
  tb_maxn_mod_null_temp <- glmmTMB(value~location+(1|location:block),
                                   data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="MaxN"),
                                   family=nbinom2)
  tb_maxn_anov_list[[i]] <- anova(tb_maxn_mod_null_temp,tb_maxn_mod_full_temp)
  
  tb_avgN_mod_full_temp <- glmmTMB(value~bait*treatment+location+(1|location:block),
                                   data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="AvgN"),
                                   family=tweedie)
  tb_avgn_mod_null_temp <- glmmTMB(value~location+(1|location:block),
                                   data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="AvgN"),
                                   family=tweedie)
  tb_avgn_anov_list[[i]] <- anova(tb_avgn_mod_null_temp,tb_avgN_mod_full_temp)
  
  rm(tb_maxn_mod_full_temp,tb_maxn_mod_null_temp,tb_avgN_mod_full_temp,tb_avgn_mod_null_temp)
}


# 
# #check model assumptions
# i=1 #cause for concern??
# tb_maxn_mod_full_temp <- glmmTMB(value~bait*treatment+location+(1|location:block),
#                                  data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="MaxN"),
#                                  family=nbinom2)
# 
# #plot(simulateResiduals(tb_maxn_mod_full_temp))
# sim= simulateResiduals(tb_maxn_mod_full_temp)
# resid = residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
# plot(resid~predict(tb_maxn_mod_full_temp, re.form = NA,type="response"))
# 
# i=2
# tb_avgN_mod_full_temp <- glmmTMB(value~bait*treatment+location+(1|location:block),
#                                  data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="AvgN"),
#                                  family=tweedie)
# #plot(simulateResiduals(tb_avgN_mod_full_temp))
# sim= simulateResiduals(tb_avgN_mod_full_temp)
# resid = residuals(sim, quantileFunction = qnorm, outlierValues = c(-4,4))
# plot(resid~predict(tb_avgN_mod_full_temp, re.form = NA,type="response"))



#extract the test statistics
test_statistics_maxN_tb <- vector()
test_statistics_avgn_tb <- vector()
p_values_avgn_tb <- vector()
p_values_maxn_tb <- vector()

for (i in c(1:length(species_tb))){
  test_statistics_avgn_tb[i] <- tb_avgn_anov_list[[i]]$Chisq[2][1]
  test_statistics_maxN_tb[i] <- tb_maxn_anov_list[[i]]$Chisq[2][1]
  p_values_avgn_tb[i] <- tb_avgn_anov_list[[i]]$`Pr(>Chisq)`[2][1]
  p_values_maxn_tb[i] <- tb_maxn_anov_list[[i]]$`Pr(>Chisq)`[2][1]
}

test_statistic_dat_tb <- tibble(species=species_tb,MaxN=test_statistics_maxN_tb,AvgN=test_statistics_avgn_tb)
p_values_dat_tb <- tibble(species=species_tb,MaxN=p_values_maxn_tb,AvgN=p_values_avgn_tb)



#try and fix the NA's
i=2

tb_maxn_mod_full_temp <- glmmTMB(value~bait*treatment+location+(1|location:block),
                                 data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
tb_maxn_mod_null_temp <- glmmTMB(value~location+(1|location:block),
                                 data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
test_statistic_dat_tb$MaxN[i] <- anova(tb_maxn_mod_null_temp,tb_maxn_mod_full_temp)$Chisq[2][1]
p_values_dat_tb$MaxN[i] <- anova(tb_maxn_mod_full_temp,tb_maxn_mod_null_temp)$`Pr(>Chisq)`[2][1]


#residuals don't look great - consider changing with starting values
# #plot(simulateResiduals(tb_maxn_mod_full_temp))
# sim= simulateResiduals(tb_maxn_mod_full_temp)
# resid = residuals(sim, quantileFunction = qnorm, outlierValues = c(-4,4))
# plot(resid~predict(tb_maxn_mod_full_temp, re.form = NA,type="response"))


i=3
tb_maxn_mod_full_temp <- glmmTMB(value~bait*treatment+location+(1|location:block),
                                 data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
tb_maxn_mod_null_temp <- glmmTMB(value~location+(1|location:block),
                                 data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
test_statistic_dat_tb$MaxN[i] <- anova(tb_maxn_mod_null_temp,tb_maxn_mod_full_temp)$Chisq[2][1]
p_values_dat_tb$MaxN[i] <- anova(tb_maxn_mod_full_temp,tb_maxn_mod_null_temp)$`Pr(>Chisq)`[2][1]

#plot(simulateResiduals(tb_maxn_mod_full_temp))
# sim= simulateResiduals(tb_maxn_mod_full_temp)
# resid = residuals(sim, quantileFunction = qnorm, outlierValues = c(-4,4))
# plot(resid~predict(tb_maxn_mod_full_temp, re.form = NA,type="response"))


#convert to sumcount
a = goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="AvgN") %>% select(value)
a*42


tb_avgn_mod_full_temp <- glmmTMB(value*42~bait*treatment+location+(1|location:block),
                                 data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="AvgN"),
                                 family=poisson,offset=rep(42,42),
                                 control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))

tb_avgn_mod_null_temp <- glmmTMB(value*42~location+(1|location:block),
                                 data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="AvgN"),
                                 family=poisson,offset=rep(42,42),
                                 control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))


test_statistic_dat_tb$AvgN[i] <- anova(tb_avgn_mod_null_temp,tb_avgn_mod_full_temp)$Chisq[2][1]
p_values_dat_tb$AvgN[i] <- anova(tb_avgn_mod_full_temp,tb_avgn_mod_null_temp)$`Pr(>Chisq)`[2][1]


i=4

#convert to sumcount
a = goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="AvgN") %>% select(value)
a*42


tb_avgn_mod_full_temp <- glmmTMB(value*42~bait*treatment+location+(1|location:block),
                                 data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="AvgN"),
                                 family=poisson,offset=rep(42,42),
                                 control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))

tb_avgn_mod_null_temp <- glmmTMB(value*42~location+(1|location:block),
                                 data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="AvgN"),
                                 family=poisson,offset=rep(42,42),
                                 control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))


test_statistic_dat_tb$AvgN[i] <- anova(tb_avgn_mod_null_temp,tb_avgn_mod_full_temp)$Chisq[2][1]
p_values_dat_tb$AvgN[i] <- anova(tb_avgn_mod_full_temp,tb_avgn_mod_null_temp)$`Pr(>Chisq)`[2][1]




i=5
#convert to sumcount
a = goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="AvgN") %>% select(value)
a*42


tb_avgn_mod_full_temp <- glmmTMB(value*42~bait*treatment+location+(1|location:block),
                                 data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="AvgN"),
                                 family=poisson,offset=rep(42,42))

tb_avgn_mod_null_temp <- glmmTMB(value*42~location+(1|location:block),
                                 data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="AvgN"),
                                 family=poisson,offset=rep(42,42),
                                 control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))


test_statistic_dat_tb$AvgN[i] <- anova(tb_avgn_mod_null_temp,tb_avgn_mod_full_temp)$Chisq[2][1]
p_values_dat_tb$AvgN[i] <- anova(tb_avgn_mod_full_temp,tb_avgn_mod_null_temp)$`Pr(>Chisq)`[2][1]




i=8
tb_maxn_mod_full_temp <- glmmTMB(value~bait*treatment+location+(1|location:block),
                                 data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
tb_maxn_mod_null_temp <- glmmTMB(value~location+(1|location:block),
                                 data=goal_4_dat_tb %>% filter(Class == species_tb[i],measure=="MaxN"),
                                 family=nbinom2,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
test_statistic_dat_tb$MaxN[i] <- anova(tb_maxn_mod_null_temp,tb_maxn_mod_full_temp)$Chisq[2][1]
p_values_dat_tb$MaxN[i] <- anova(tb_maxn_mod_full_temp,tb_maxn_mod_null_temp)$`Pr(>Chisq)`[2][1]


# #plot(simulateResiduals(tb_maxn_mod_full_temp))
# sim= simulateResiduals(tb_maxn_mod_full_temp)
# resid = residuals(sim, quantileFunction = qnorm, outlierValues = c(-4,4))
# plot(resid~predict(tb_maxn_mod_full_temp, re.form = NA,type="response"))


#prep the plot data
#test_statistic_dat_kf <- test_statistic_dat_kf[,c(1:4)]
test_statistic_dat_kf$diff <- test_statistic_dat_kf$AvgN - test_statistic_dat_kf$MaxN
test_statistic_dat_tb$diff <- test_statistic_dat_tb$AvgN - test_statistic_dat_tb$MaxN

colnames(test_statistic_dat_kf) <- c("Class", "MaxN", "AvgN", "diff")
colnames(test_statistic_dat_tb) <- c("Class", "MaxN", "AvgN", "diff")

test_statistic_dat_kf <- test_statistic_dat_kf %>% dplyr::select(Class,MaxN,AvgN,diff)
test_statistic_dat_tb <- test_statistic_dat_tb %>% dplyr::select(Class,MaxN,AvgN,diff)

# 
# goal_4_dat_kf_abund_maxn <- goal_4_dat_kf %>% filter(measure == "MaxN") %>% group_by(Class) %>%
#   summarise(MaxN_mean = mean(value))
# goal_4_dat_kf_abund_avgn <- goal_4_dat_kf %>% filter(measure == "AvgN") %>% group_by(Class) %>%
#   summarise(AvgN_mean = mean(value))
# 
# colnames(test_statistic_dat_kf)[1] = colnames(test_statistic_dat_tb)[1] = "Class"
# test_statistic_dat_kf <- test_statistic_dat_kf %>% left_join(species_metadata_kf,by="Class") %>%
#   left_join(goal_4_dat_kf_abund_avgn,by="Class") %>% left_join(goal_4_dat_kf_abund_maxn,by="Class")
# test_statistic_dat_tb <- left_join(test_statistic_dat_tb,species_metadata_tb,by="Class")
# test_statistic_dat_kf <- test_statistic_dat_kf %>% mutate(Schooling = as.factor(Schooling),
#                                                           Mobility = as.factor(Mobility),
#                                                           Class = as.factor(Class)) %>%
#   select(-Schooling_3_groups,-Mobility_3_groups)
# test_statistic_dat_tb <- test_statistic_dat_tb %>% mutate(Schooling = as.factor(Schooling),
#                                                           Mobility = as.factor(Mobility),
#                                                           Class = as.factor(Class)) %>%
#   select(-Schooling_3_groups,-Mobility_3_groups)
#   
# 

  
write.csv(test_statistic_dat_kf,"results/60_seconds/goal_4_test_statistic_dat_kf.csv")  
write.csv(test_statistic_dat_tb,"results/60_seconds/goal_4_test_statistic_dat_tb.csv")  
write.csv(p_values_dat_kf,"results/60_seconds/p_values_dat_kf.csv")
write.csv(p_values_dat_tb,"results/60_seconds/p_values_dat_tb.csv")
