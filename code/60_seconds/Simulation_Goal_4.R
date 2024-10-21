#simulate MaxN vs Average N
library(MASS)
library(tidyverse)
library(glmmTMB)


#parameter set - winner winner chicken dinner
# speed_ratio = 10
# overall_population_mean = 25
# p_in = 0.001
# p_out = 0.01
# 
# n_sims = 1000
# vid_length = 60*60
# pop_disp = 1
# n_schooling = 2


set.seed(1234)

power_fun <- function(u_1,u_2,sample_size,power_sims,vid_length){

vid_sim_function <- function(overall_population_mean,sample_size,power_sims,vid_length){
  
  #parameter set - small differences in var for pop mean 
  speed_ratio = 10
  
  p_in = 0.001
  p_out = 0.01
  
  n_sims = power_sims*sample_size
  pop_disp = 1
  n_schooling = 2
  
  p_in_f = p_in*speed_ratio
  p_out_f = p_out*speed_ratio
  school_disp = (n_schooling*pop_disp)/overall_population_mean
  
  
  #Long term - stationary distribution
  pop_stat = p_out/(p_in+p_out)
  samp_stat = 1 - pop_stat 
  
  
  #slow movers
  
  MaxN_slow = rep(NA,n_sims)
  AvgN_slow = rep(NA,n_sims)
  N_pop_true_slow = rep(NA,n_sims)
  
  for (j in 1:n_sims){
    
    
    N_vid = rep(NA,vid_length)
    N_pop = rep(NA,vid_length)
    
    N_pop_true_slow[j] = rnegbin(1,overall_population_mean,pop_disp)
    N_pop[1] = rbinom(1,N_pop_true_slow[j],pop_stat)
    N_vid[1] = N_pop_true_slow[j] - N_pop[1]
    
    
    for (i in 2:vid_length){
      number_in = rbinom(1,N_pop[i-1],p_in)
      number_out = rbinom(1,N_vid[i-1],p_out)
      N_vid[i] = N_vid[i-1] + number_in - number_out
      N_pop[i] = N_pop[i-1] - number_in + number_out
    }
    
    MaxN_slow[j] = max(N_vid)
    AvgN_slow[j] = mean(N_vid)
  }
  
  
  
  #fast movers
  
  MaxN_fast = rep(NA,n_sims)
  AvgN_fast = rep(NA,n_sims)
  N_pop_true_fast = rep(NA,n_sims)
  
  for (j in 1:n_sims){
    
    
    N_vid = rep(NA,vid_length)
    N_pop = rep(NA,vid_length)
    
    N_pop_true_fast[j] = rnegbin(1,overall_population_mean,pop_disp)
    N_pop[1] = rbinom(1,N_pop_true_fast[j],pop_stat)
    N_vid[1] = N_pop_true_fast[j] - N_pop[1]
    
    
    for (i in 2:vid_length){
      number_in = rbinom(1,N_pop[i-1],p_in_f)
      number_out = rbinom(1,N_vid[i-1],p_out_f)
      N_vid[i] = N_vid[i-1] + number_in - number_out
      N_pop[i] = N_pop[i-1] - number_in + number_out
    }
    
    MaxN_fast[j] = max(N_vid)
    AvgN_fast[j] = mean(N_vid)
  }
  
  
  
  
  #slow movers high schooling
  
  
  #starting conditions
  MaxN_slow_school = rep(NA,n_sims)
  AvgN_slow_school = rep(NA,n_sims)
  N_pop_true_slow_school = rep(NA,n_sims)
  
  for (j in 1:n_sims){
    
    pop_schools = list()
    vid_schools = list()
    
    
    all_schools = rnegbin(overall_population_mean/n_schooling,n_schooling,school_disp)
    N_vid = rep(NA,vid_length)
    N_pop = rep(NA,vid_length)
    
    N_pop_true_slow_school[j] = sum(all_schools)
    
    starting_split = rbinom(length(all_schools),1,pop_stat)
    pop_schools[[1]] = all_schools[starting_split==1]
    vid_schools[[1]] = all_schools[starting_split==0]
    
    N_pop[1] = sum(pop_schools[[1]])
    N_vid[1] = sum(vid_schools[[1]])
    
    for (i in 1:(vid_length-1)){
      pop_split = rbinom(length(pop_schools[[i]]),1,p_in)
      vid_split = rbinom(length(vid_schools[[i]]),1,p_out)
      
      pop_schools[[i+1]] = c(pop_schools[[i]][pop_split==0],vid_schools[[i]][vid_split==1])
      vid_schools[[i+1]] = c(pop_schools[[i]][pop_split==1],vid_schools[[i]][vid_split==0])
      
      
      N_pop[i+1] = sum(pop_schools[[i+1]])
      N_vid[i+1] = sum(vid_schools[[i+1]])
    }
    
    MaxN_slow_school[j] = max(N_vid)
    AvgN_slow_school[j] = mean(N_vid)
  }
  
  
  
  #fast movers high schooling
  
  
  #starting conditions
  MaxN_fast_school = rep(NA,n_sims)
  AvgN_fast_school = rep(NA,n_sims)
  N_pop_true_fast_school = rep(NA,n_sims)
  
  for (j in 1:n_sims){
    
    pop_schools = list()
    vid_schools = list()
    all_schools = rnegbin(overall_population_mean/n_schooling,n_schooling,school_disp)
    
    N_vid = rep(NA,vid_length)
    N_pop = rep(NA,vid_length)
    
    N_pop_true_fast_school[j] = sum(all_schools)
    
    starting_split = rbinom(length(all_schools),1,pop_stat)
    pop_schools[[1]] = all_schools[starting_split==1]
    vid_schools[[1]] = all_schools[starting_split==0]
    
    N_pop[1] = sum(pop_schools[[1]])
    N_vid[1] = sum(vid_schools[[1]])
    
    for (i in 1:(vid_length-1)){
      pop_split = rbinom(length(pop_schools[[i]]),1,p_in_f)
      vid_split = rbinom(length(vid_schools[[i]]),1,p_out_f)
      
      pop_schools[[i+1]] = c(pop_schools[[i]][pop_split==0],vid_schools[[i]][vid_split==1])
      vid_schools[[i+1]] = c(pop_schools[[i]][pop_split==1],vid_schools[[i]][vid_split==0])
      
      
      N_pop[i+1] = sum(pop_schools[[i+1]])
      N_vid[i+1] = sum(vid_schools[[i+1]])
    }
    
    MaxN_fast_school[j] = max(N_vid)
    AvgN_fast_school[j] = mean(N_vid)
  }
  
  
  
  behaviour_comp <- data.frame(Mobility = rep(c("High","Low"),each=n_sims*2),
                               Schooling = rep(rep(c("High","Low"),each=n_sims),2),
                               MaxN=c(MaxN_fast_school,MaxN_fast,MaxN_slow_school, MaxN_slow),
                               AvgN=c(AvgN_fast_school, AvgN_fast, AvgN_slow_school, AvgN_slow),
                               PopN=c(N_pop_true_fast_school,N_pop_true_fast,N_pop_true_slow_school,N_pop_true_slow))
  
  behaviour_comp$Mobility <- as.factor(behaviour_comp$Mobility)
  behaviour_comp$Schooling <- as.factor(behaviour_comp$Schooling)
  
  

  return(behaviour_comp)
  
}

u_1 = u_1
u_2 = u_2
sample_size = sample_size
power_sims = power_sims
vid_length = vid_length


mean_1 <- vid_sim_function(u_1,sample_size,power_sims,vid_length)
mean_2 <- vid_sim_function(u_2,sample_size,power_sims,vid_length)

mean_1$Class <- mean_1$Mobility:mean_1$Schooling
mean_2$Class <- mean_2$Mobility:mean_2$Schooling


power_dat <- data.frame(power = rep(NA,8),
                        Class = rep(levels(mean_1$Class),each=2),
                        measure = rep(c("MaxN","meancount"),4),
                        na_sum =  rep(NA,8))


p_vals_MaxN <- rep(NA,power_sims)
p_vals_meancount <- rep(NA,power_sims)

mean_1_temp <- mean_1 %>% filter(Mobility=="High",Schooling=="High")
mean_2_temp <- mean_2 %>% filter(Mobility=="High",Schooling=="High")


for (i in 1:power_sims){
  from = (sample_size*(i-1)+1)
  to = sample_size*i
  dat_temp <- rbind(mean_1_temp[from:to,],
                    mean_2_temp[from:to,])
  
  dat_temp$treatment <- as.factor(rep(c("A","B"),sample_size))
  
  
  fit_MaxN <- glmmTMB(MaxN~treatment,family=nbinom2,data=dat_temp)
  fit_MaxN_null <- glmmTMB(MaxN~1,family=nbinom2,data=dat_temp)
  
  p_vals_MaxN[i] <- anova(fit_MaxN_null,fit_MaxN)$`Pr(>Chisq)`[2]
  
  
  fit_meancount <- glmmTMB(AvgN~treatment,family=tweedie,data=dat_temp)
  fit_meancount_null <- glmmTMB(AvgN~1,family=tweedie,data=dat_temp)
  
  p_vals_meancount[i] <- anova(fit_meancount_null,fit_meancount)$`Pr(>Chisq)`[2]
  
  #catch if models struggles to converge
  if(is.na(p_vals_meancount[i])){
    fit_meancount <- glmmTMB(AvgN~treatment,family=tweedie,data=dat_temp,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
    fit_meancount_null <- glmmTMB(AvgN~1,family=tweedie,data=dat_temp,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
    
    p_vals_meancount[i] <- anova(fit_meancount_null,fit_meancount)$`Pr(>Chisq)`[2]
  }
  
  if(is.na(p_vals_MaxN[i])){
    fit_MaxN <- glmmTMB(MaxN~treatment,family=nbinom2,data=dat_temp,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
    fit_MaxN_null <- glmmTMB(MaxN~1,family=nbinom2,data=dat_temp,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
    
    p_vals_MaxN[i] <- anova(fit_MaxN_null,fit_MaxN)$`Pr(>Chisq)`[2]
  }
}

power_dat$power[1] <- sum(p_vals_MaxN<0.05,na.rm=T)/sum(is.na(p_vals_MaxN)==F)
power_dat$power[2] <- sum(p_vals_meancount<0.05,na.rm=T)/sum(is.na(p_vals_meancount)==F)

power_dat$na_sum[1] <- sum(is.na(p_vals_MaxN))
power_dat$na_sum[2] <- sum(is.na(p_vals_meancount))

mean_1_temp <- mean_1 %>% filter(Mobility=="High",Schooling=="Low")
mean_2_temp <- mean_2 %>% filter(Mobility=="High",Schooling=="Low")


for (i in 1:power_sims){
  from = (sample_size*(i-1)+1)
  to = sample_size*i
  dat_temp <- rbind(mean_1_temp[from:to,],
                    mean_2_temp[from:to,])
  
  dat_temp$treatment <- as.factor(rep(c("A","B"),sample_size))
  
  
  fit_MaxN <- glmmTMB(MaxN~treatment,family=nbinom2,data=dat_temp)
  fit_MaxN_null <- glmmTMB(MaxN~1,family=nbinom2,data=dat_temp)
  
  p_vals_MaxN[i] <- anova(fit_MaxN_null,fit_MaxN)$`Pr(>Chisq)`[2]
  
  
  fit_meancount <- glmmTMB(AvgN~treatment,family=tweedie,data=dat_temp)
  fit_meancount_null <- glmmTMB(AvgN~1,family=tweedie,data=dat_temp)
  
  p_vals_meancount[i] <- anova(fit_meancount_null,fit_meancount)$`Pr(>Chisq)`[2]
  
  #catch if models struggles to converge
  if(is.na(p_vals_meancount[i])){
    fit_meancount <- glmmTMB(AvgN~treatment,family=tweedie,data=dat_temp,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
    fit_meancount_null <- glmmTMB(AvgN~1,family=tweedie,data=dat_temp,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
    
    p_vals_meancount[i] <- anova(fit_meancount_null,fit_meancount)$`Pr(>Chisq)`[2]
  }
  
  if(is.na(p_vals_MaxN[i])){
    fit_MaxN <- glmmTMB(MaxN~treatment,family=nbinom2,data=dat_temp,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
    fit_MaxN_null <- glmmTMB(MaxN~1,family=nbinom2,data=dat_temp,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
    
    p_vals_MaxN[i] <- anova(fit_MaxN_null,fit_MaxN)$`Pr(>Chisq)`[2]
  }
}


power_dat$power[3] <- sum(p_vals_MaxN<0.05,na.rm=T)/sum(is.na(p_val_high_m_high_s_MaxN)==F)
power_dat$power[4] <- sum(p_vals_meancount<0.05,na.rm=T)/sum(is.na(p_val_high_m_high_s_meancount)==F)

power_dat$na_sum[3] <- sum(is.na(p_vals_MaxN))
power_dat$na_sum[4] <- sum(is.na(p_vals_meancount))

mean_1_temp <- mean_1 %>% filter(Mobility=="Low",Schooling=="High")
mean_2_temp <- mean_2 %>% filter(Mobility=="Low",Schooling=="High")


for (i in 1:power_sims){
  from = (sample_size*(i-1)+1)
  to = sample_size*i
  dat_temp <- rbind(mean_1_temp[from:to,],
                    mean_2_temp[from:to,])
  
  dat_temp$treatment <- as.factor(rep(c("A","B"),sample_size))
  
  
  fit_MaxN <- glmmTMB(MaxN~treatment,family=nbinom2,data=dat_temp)
  fit_MaxN_null <- glmmTMB(MaxN~1,family=nbinom2,data=dat_temp)
  
  p_vals_MaxN[i] <- anova(fit_MaxN_null,fit_MaxN)$`Pr(>Chisq)`[2]
  
  
  fit_meancount <- glmmTMB(AvgN~treatment,family=tweedie,data=dat_temp)
  fit_meancount_null <- glmmTMB(AvgN~1,family=tweedie,data=dat_temp)
  
  p_vals_meancount[i] <- anova(fit_meancount_null,fit_meancount)$`Pr(>Chisq)`[2]
  
  #catch if models struggles to converge
  if(is.na(p_vals_meancount[i])){
    fit_meancount <- glmmTMB(AvgN~treatment,family=tweedie,data=dat_temp,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
    fit_meancount_null <- glmmTMB(AvgN~1,family=tweedie,data=dat_temp,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
    
    p_vals_meancount[i] <- anova(fit_meancount_null,fit_meancount)$`Pr(>Chisq)`[2]
  }
  
  if(is.na(p_vals_MaxN[i])){
    fit_MaxN <- glmmTMB(MaxN~treatment,family=nbinom2,data=dat_temp,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
    fit_MaxN_null <- glmmTMB(MaxN~1,family=nbinom2,data=dat_temp,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
    
    p_vals_MaxN[i] <- anova(fit_MaxN_null,fit_MaxN)$`Pr(>Chisq)`[2]
  }
}


power_dat$power[5] <- sum(p_vals_MaxN<0.05,na.rm=T)/sum(is.na(p_val_high_m_high_s_MaxN)==F)
power_dat$power[6] <- sum(p_vals_meancount<0.05,na.rm=T)/sum(is.na(p_val_high_m_high_s_meancount)==F)

power_dat$na_sum[5] <- sum(is.na(p_vals_MaxN))
power_dat$na_sum[6] <- sum(is.na(p_vals_meancount))

mean_1_temp <- mean_1 %>% filter(Mobility=="Low",Schooling=="Low")
mean_2_temp <- mean_2 %>% filter(Mobility=="Low",Schooling=="Low")


for (i in 1:power_sims){
  from = (sample_size*(i-1)+1)
  to = sample_size*i
  dat_temp <- rbind(mean_1_temp[from:to,],
                    mean_2_temp[from:to,])
  
  dat_temp$treatment <- as.factor(rep(c("A","B"),sample_size))
  
  
  fit_MaxN <- glmmTMB(MaxN~treatment,family=nbinom2,data=dat_temp)
  fit_MaxN_null <- glmmTMB(MaxN~1,family=nbinom2,data=dat_temp)
  
  p_vals_MaxN[i] <- anova(fit_MaxN_null,fit_MaxN)$`Pr(>Chisq)`[2]
  
  
  fit_meancount <- glmmTMB(AvgN~treatment,family=tweedie,data=dat_temp)
  fit_meancount_null <- glmmTMB(AvgN~1,family=tweedie,data=dat_temp)
  
  p_vals_meancount[i] <- anova(fit_meancount_null,fit_meancount)$`Pr(>Chisq)`[2]
  
  #catch if models struggles to converge
  if(is.na(p_vals_meancount[i])){
    fit_meancount <- glmmTMB(AvgN~treatment,family=tweedie,data=dat_temp,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
    fit_meancount_null <- glmmTMB(AvgN~1,family=tweedie,data=dat_temp,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
    
    p_vals_meancount[i] <- anova(fit_meancount_null,fit_meancount)$`Pr(>Chisq)`[2]
  }
  
  if(is.na(p_vals_MaxN[i])){
    fit_MaxN <- glmmTMB(MaxN~treatment,family=nbinom2,data=dat_temp,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
    fit_MaxN_null <- glmmTMB(MaxN~1,family=nbinom2,data=dat_temp,control=glmmTMBControl(optimizer=optim,optArgs = list(method="BFGS")))
    
    p_vals_MaxN[i] <- anova(fit_MaxN_null,fit_MaxN)$`Pr(>Chisq)`[2]
  }
}


power_dat$power[7] <- sum(p_vals_MaxN<0.05,na.rm=T)/sum(is.na(p_val_high_m_high_s_MaxN)==F)
power_dat$power[8] <- sum(p_vals_meancount<0.05,na.rm=T)/sum(is.na(p_val_high_m_high_s_meancount)==F)

power_dat$na_sum[7] <- sum(is.na(p_vals_MaxN))
power_dat$na_sum[8] <- sum(is.na(p_vals_meancount))


return(power_dat)

}


power_10_100_5 <- power_fun(10,100,10,10,60)
power_10_100_5
# power     Class   measure na_sum
# 1 0.11000000 High:High      MaxN      0
# 2 0.12000000 High:High meancount      0
# 3 0.05000000  High:Low      MaxN      0
# 4 0.10101010  High:Low meancount      1
# 5 0.11000000  Low:High      MaxN      0
# 6 0.09090909  Low:High meancount      2
# 7 0.04000000   Low:Low      MaxN      0
# 8 0.11111111   Low:Low meancount      1


# power     Class   measure na_sum
# 1 0.04000000 High:High      MaxN      0
# 2 0.03000000 High:High meancount      0
# 3 0.05000000  High:Low      MaxN      0
# 4 0.09090909  High:Low meancount      0
# 5 0.10000000  Low:High      MaxN      0
# 6 0.10101010  Low:High meancount      0
# 7 0.05000000   Low:Low      MaxN      0
# 8 0.11111111   Low:Low meancount      0

power_20_40_100 <- power_fun(20,40,100,100,60*60)

power_20_40_100

# power     Class   measure na_sum
# 1 0.08000000 High:High      MaxN      0
# 2 0.10000000 High:High meancount      0
# 3 0.07000000  High:Low      MaxN      0
# 4 0.14141414  High:Low meancount      0
# 5 0.09000000  Low:High      MaxN      0
# 6 0.15151515  Low:High meancount      0
# 7 0.04000000   Low:Low      MaxN      0
# 8 0.06060606   Low:Low meancount      0

power_10_100_100 <- power_fun(10,100,100,100,60*60)

power_10_100_100
