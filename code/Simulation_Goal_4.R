#simulate MaxN vs Average N
library(dplyr)
library(statmod)
library(MASS)
library(tweedie)


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
    p_out = 0.05
    
    n_sims = power_sims*sample_size
    pop_disp = 4
    n_schooling = 2
    
    p_in_f = p_in*speed_ratio
    p_out_f = p_out*speed_ratio
    school_disp = (n_schooling*pop_disp)/overall_population_mean
    
    
    #Long term - stationary distribution
    pop_stat = p_out/(p_in+p_out)
    samp_stat = 1 - pop_stat 
    
    
    #slow movers
    
    MaxN_slow = rep(NA,n_sims)
    MeanCount_slow = rep(NA,n_sims)
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
      MeanCount_slow[j] = mean(N_vid)
    }
    
    
    
    #fast movers
    
    MaxN_fast = rep(NA,n_sims)
    MeanCount_fast = rep(NA,n_sims)
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
      MeanCount_fast[j] = mean(N_vid)
    }
    
    
    
    #slow movers high schooling
    
    
    #starting conditions
    MaxN_slow_school = rep(NA,n_sims)
    MeanCount_slow_school = rep(NA,n_sims)
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
      MeanCount_slow_school[j] = mean(N_vid)
    }
    
    
    
    #fast movers high schooling
    
    
    #starting conditions
    MaxN_fast_school = rep(NA,n_sims)
    MeanCount_fast_school = rep(NA,n_sims)
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
      MeanCount_fast_school[j] = mean(N_vid)
    }
    
    
    
    behaviour_comp <- data.frame(Mobility = rep(c("High","Low"),each=n_sims*2),
                                 Schooling = rep(rep(c("High","Low"),each=n_sims),2),
                                 MaxN=c(MaxN_fast_school,MaxN_fast,MaxN_slow_school, MaxN_slow),
                                 MeanCount=c(MeanCount_fast_school, MeanCount_fast, MeanCount_slow_school, MeanCount_slow),
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
  
  all_dat <- rbind(mean_1,mean_2)
  all_dat$means <- rep(c(u_1,u_2),each=dim(mean_1)[1])
  write.csv(all_dat,paste0("../../results/Chapter_2/simulation_results/raw_data_",u_1,"_",u_2,"_",sample_size,"_",power_sims,".csv"))
  
  
  mean_1$Class <- mean_1$Mobility:mean_1$Schooling
  mean_2$Class <- mean_2$Mobility:mean_2$Schooling
  
  
  power_dat <- data.frame(power = rep(NA,8),
                          Class = rep(levels(mean_1$Class),each=2),
                          measure = rep(c("MaxN","meancount"),4),
                          na_sum =  rep(NA,8),
                          p_mean = rep(NA,8))
  
  
  p_vals_MaxN <- rep(NA,power_sims)
  p_vals_meancount <- rep(NA,power_sims)
  
  mean_1_temp <- mean_1 %>% filter(Mobility=="High",Schooling=="High")
  mean_2_temp <- mean_2 %>% filter(Mobility=="High",Schooling=="High")
  
  
  for (i in 1:power_sims){
    from = (sample_size*(i-1)+1)
    to = sample_size*i
    dat_temp <- rbind(mean_1_temp[from:to,],
                      mean_2_temp[from:to,])
    
    dat_temp$treatment <- as.factor(rep(c("A","B"),each=sample_size))
    
    
    try(fit_MaxN <- glm.nb(MaxN~treatment,data=dat_temp))
    try(p_vals_MaxN[i] <- anova(fit_MaxN,test="LRT")$`Pr(>Chi)`[2])
    
    #catch if theta estimation fails to converge
    if(is.na(p_vals_MaxN[i])){
      try(fit_MaxN <- glm(MaxN~treatment,data=dat_temp,family=negative.binomial(theta=1.2)))
      try(p_vals_MaxN[i] <- anova(fit_MaxN,test="LRT")$`Pr(>Chi)`[2])
    }
    
    dat_temp$sumcount = dat_temp$MeanCount*vid_length
    dat_temp$vid_length =  vid_length
    
    try(fit_meancount <- glm.nb(sumcount~treatment+offset(log(vid_length)),data=dat_temp))
    try(p_vals_meancount[i] <- anova(fit_meancount,test="LRT")$`Pr(>Chi)`[2])
    
    #catch if theta estimation fails to converge
    if(is.na(p_vals_meancount[i])){
      try(fit_meancount <- glm(sumcount~treatment+offset(log(vid_length)),data=dat_temp,family=negative.binomial(theta=1.2)))
      try(p_vals_meancount[i] <- anova(fit_meancount,test="LRT")$`Pr(>Chi)`[2])
    }
    
    #  prof <- tweedie.profile(MeanCount ~ treatment,data=dat_temp, p.vec = seq(1.1, 1.9, 0.1), method = "series")
    #  fit_meancount <- glm(MeanCount~treatment,data=dat_temp,family=tweedie(var.power=prof$p.max,link.power=0))
    
    #  p_vals_meancount[i] <- anova(fit_meancount,test="LRT")$`Pr(>Chi)`[2]
    
    
    
  }
  
  power_dat$power[1] <- sum(p_vals_MaxN<0.05,na.rm=T)/sum(is.na(p_vals_MaxN)==F)
  power_dat$power[2] <- sum(p_vals_meancount<0.05,na.rm=T)/sum(is.na(p_vals_meancount)==F)
  
  power_dat$na_sum[1] <- sum(is.na(p_vals_MaxN))
  power_dat$na_sum[2] <- sum(is.na(p_vals_meancount))
  
  power_dat$p_mean[1] <- mean(p_vals_MaxN,na.rm=T)
  power_dat$p_mean[2] <- mean(p_vals_meancount,na.rm=T)
  
  mean_1_temp <- mean_1 %>% filter(Mobility=="High",Schooling=="Low")
  mean_2_temp <- mean_2 %>% filter(Mobility=="High",Schooling=="Low")
  
  
  for (i in 1:power_sims){
    from = (sample_size*(i-1)+1)
    to = sample_size*i
    dat_temp <- rbind(mean_1_temp[from:to,],
                      mean_2_temp[from:to,])
    
    dat_temp$treatment <- as.factor(rep(c("A","B"),each=sample_size))
    
    
    try(fit_MaxN <- glm.nb(MaxN~treatment,data=dat_temp))
    try(p_vals_MaxN[i] <- anova(fit_MaxN,test="LRT")$`Pr(>Chi)`[2])
    
    #catch if theta estimation fails to converge
    if(is.na(p_vals_MaxN[i])){
      try(fit_MaxN <- glm(MaxN~treatment,data=dat_temp,family=negative.binomial(theta=1.2)))
      try(p_vals_MaxN[i] <- anova(fit_MaxN,test="LRT")$`Pr(>Chi)`[2])
    }
    
    dat_temp$sumcount = dat_temp$MeanCount*vid_length
    dat_temp$vid_length =  vid_length
    
    try(fit_meancount <- glm.nb(sumcount~treatment+offset(log(vid_length)),data=dat_temp))
    try(p_vals_meancount[i] <- anova(fit_meancount,test="LRT")$`Pr(>Chi)`[2])
    
    #catch if theta estimation fails to converge
    if(is.na(p_vals_meancount[i])){
      try(fit_meancount <- glm(sumcount~treatment+offset(log(vid_length)),data=dat_temp,family=negative.binomial(theta=1.2)))
      try(p_vals_meancount[i] <- anova(fit_meancount,test="LRT")$`Pr(>Chi)`[2])
    }
    
    
    #  prof <- tweedie.profile(MeanCount ~ treatment,data=dat_temp, p.vec = seq(1.1, 1.9, 0.1), method = "series")
    #  fit_meancount <- glm(MeanCount~treatment,data=dat_temp,family=tweedie(var.power=prof$p.max,link.power=0))
    
    #  p_vals_meancount[i] <- anova(fit_meancount,test="LRT")$`Pr(>Chi)`[2]
    
  }
  
  
  power_dat$power[3] <- sum(p_vals_MaxN<0.05,na.rm=T)/sum(is.na(p_vals_MaxN)==F)
  power_dat$power[4] <- sum(p_vals_meancount<0.05,na.rm=T)/sum(is.na(p_vals_meancount)==F)
  
  power_dat$na_sum[3] <- sum(is.na(p_vals_MaxN))
  power_dat$na_sum[4] <- sum(is.na(p_vals_meancount))
  
  power_dat$p_mean[3] <- mean(p_vals_MaxN,na.rm=T)
  power_dat$p_mean[4] <- mean(p_vals_meancount,na.rm=T)
  
  mean_1_temp <- mean_1 %>% filter(Mobility=="Low",Schooling=="High")
  mean_2_temp <- mean_2 %>% filter(Mobility=="Low",Schooling=="High")
  
  
  for (i in 1:power_sims){
    from = (sample_size*(i-1)+1)
    to = sample_size*i
    dat_temp <- rbind(mean_1_temp[from:to,],
                      mean_2_temp[from:to,])
    
    dat_temp$treatment <- as.factor(rep(c("A","B"),each=sample_size))
    
    
    try(fit_MaxN <- glm.nb(MaxN~treatment,data=dat_temp))
    try(p_vals_MaxN[i] <- anova(fit_MaxN,test="LRT")$`Pr(>Chi)`[2])
    
    #catch if theta estimation fails to converge
    if(is.na(p_vals_MaxN[i])){
      try(fit_MaxN <- glm(MaxN~treatment,data=dat_temp,family=negative.binomial(theta=1.2)))
      try(p_vals_MaxN[i] <- anova(fit_MaxN,test="LRT")$`Pr(>Chi)`[2])
    }
    
    dat_temp$sumcount = dat_temp$MeanCount*vid_length
    dat_temp$vid_length =  vid_length
    
    try(fit_meancount <- glm.nb(sumcount~treatment+offset(log(vid_length)),data=dat_temp))
    try(p_vals_meancount[i] <- anova(fit_meancount,test="LRT")$`Pr(>Chi)`[2])
    
    #catch if theta estimation fails to converge
    if(is.na(p_vals_meancount[i])){
      try(fit_meancount <- glm(sumcount~treatment+offset(log(vid_length)),data=dat_temp,family=negative.binomial(theta=1.2)))
      try(p_vals_meancount[i] <- anova(fit_meancount,test="LRT")$`Pr(>Chi)`[2])
    }
    
    
    #  prof <- tweedie.profile(MeanCount ~ treatment,data=dat_temp, p.vec = seq(1.1, 1.9, 0.1), method = "series")
    #  fit_meancount <- glm(MeanCount~treatment,data=dat_temp,family=tweedie(var.power=prof$p.max,link.power=0))
    
    #  p_vals_meancount[i] <- anova(fit_meancount,test="LRT")$`Pr(>Chi)`[2]
    
  }
  
  
  power_dat$power[5] <- sum(p_vals_MaxN<0.05,na.rm=T)/sum(is.na(p_vals_MaxN)==F)
  power_dat$power[6] <- sum(p_vals_meancount<0.05,na.rm=T)/sum(is.na(p_vals_meancount)==F)
  
  power_dat$na_sum[5] <- sum(is.na(p_vals_MaxN))
  power_dat$na_sum[6] <- sum(is.na(p_vals_meancount))
  
  power_dat$p_mean[5] <- mean(p_vals_MaxN,na.rm=T)
  power_dat$p_mean[6] <- mean(p_vals_meancount,na.rm=T)
  
  mean_1_temp <- mean_1 %>% filter(Mobility=="Low",Schooling=="Low")
  mean_2_temp <- mean_2 %>% filter(Mobility=="Low",Schooling=="Low")
  
  
  for (i in 1:power_sims){
    from = (sample_size*(i-1)+1)
    to = sample_size*i
    dat_temp <- rbind(mean_1_temp[from:to,],
                      mean_2_temp[from:to,])
    
    dat_temp$treatment <- as.factor(rep(c("A","B"),each=sample_size))
    
    try(fit_MaxN <- glm.nb(MaxN~treatment,data=dat_temp))
    try(p_vals_MaxN[i] <- anova(fit_MaxN,test="LRT")$`Pr(>Chi)`[2])
    
    #catch if theta estimation fails to converge
    if(is.na(p_vals_MaxN[i])){
      try(fit_MaxN <- glm(MaxN~treatment,data=dat_temp,family=negative.binomial(theta=1.2)))
      try(p_vals_MaxN[i] <- anova(fit_MaxN,test="LRT")$`Pr(>Chi)`[2])
    }
    
    dat_temp$sumcount = dat_temp$MeanCount*vid_length
    dat_temp$vid_length =  vid_length
    
    try(fit_meancount <- glm.nb(sumcount~treatment+offset(log(vid_length)),data=dat_temp))
    try(p_vals_meancount[i] <- anova(fit_meancount,test="LRT")$`Pr(>Chi)`[2])
    
    #catch if theta estimation fails to converge
    if(is.na(p_vals_meancount[i])){
      try(fit_meancount <- glm(sumcount~treatment+offset(log(vid_length)),data=dat_temp,family=negative.binomial(theta=1.2)))
      try(p_vals_meancount[i] <- anova(fit_meancount,test="LRT")$`Pr(>Chi)`[2])
    }
    
    
    #  prof <- tweedie.profile(MeanCount ~ treatment,data=dat_temp, p.vec = seq(1.1, 1.9, 0.1), method = "series")
    #  fit_meancount <- glm(MeanCount~treatment,data=dat_temp,family=tweedie(var.power=prof$p.max,link.power=0))
    
    #  p_vals_meancount[i] <- anova(fit_meancount,test="LRT")$`Pr(>Chi)`[2]
    
  }
  
  
  power_dat$power[7] <- sum(p_vals_MaxN<0.05,na.rm=T)/sum(is.na(p_vals_MaxN)==F)
  power_dat$power[8] <- sum(p_vals_meancount<0.05,na.rm=T)/sum(is.na(p_vals_meancount)==F)
  
  power_dat$na_sum[7] <- sum(is.na(p_vals_MaxN))
  power_dat$na_sum[8] <- sum(is.na(p_vals_meancount))
  
  power_dat$p_mean[7] <- mean(p_vals_MaxN,na.rm=T)
  power_dat$p_mean[8] <- mean(p_vals_meancount,na.rm=T)
  
  #write.csv(mean_1,"../../results/Chapter_2/simulation_results/mean_1_100.csv")
  #write.csv(mean_2,"../../results/Chapter_2/simulation_results/mean_2_100.csv")
  
  return(power_dat)
  
}

argv <- as.numeric(commandArgs(TRUE))

sample_sizes <- seq(4,40,4)

#power_fun <- function(u_1,u_2,sample_size,power_sims,vid_length)

#power_sim_r <- power_fun(10,20,argv,10,60*60)
power_sim_r <- power_fun(10,20,sample_sizes[argv],1000,60*60)

power_sim_r

#write.csv(power_sim_r,"../../results/Chapter_2/simulation_results/test_10_20_100_10.csv")
write.csv(power_sim_r,paste0("../../results/Chapter_2/simulation_results/power_10_20_",sample_sizes[argv],"_1000.csv"))
