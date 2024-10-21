#simulate MaxN vs Average N
library(MASS)
library(tidyverse)



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

mean_var_function <- function(overall_population_mean){

#Parameter set - winner winner chicken dinner
#  speed_ratio = 10
#  p_in = 0.001
#  p_out = 0.01
# # 
#  n_sims = 1000
#  vid_length = 60*60
#  pop_disp = 1
#  n_schooling = 2

 
 # speed_ratio = 10
 # # overall_population_mean = 2
 # p_in = 0.001
 # p_out = 0.09
 # 
 # n_sims = 1000
 # vid_length = 60*60
 # pop_disp = 1
 # n_schooling = 1
 
 speed_ratio = 10
 #overall_population_mean = 10
 p_in = 0.001
 p_out = 0.05
 
 n_sims = 1000
 vid_length = 60*60
 pop_disp = 4
 n_schooling = 2
 
 
  
#parameter set - small differences in var for pop mean 
# speed_ratio = 10
# 
# p_in = 0.004
# p_out = 0.05
# 
# n_sims = 5000
# vid_length = 60*60
# pop_disp = 1
# n_schooling = 2

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


# behaviour_comp_summary <- behaviour_comp %>% group_by(Mobility,Schooling) %>% summarise(MaxN_mean = mean(MaxN),MaxN_cov = sd(MaxN)/mean(MaxN),
#                                                               AvgN_mean = mean(AvgN),AvgN_cov = sd(AvgN)/mean(AvgN),
#                                                               PopN_mean = mean(PopN),PopN_cov = sd(PopN)/mean(PopN))

return(behaviour_comp)

}

mean_var_list <- list()
pop_means <- c(10,20,30,40,50,60,70,80,90,100)


for (i in 1:length(pop_means)){
  mean_var_list[[i]] <- mean_var_function(pop_means[i])
}


mean_var_dat <- mean_var_list[[1]]

for (i in 2:length(pop_means)){
  mean_var_dat <- rbind(mean_var_dat,mean_var_list[[i]])
}


mean_var_dat$pop_means <- as.factor(rep(pop_means,each =nrow(mean_var_list[[1]])))

#write.csv(mean_var_dat,"results/simulation_goal3_dat.csv")
mean_var_dat <- read.csv("results/simulation_goal3_dat.csv")
mean_var_dat$Mobility <- factor(mean_var_dat$Mobility)
mean_var_dat$Schooling <- factor(mean_var_dat$Schooling)
mean_var_dat <- mean_var_dat %>% select(-X)

mean_var_dat_summary <- mean_var_dat %>% group_by(Mobility,Schooling,pop_means) %>% summarise(MaxN_mean = mean(MaxN),MaxN_var = sd(MaxN)^2,
                                                                                                                                  AvgN_mean = mean(AvgN),AvgN_var = sd(AvgN)^2,
                                                                                                                                  PopN_mean = mean(PopN),PopN_var = sd(PopN)^2)
pop_mean_overall = mean(mean_var_dat$PopN)
AvgN_mean_overall = mean(mean_var_dat$AvgN)
MaxN_mean_overall = mean(mean_var_dat$MaxN)



library(RColorBrewer)

plot_list_sim <- list()

plot_list_sim[[2]] <- ggplot(mean_var_dat_summary, aes(y=AvgN_var,x=AvgN_mean,colour=Schooling:Mobility)) + geom_point() + scale_x_log10() + scale_y_log10()  +theme_bw() + 
  scale_color_manual(values = brewer.pal(n = 4, name = "Set2")[c(1,2,3,4)]) + ylab("MeanCount variance (log)")+xlab("MeanCount mean (log)") +geom_smooth(se=F,size=0.5,method = "gam",formula = y~s(x,k=3)) 
plot_list_sim[[2]]

ggsave("plots/sim_goal3_meancount.png",height=5.12,width=5.79*(0.98/0.62))

plot_list_sim[[4]] <- ggplot(mean_var_dat_summary, aes(y=MaxN_var,x=MaxN_mean,colour=Schooling:Mobility)) + geom_point() + scale_x_log10() + scale_y_log10()  +theme_bw() + 
  scale_color_manual(values = brewer.pal(n = 4, name = "Set2")[c(1,2,3,4)]) + ylab("MaxN variance (log)")+xlab("MaxN mean (log)")+ geom_smooth(se=F,size=0.5,method = "gam",formula = y~s(x,k=3)) 
plot_list_sim[[4]]

ggsave("plots/sim_goal3_maxn.png",height=5.12,width=5.79*(0.98/0.62))


plot_list_sim[[1]] <- ggplot(mean_var_dat_summary, aes(x=PopN_mean/pop_mean_overall,y=AvgN_mean/AvgN_mean_overall,colour=Schooling:Mobility))   +theme_bw() + 
  scale_color_manual(values = brewer.pal(n = 4, name = "Set2")[c(1,2,3,4)]) + ylab("MeanCount mean (scaled)")+xlab("Pop mean (scaled)")+ 
  geom_point(data=mean_var_dat,aes(x=PopN/pop_mean_overall,y=AvgN/AvgN_mean_overall,colour=Schooling:Mobility),size=0.1,alpha=0.1) +  
 geom_smooth(se=F,size=0.5,method = "gam",formula = y~s(x,k=3)) + 
  geom_abline(colour="red")+ylim(0,2.5)+xlim(0,2.5) + geom_point() + theme(legend.position = "none")
plot_list_sim[[1]]

ggsave("plots/sim_goal3_meancount_bias.png",height=5.12,width=5.79*(0.98/0.62))


plot_list_sim[[3]] <- ggplot(mean_var_dat_summary, aes(y=MaxN_mean/mean(MaxN_mean),x=PopN_mean/mean(PopN_mean),colour=Schooling:Mobility))  +theme_bw() + 
  scale_color_manual(values = brewer.pal(n = 4, name = "Set2")[c(1,2,3,4)]) + ylab("MaxN mean (scaled)")+xlab("Pop mean (scaled)") +
  geom_point(data=mean_var_dat,aes(x=PopN/pop_mean_overall,y=MaxN/MaxN_mean_overall,colour=Schooling:Mobility),size=0.1,alpha=0.1) + 
  geom_smooth(se=F,size=0.5,method = "gam",formula = y~s(x,k=3))  + 
  geom_abline(colour="red")+ylim(0,3.5)+xlim(0,3.5)  + geom_point() + theme(legend.position = "none")
plot_list_sim[[3]]

ggsave("plots/sim_goal3_maxn_bias.png",height=5.12,width=5.79*(0.98/0.62))

library(patchwork)

combined_plot_all <- wrap_plots(plotlist = plot_list_sim, ncol = 2,nrow=2,guides ='collect') #+ plot_layout(axis_titles = "collect")
combined_plot_all

ggsave("plots/sim_goal3.png",height=5.12,width=5.79*(0.98/0.62))





