#simulate MaxN vs Average N
library(MASS)
library(tidyverse)

set.seed(1234)



#parameter set
speed_ratio = 10
overall_population_mean = 10
p_in = 0.001
p_out = 0.05

second_skipper = 60

n_sims = 1000
vid_length = 60*60
pop_disp = 4
n_schooling = 2

set.seed(1234)



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
  AvgN_slow[j] = mean(N_vid[seq(1, length(N_vid), second_skipper)])
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
  AvgN_fast[j] = mean(N_vid[seq(1, length(N_vid), second_skipper)])
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
  AvgN_slow_school[j] = mean(N_vid[seq(1, length(N_vid), second_skipper)])
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
  AvgN_fast_school[j] = mean(N_vid[seq(1, length(N_vid), second_skipper)])
}



behaviour_comp <- data.frame(Mobility = rep(c("High","Low"),each=n_sims*2),
                             Schooling = rep(rep(c("High","Low"),each=n_sims),2),
                             MaxN=c(MaxN_fast_school,MaxN_fast,MaxN_slow_school, MaxN_slow),
                             AvgN=c(AvgN_fast_school, AvgN_fast, AvgN_slow_school, AvgN_slow),
                             PopN=c(N_pop_true_fast_school,N_pop_true_fast,N_pop_true_slow_school,N_pop_true_slow))

behaviour_comp$Mobility <- as.factor(behaviour_comp$Mobility)
behaviour_comp$Schooling <- as.factor(behaviour_comp$Schooling)


ggplot(behaviour_comp,aes(x=AvgN,y=jitter(MaxN,1.5),colour = Mobility:Schooling)) + geom_point(alpha=0.4) + 
  scale_y_log10(limits=c(0.8,50)) + scale_x_log10() + scale_color_brewer(palette = "Set2") + ylab("MaxN") + theme_bw()# +


ggplot(behaviour_comp,aes(x=AvgN/mean(AvgN),y=jitter(MaxN,1.5)/mean(MaxN),colour = Mobility:Schooling)) + geom_point(alpha=0.4) +
  scale_color_brewer(palette = "Set2") + ylab("MaxN") + geom_abline(colour="red")+ theme_bw() +scale_y_log10(limits=c(0.05,10)) + 
  scale_x_log10(limits=c(0.05,10))


min((behaviour_comp$AvgN/mean(behaviour_comp$AvgN))[behaviour_comp$AvgN/mean(behaviour_comp$AvgN)!=0])
#ggsave("plots/sim_goal1_meancount_maxn_p1.png",height=5.12,width=5.79*(0.98/0.62))


ggplot(behaviour_comp,aes(x=PopN,y=jitter(MaxN+0.5,1.5),colour = Mobility:Schooling)) + geom_point(alpha=0.4) + 
  scale_y_log10() + scale_x_log10() + scale_color_brewer(palette = "Set2") + ylab("MaxN") + theme_bw() +
  geom_smooth(method = "gam",formula = y~s(x,k=3),se=F)

#ggsave("plots/sim_goal1_popn_maxn_p1.png",height=5.12,width=5.79*(0.98/0.62))


#ggplot(behaviour_comp,aes(x=PopN,y=AvgN,colour = Mobility:Schooling)) + geom_point(alpha=0.4) + 
#  scale_y_log10() + scale_x_log10() + scale_color_brewer(palette = "Set2") + ylab("AvgN") + theme_bw() +
#  geom_smooth(method = "gam",formula = y~s(x,k=3),se=F)


behaviour_comp$PopN_adj <- behaviour_comp$PopN + min(behaviour_comp$PopN[behaviour_comp$PopN!=0])/2
behaviour_comp$AvgN_adj <- behaviour_comp$AvgN + min(behaviour_comp$AvgN[behaviour_comp$AvgN!=0])/2



ggplot(behaviour_comp,aes(x=PopN,y=AvgN_adj,colour = Mobility:Schooling)) + geom_point(alpha=0.4) + 
  scale_y_log10() + scale_x_log10() + scale_color_brewer(palette = "Set2") + ylab("AvgN") + theme_bw() +
  geom_smooth(method = "gam",formula = y~s(x,k=3),se=F)


#ggsave("plots/sim_goal1_popn_meancount_p1.png",height=5.12,width=5.79*(0.98/0.62))



behaviour_comp %>% group_by(Mobility,Schooling) %>% summarise(MaxN_mean = mean(MaxN),MaxN_cov = sd(MaxN)/mean(MaxN),
                                                              AvgN_mean = mean(AvgN),AvgN_cov = sd(AvgN)/mean(AvgN),
                                                              PopN_mean = mean(PopN),PopN_cov = sd(PopN)/mean(PopN))



write.csv(behaviour_comp,"results/60_seconds/simulation_goal1_dat.csv")

