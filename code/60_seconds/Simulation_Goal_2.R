#simulate MaxN vs Average N
library(MASS)
library(tidyverse)




#parameter set
speed_ratio = 10
overall_population_mean = 10
p_in = 0.001
p_out = 0.05

n_sims = 1000
vid_length = 60*60
pop_disp = 4
n_schooling = 2

video_id  <- rep(1:n_sims,each=vid_length)
time      <- rep(1:vid_length,n_sims)

set.seed(1234)

second_skipper = 60


p_in_f = p_in*speed_ratio
p_out_f = p_out*speed_ratio
school_disp = (n_schooling*pop_disp)/overall_population_mean


#Long term - stationary distribution
pop_stat = p_out/(p_in+p_out)
samp_stat = 1 - pop_stat 


#slow movers

MaxN_slow = rep(NA,n_sims*vid_length)
AvgN_slow = rep(NA,n_sims*vid_length)
N_pop_true_slow = rep(NA,n_sims)

for (j in 1:n_sims){
  
  N_vid = rep(NA,vid_length)
  N_pop = rep(NA,vid_length)
  
  N_pop_true_slow[j] = rnegbin(1,overall_population_mean,pop_disp)
  N_pop[1] = rbinom(1,N_pop_true_slow[j],pop_stat)
  N_vid[1] = N_pop_true_slow[j] - N_pop[1]
  
  MaxN_slow[1+(j-1)*(vid_length)] <- N_vid[1]
  AvgN_slow[1+(j-1)*(vid_length)] <- N_vid[1]
  
  for (i in 2:vid_length){
    number_in = rbinom(1,N_pop[i-1],p_in)
    number_out = rbinom(1,N_vid[i-1],p_out)
    N_vid[i] = N_vid[i-1] + number_in - number_out
    N_pop[i] = N_pop[i-1] - number_in + number_out
    
    vid_state_temp = N_vid[1:i]
    
    MaxN_slow[(j-1)*(vid_length)+i] = max(vid_state_temp)
    AvgN_slow[(j-1)*(vid_length)+i] = mean(vid_state_temp[seq(1, length(vid_state_temp), second_skipper)])
  }
}


#fast movers

MaxN_fast = rep(NA,n_sims*vid_length)
AvgN_fast = rep(NA,n_sims*vid_length)
N_pop_true_fast = rep(NA,n_sims)

for (j in 1:n_sims){
  
  
  N_vid = rep(NA,vid_length)
  N_pop = rep(NA,vid_length)
  
  N_pop_true_fast[j] = rnegbin(1,overall_population_mean,pop_disp)
  N_pop[1] = rbinom(1,N_pop_true_fast[j],pop_stat)
  N_vid[1] = N_pop_true_fast[j] - N_pop[1]
  
  MaxN_fast[1+(j-1)*(vid_length)] <- N_vid[1]
  AvgN_fast[1+(j-1)*(vid_length)] <- N_vid[1]
  
  for (i in 2:vid_length){
    number_in = rbinom(1,N_pop[i-1],p_in_f)
    number_out = rbinom(1,N_vid[i-1],p_out_f)
    N_vid[i] = N_vid[i-1] + number_in - number_out
    N_pop[i] = N_pop[i-1] - number_in + number_out
    
    vid_state_temp = N_vid[1:i]
    
    MaxN_fast[(j-1)*(vid_length)+i] = max(vid_state_temp)
    AvgN_fast[(j-1)*(vid_length)+i] = mean(vid_state_temp[seq(1, length(vid_state_temp), second_skipper)])
  }
}


#slow movers high schooling


#starting conditions
MaxN_slow_school = rep(NA,n_sims*vid_length)
AvgN_slow_school = rep(NA,n_sims*vid_length)
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
  
  MaxN_slow_school[1+(j-1)*(vid_length)] <- N_vid[1]
  AvgN_slow_school[1+(j-1)*(vid_length)] <- N_vid[1]
  
  for (i in 1:(vid_length-1)){
    pop_split = rbinom(length(pop_schools[[i]]),1,p_in)
    vid_split = rbinom(length(vid_schools[[i]]),1,p_out)
    
    pop_schools[[i+1]] = c(pop_schools[[i]][pop_split==0],vid_schools[[i]][vid_split==1])
    vid_schools[[i+1]] = c(pop_schools[[i]][pop_split==1],vid_schools[[i]][vid_split==0])
    
    
    N_pop[i+1] = sum(pop_schools[[i+1]])
    N_vid[i+1] = sum(vid_schools[[i+1]])
    
    vid_state_temp = N_vid[1:i]
    
    MaxN_slow_school[(j-1)*(vid_length)+i+1] = max(vid_state_temp)
    AvgN_slow_school[(j-1)*(vid_length)+i+1] = mean(vid_state_temp[seq(1, length(vid_state_temp), second_skipper)])
  }
}



#fast movers high schooling


#starting conditions
MaxN_fast_school = rep(NA,n_sims*vid_length)
AvgN_fast_school = rep(NA,n_sims*vid_length)
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
  
  MaxN_fast_school[1+(j-1)*(vid_length)] <- N_vid[1]
  AvgN_fast_school[1+(j-1)*(vid_length)] <- N_vid[1]
  
  for (i in 1:(vid_length-1)){
    pop_split = rbinom(length(pop_schools[[i]]),1,p_in_f)
    vid_split = rbinom(length(vid_schools[[i]]),1,p_out_f)
    
    pop_schools[[i+1]] = c(pop_schools[[i]][pop_split==0],vid_schools[[i]][vid_split==1])
    vid_schools[[i+1]] = c(pop_schools[[i]][pop_split==1],vid_schools[[i]][vid_split==0])
    
    
    N_pop[i+1] = sum(pop_schools[[i+1]])
    N_vid[i+1] = sum(vid_schools[[i+1]])
    
    vid_state_temp = N_vid[1:i]
    
    MaxN_fast_school[(j-1)*(vid_length)+i+1] = max(vid_state_temp)
    AvgN_fast_school[(j-1)*(vid_length)+i+1] = mean(vid_state_temp[seq(1, length(vid_state_temp), second_skipper)])
  }
}





time_comp <- data.frame(Mobility = rep(c("High","Low"),each=length(MaxN_slow)*2),
                             Schooling = rep(rep(c("High","Low"),each=length(MaxN_slow)),2),
                             MaxN=c(MaxN_fast_school,MaxN_fast,MaxN_slow_school, MaxN_slow),
                             AvgN=c(AvgN_fast_school, AvgN_fast, AvgN_slow_school, AvgN_slow),
                        video_id=as.factor(rep(video_id,4)),
                        time=rep(time,4))

time_comp_long <- data.frame(Mobility = as.factor(rep(rep(c("High","Low"),each=length(MaxN_slow)*2),2)),
                             Schooling = as.factor(rep(rep(rep(c("High","Low"),each=length(MaxN_slow)),2),2)),
                             value=c(MaxN_fast_school,MaxN_fast,MaxN_slow_school, MaxN_slow, AvgN_fast_school, AvgN_fast, AvgN_slow_school, AvgN_slow),
                             measure=as.factor(rep(c("MaxN","meancount"),each=length(MaxN_slow)*4)),
                             Video_name=as.factor(rep(video_id,8)),
                        split_number=rep(time,8)/60)

time_comp_long$Class <- time_comp_long$Mobility:time_comp_long$Schooling

write.csv(time_comp_long,"results/60_seconds/sim_goal_2_dat.csv")

#plot params
# maxn_tol_sim = 0.5
# a_clarity = 0.2
# meancount_tol_sim = min(time_comp_long$value[time_comp_long$value!=0 & time_comp_long$measure == "meancount"])/2
# 
# time_comp_long$value_tol <- time_comp_long$value + 
#   (time_comp_long$measure == "meancount")*meancount_tol_sim + (time_comp_long$measure == "MaxN")*maxn_tol_sim
# 
# 
# time_comp_long_sum <- time_comp_long %>% group_by(measure,split_number) %>% summarise(value = mean(value_tol))
# time_comp_long_sum_sp <- time_comp_long %>% group_by(measure,split_number,Class) %>% summarise(value = mean(value_tol))
# 
# 
# 
# cols <- c("Overall Avg" = "#66C2A5","Avg per species" = "#8DA0CB", "Video:Species" = "#FC8D62")
# 
# 
# ggplot(time_comp_long,aes(x=split_number,y=value_tol,colour="Video:Species",group=Video_name:Class)) + 
#   geom_line(alpha=0.25)  + scale_y_log10() + facet_grid(measure~1,scales = "free",switch="y") +
#   geom_line(data=time_comp_long_sum,mapping=aes(x=split_number,y=value,group=measure,colour="Overall Avg"),
#             size=1.6) + 
#   geom_line(data=time_comp_long_sum_sp,mapping=aes(x=split_number,y=value,group=measure:Class,colour="Avg per species"),
#             size=0.67,alpha=0.7)+ theme_bw() + guides(colour=guide_legend(title="Legend")) + 
#   xlab("video length") + ylab("value") + 
#   scale_color_manual(values = cols,limits=c("Overall Avg","Avg per species","Video:Species"),
#                      labels = c("Overall Avg","Avg per species","Video:Species")) +
#   theme(axis.title.y=element_blank(),
#         strip.background = element_blank(),
#         strip.placement = "outside",
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         legend.position = "bottom",
#         legend.title  = element_blank(),
#         strip.text.x = element_text(hjust=-0.01,size=10))
# 
# 
# ggsave("plots/abund_video_length_sim_alt.png",height=5.12,width=5.79*(0.98/0.62))












