#load libraries
library(tidyverse)

#read data
kf_4_min_dat <- read_csv("data/Kakadu_fish_pred.csv")
tb_4_min_dat <- read_csv("data/Tassie_BRUV_RUV_pred.csv")

#inspect
str(kf_4_min_dat)
str(tb_4_min_dat)
head(kf_4_min_dat)
head(tb_4_min_dat)


####clean data

#rename columns
colnames(kf_4_min_dat)[2:3] <- c("AvgN", "MaxN")
colnames(tb_4_min_dat)[2:3] <- c("AvgN", "MaxN")

#remove first appeared metrics for now
kf_4_min_dat <- kf_4_min_dat %>% select(-c("First appeared","First appeared prop"))
tb_4_min_dat <- tb_4_min_dat %>% select(-c("First appeared","First appeared prop"))

#replace NA with 0
kf_4_min_dat$MaxN <- kf_4_min_dat$MaxN %>% replace_na(replace=0)
tb_4_min_dat$MaxN <- tb_4_min_dat$MaxN %>% replace_na(replace=0)

#filter out first and last two chunks for Kakadu Fish to avoid footage of placing and removing the camera

max_split_dat <-  kf_4_min_dat %>% group_by(Video_name) %>% summarise(max_split=max(split_number))
kf_4_min_dat_max_split_temp <- left_join(kf_4_min_dat,max_split_dat,by="Video_name")
kf_4_min_dat <- kf_4_min_dat_max_split_temp %>% filter(split_number>1,split_number<max_split-1) %>% 
  select(-max_split) %>% droplevels() 
rm(max_split_dat,kf_4_min_dat_max_split_temp)

#filter out problem videos
kf_4_min_dat <- kf_4_min_dat %>% 
  filter(grepl("2016_Channels_2016_Mudginberri_Videos_Transect_6_(bad_resolution)", Video_name, fixed = TRUE)==FALSE) %>%
  filter(Video_name!="2016_Channels_2016_Sandy_Videos_Sandy_Transect_6_Location_1_Camera_3")
tb_4_min_dat <- tb_4_min_dat %>% filter(Video_name!="20210927_BRUVS_NWB_B2_TRAD_5446")


#create split numbers
kf_4_min_dat$split_number <- kf_4_min_dat$split_number -1
kf_4_min_dat$split_number_fac <- as.factor(kf_4_min_dat$split_number)
tb_4_min_dat$split_number_fac <- as.factor(tb_4_min_dat$split_number)


#work out optimum video length

#work out how many splits to keep to maximise video time

goal_1_dat_tb_num_splits <- tb_4_min_dat %>% select(Video_name,split_number) %>%
  group_by(Video_name) %>% summarise(number_of_splits = max(split_number))
goal_1_dat_kf_num_splits <- kf_4_min_dat %>% select(Video_name,split_number) %>%
  group_by(Video_name) %>% summarise(number_of_splits = max(split_number))



split_filt_find_func_4 <- function(dat,splitlim){
  total_vid_lengths <- vector()
  
  for (i in c(1:splitlim)){
    split_filt <- i
    total_vid_lengths[i] = sum(dat$number_of_splits >=  split_filt)*split_filt*4
  }
  vid_length_dat = data.frame(split_filt=1:splitlim,total_vid_lengths)
  return(vid_length_dat$split_filt[vid_length_dat$total_vid_lengths==max(vid_length_dat$total_vid_lengths)])
}

split_filt_find_func_4(goal_1_dat_kf_num_splits,50)
#15
split_filt_find_func_4(goal_1_dat_tb_num_splits,50)
#11

max_split_dat_kf <-  kf_4_min_dat %>% group_by(Video_name) %>% summarise(max_split=max(split_number))
kf_4_min_dat <- left_join(kf_4_min_dat,max_split_dat_kf,by="Video_name")

max_split_dat_tb <-  tb_4_min_dat %>% group_by(Video_name) %>% summarise(max_split=max(split_number))
tb_4_min_dat <- left_join(tb_4_min_dat,max_split_dat_tb,by="Video_name")


kf_4_min_dat <- kf_4_min_dat %>% filter(split_number <= 15, max_split >= 15) %>% select(-max_split)
tb_4_min_dat <- tb_4_min_dat %>% filter(split_number <= 11, max_split >= 11) %>% select(-max_split)








#block bootstrap

#number of splits per video
num_splits_kf = kf_4_min_dat %>% group_by(Video_name) %>% summarise(splits = max(split_number))
num_splits_tb = tb_4_min_dat %>% group_by(Video_name) %>% summarise(splits = max(split_number))



block_boot <- function(nsim,num_splits,dat){
  
  for (k in 1:nsim){
    
    for (j in c(1:nrow(num_splits))){
      
      #extract new split combo
      new_split = sample(x = as.numeric(num_splits[j,2]), size= as.integer(num_splits[j,2]), replace = TRUE)
      
      #reconstruct data set
      
      video_fil_temp = dat %>% filter(Video_name == num_splits[j,1][[1]])
      
      for (i in c(1:length(new_split))){
        if (i==1){
          new_video_struc = video_fil_temp %>% filter(split_number == new_split[i])
        }else{
          new_video_struc = rbind(new_video_struc,video_fil_temp %>% filter(split_number == new_split[i]))
        }
      }
      
      if(j==1){
        resamp_dat = new_video_struc
      }else{
        resamp_dat = rbind(resamp_dat,new_video_struc)
      }
      
    }
    #reconstruct measure per species
    resamp_sum = resamp_dat %>% group_by(Video_name,Class) %>% summarise(AvgN = sum(AvgN*split_length/(sum(split_length))), MaxN = max(MaxN))
    
    resamp_sum$boot_id = rep(k,nrow(resamp_sum))
    
    if(k==1){
      resamp_sum_all = resamp_sum
    }else{
      resamp_sum_all = rbind(resamp_sum_all,resamp_sum)
    }
  }
  
  resamp_sum_true = dat %>% group_by(Video_name,Class) %>% group_by(Video_name,Class) %>% summarise(AvgN = sum(AvgN*split_length/(sum(split_length))), MaxN = max(MaxN))
  resamp_sum_true$boot_id = rep(nsim +1,dim(resamp_sum_true)[1])
  resamp_sum_all = rbind(resamp_sum_all,resamp_sum_true)
  
  return(resamp_sum_all)  
}

tb_resamp_dat <- block_boot(nsim=999,num_splits = num_splits_tb, dat = tb_4_min_dat)

start.time <- Sys.time()
kf_resamp_dat <- block_boot(nsim=999,num_splits = num_splits_kf, dat = kf_4_min_dat)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#write.csv(kf_resamp_dat,"results/kf_resamp_dat.csv")
#write.csv(tb_resamp_dat,"results/tb_resamp_dat.csv")



resamp_sum_all_mean_kf = kf_resamp_dat %>% group_by(Video_name,Class) %>% summarise(AvgN_mean=mean(AvgN),MaxN_mean = mean(MaxN),
                                                                                    AvgN_CoV = sd(AvgN)/(mean(AvgN)),Maxn_CoV = sd(MaxN)/(mean(MaxN)))

resamp_sum_all_mean_tb = tb_resamp_dat %>% group_by(Video_name,Class) %>% summarise(AvgN_mean=mean(AvgN),MaxN_mean = mean(MaxN),
                                                                                    AvgN_CoV = sd(AvgN)/(mean(AvgN)),Maxn_CoV = sd(MaxN)/(mean(MaxN)))



true_sum_all_kf = kf_4_min_dat %>% group_by(Video_name,Class) %>% summarise(AvgN = sum(AvgN*split_length/(sum(split_length))), MaxN = max(MaxN))

true_sum_all_tb = tb_4_min_dat %>% group_by(Video_name,Class) %>% summarise(AvgN = sum(AvgN*split_length/(sum(split_length))), MaxN = max(MaxN))


#insert bias vars
resamp_sum_all_mean_kf$AvgN_bias = true_sum_all_kf$AvgN - resamp_sum_all_mean_kf$AvgN_mean
resamp_sum_all_mean_kf$MaxN_bias = true_sum_all_kf$MaxN - resamp_sum_all_mean_kf$MaxN_mean
resamp_sum_all_mean_tb$AvgN_bias = true_sum_all_tb$AvgN - resamp_sum_all_mean_tb$AvgN_mean
resamp_sum_all_mean_tb$MaxN_bias = true_sum_all_tb$MaxN - resamp_sum_all_mean_tb$MaxN_mean

#insert % diff vars
resamp_sum_all_mean_kf$AvgN_perc_diff = 100*(true_sum_all_kf$AvgN  - resamp_sum_all_mean_kf$AvgN_mean)/(true_sum_all_kf$AvgN) 
resamp_sum_all_mean_kf$MaxN_perc_diff = 100*(true_sum_all_kf$MaxN - resamp_sum_all_mean_kf$MaxN_mean)/(true_sum_all_kf$MaxN)
resamp_sum_all_mean_tb$AvgN_perc_diff = 100*(true_sum_all_tb$AvgN  - resamp_sum_all_mean_tb$AvgN_mean)/(true_sum_all_tb$AvgN) 
resamp_sum_all_mean_tb$MaxN_perc_diff = 100*(true_sum_all_tb$MaxN - resamp_sum_all_mean_tb$MaxN_mean)/(true_sum_all_tb$MaxN)

#insert relative bias vars
resamp_sum_all_mean_kf$AvgN_bias_rel = resamp_sum_all_mean_kf$AvgN_bias/resamp_sum_all_mean_kf$AvgN_mean
resamp_sum_all_mean_kf$MaxN_bias_rel = resamp_sum_all_mean_kf$MaxN_bias/resamp_sum_all_mean_kf$MaxN_mean
resamp_sum_all_mean_tb$AvgN_bias_rel = resamp_sum_all_mean_tb$AvgN_bias/resamp_sum_all_mean_tb$AvgN_mean
resamp_sum_all_mean_tb$MaxN_bias_rel = resamp_sum_all_mean_tb$MaxN_bias/resamp_sum_all_mean_tb$MaxN_mean


#write.csv(resamp_sum_all_mean_tb,"results/resamp_sum_all_mean_tb.csv")
#write.csv(resamp_sum_all_mean_kf,"results/resamp_sum_all_mean_kf.csv")


#long format
AvgN_half_kf = resamp_sum_all_mean_kf %>% select(Video_name, Class, AvgN_mean, AvgN_bias, AvgN_CoV, AvgN_perc_diff,AvgN_bias_rel)
MaxN_half_kf = resamp_sum_all_mean_kf %>% select(Video_name, Class, MaxN_mean , MaxN_bias, Maxn_CoV, MaxN_perc_diff,MaxN_bias_rel)
AvgN_half_tb = resamp_sum_all_mean_tb %>% select(Video_name, Class, AvgN_mean, AvgN_bias, AvgN_CoV, AvgN_perc_diff,AvgN_bias_rel)
MaxN_half_tb = resamp_sum_all_mean_tb %>% select(Video_name, Class, MaxN_mean , MaxN_bias, Maxn_CoV, MaxN_perc_diff,MaxN_bias_rel)


colnames(AvgN_half_kf)[3:7] = colnames(MaxN_half_kf)[3:7] = c("mean", "bias", "CoV", "perc_diff","bias_rel")
colnames(AvgN_half_tb)[3:7] = colnames(MaxN_half_tb)[3:7] = c("mean", "bias", "CoV", "perc_diff","bias_rel")

resamp_sum_all_mean_long_kf = rbind(AvgN_half_kf,MaxN_half_kf)
resamp_sum_all_mean_long_tb = rbind(AvgN_half_tb,MaxN_half_tb)

resamp_sum_all_mean_long_kf$measure = rep(c("AvgN","MaxN"),each = nrow(AvgN_half_kf))
resamp_sum_all_mean_long_tb$measure = rep(c("AvgN","MaxN"),each = nrow(AvgN_half_tb))

write.csv(resamp_sum_all_mean_long_tb,"results/goal_3_dat_tb.csv")
write.csv(resamp_sum_all_mean_long_kf,"results/goal_3_dat_kf.csv")

