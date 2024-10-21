#load libraries
library(tidyverse)

#read data
kf_1_min_dat <- read_csv("data/Kakadu_fish_pred_1_min.csv")
tb_1_min_dat <- read_csv("data/Tassie_BRUV_RUV_pred_1_min.csv")

#inspect
str(kf_1_min_dat)
str(tb_1_min_dat)
head(kf_1_min_dat)
head(tb_1_min_dat)

####clean data

#rename columns
colnames(kf_1_min_dat)[2:3] <- c("AvgN", "MaxN")
colnames(tb_1_min_dat)[2:3] <- c("AvgN", "MaxN")

#remove first appeared metrics for now
kf_1_min_dat <- kf_1_min_dat %>% dplyr::select(-c("First appeared","First appeared prop"))
tb_1_min_dat <- tb_1_min_dat %>% dplyr::select(-c("First appeared","First appeared prop"))

#replace NA with 0
kf_1_min_dat$MaxN <- kf_1_min_dat$MaxN %>% replace_na(replace=0)
tb_1_min_dat$MaxN <- tb_1_min_dat$MaxN %>% replace_na(replace=0)

#filter out first and last two chunks for Kakadu Fish to avoid footage of placing and removing the camera
max_split_dat <-  kf_1_min_dat %>% group_by(Video_name) %>% summarise(max_split=max(split_number))
kf_1_min_dat_max_split_temp <- left_join(kf_1_min_dat,max_split_dat,by="Video_name")
kf_1_min_dat <- kf_1_min_dat_max_split_temp %>% filter(split_number>4,split_number<max_split-6) %>% 
  droplevels() %>% select(-max_split)  
rm(max_split_dat,kf_1_min_dat_max_split_temp)

#filter out problem videos
kf_1_min_dat <- kf_1_min_dat %>% 
  filter(grepl("2016_Channels_2016_Mudginberri_Videos_Transect_6_(bad_resolution)", Video_name, fixed = TRUE)==FALSE) %>%
  filter(Video_name!="2016_Channels_2016_Sandy_Videos_Sandy_Transect_6_Location_1_Camera_3")
tb_1_min_dat <- tb_1_min_dat %>% filter(Video_name!="20210927_BRUVS_NWB_B2_TRAD_5446")

#filter out non-existent species in Kakadu
kf_1_min_dat <- kf_1_min_dat %>% filter(Class!="Melanotaenia nigrans",
                                        Class!="Mogurnda mogurnda")

#create split numbers
kf_1_min_dat$split_number <- kf_1_min_dat$split_number -4
kf_1_min_dat$split_number_fac <- as.factor(kf_1_min_dat$split_number)
tb_1_min_dat$split_number_fac <- as.factor(tb_1_min_dat$split_number)


#work out how many splits to keep to maximise video time

goal_2_4_dat_tb_num_splits <- tb_1_min_dat %>% select(Video_name,split_number) %>%
  group_by(Video_name) %>% summarise(number_of_splits = max(split_number))
goal_2_4_dat_kf_num_splits <- kf_1_min_dat %>% select(Video_name,split_number) %>%
  group_by(Video_name) %>% summarise(number_of_splits = max(split_number))



split_filt_find_func <- function(dat,splitlim){
  total_vid_lengths <- vector()
  
  for (i in c(1:splitlim)){
    split_filt <- i
    total_vid_lengths[i] = sum(dat$number_of_splits >=  split_filt)*split_filt
  }
  vid_length_dat = data.frame(split_filt=1:splitlim,total_vid_lengths)
  return(vid_length_dat[vid_length_dat$total_vid_lengths==max(vid_length_dat$total_vid_lengths),])
}

split_filt_find_func(goal_2_4_dat_kf_num_splits,150)
#56 - 17808
split_filt_find_func(goal_2_4_dat_tb_num_splits,150)
#42 - 2100

#calculate data lost
original_amount <- sum(goal_2_4_dat_kf_num_splits$number_of_splits) + sum(goal_2_4_dat_tb_num_splits$number_of_splits)
original_amount/60
time_lost <- original_amount - (17808 + 2142)
time_lost/60

max_split_dat_kf <-  kf_1_min_dat %>% group_by(Video_name) %>% summarise(max_split=max(split_number))
kf_1_min_dat <- left_join(kf_1_min_dat,max_split_dat_kf,by="Video_name")

max_split_dat_tb <-  tb_1_min_dat %>% group_by(Video_name) %>% summarise(max_split=max(split_number))
tb_1_min_dat <- left_join(tb_1_min_dat,max_split_dat_tb,by="Video_name")


kf_1_min_dat <- kf_1_min_dat %>% filter(split_number <= 56, max_split >= 56) %>% select(-max_split)
tb_1_min_dat <- tb_1_min_dat %>% filter(split_number <= 42, max_split >= 42) %>% select(-max_split)


#kf_1_min_dat <- kf_1_min_dat %>% dplyr::select(-max_split)
#tb_1_min_dat <- tb_1_min_dat %>% dplyr::select(-max_split)




#create data for goals 1 and 4

true_sum_all_kf = kf_1_min_dat %>% group_by(Video_name,Class) %>% summarise(AvgN = sum(AvgN*split_length/(sum(split_length))), MaxN = max(MaxN))
true_sum_all_tb = tb_1_min_dat %>% group_by(Video_name,Class) %>% summarise(AvgN = sum(AvgN*split_length/(sum(split_length))), MaxN = max(MaxN))


write.csv(true_sum_all_kf,"results/goal_1_2_4_dat_kf.csv")
write.csv(true_sum_all_tb,"results/goal_1_2_4_dat_tb.csv")


goal_2_4_dat_kf_long <-  true_sum_all_kf %>% 
  pivot_longer(
    cols = c("AvgN","MaxN"), 
    names_to = "measure",
    values_to = "value"
  )

goal_2_4_dat_tb_long <-  true_sum_all_tb %>% 
  pivot_longer(
    cols = c("AvgN","MaxN"), 
    names_to = "measure",
    values_to = "value"
  )

write.csv(goal_2_4_dat_kf_long,"results/goal_1_2_4_dat_kf_long.csv")
write.csv(goal_2_4_dat_tb_long,"results/goal_1_2_4_dat_tb_long.csv")


#create data for goal 2

#generate cumulative sum
kf_1_min_dat$Video_name <- as.factor(kf_1_min_dat$Video_name)
tb_1_min_dat$Video_name <- as.factor(tb_1_min_dat$Video_name)
kf_1_min_dat$Class <- as.factor(kf_1_min_dat$Class)
tb_1_min_dat$Class <- as.factor(tb_1_min_dat$Class)

kf_1_min_dat$Class_video_name <- kf_1_min_dat$Video_name:kf_1_min_dat$Class
tb_1_min_dat$Class_video_name <- tb_1_min_dat$Video_name:tb_1_min_dat$Class

#create blank MaxNcum and AvgN cum columns to fill
kf_1_min_dat$MaxN_cum_sum <- rep(NA,dim(kf_1_min_dat)[1])
kf_1_min_dat$AvgN_cum_sum <- rep(NA,dim(kf_1_min_dat)[1])
tb_1_min_dat$MaxN_cum_sum <- rep(NA,dim(tb_1_min_dat)[1])
tb_1_min_dat$AvgN_cum_sum <- rep(NA,dim(tb_1_min_dat)[1])


#Cumulative sum for Tassie BRUV
for (i in c(1:length(levels(tb_1_min_dat$Class_video_name)))){
  true_sum_all_tb_temp <- tb_1_min_dat %>% filter(Class_video_name ==levels(tb_1_min_dat$Class_video_name)[i])
  for (j in c(1:dim(true_sum_all_tb_temp)[1])){
   true_sum_all_tb_temp$MaxN_cum_sum[j] <- max(true_sum_all_tb_temp$MaxN[true_sum_all_tb_temp$split_number<=j])
   true_sum_all_tb_temp$AvgN_cum_sum[j] <- mean(true_sum_all_tb_temp$AvgN[true_sum_all_tb_temp$split_number<=j])
     
  }
  tb_1_min_dat[tb_1_min_dat$Class_video_name ==levels(tb_1_min_dat$Class_video_name)[i],] <- true_sum_all_tb_temp
}

tb_1_min_dat <- tb_1_min_dat %>% select(-Class_video_name)

write.csv(tb_1_min_dat,"results/goal_2_dat_tb.csv")

#Cumulative sum for Kakadu Fish

for (i in c(1:length(levels(kf_1_min_dat$Class_video_name)))){
  true_sum_all_kf_temp <- kf_1_min_dat %>% filter(Class_video_name ==levels(kf_1_min_dat$Class_video_name)[i])
  for (j in c(1:dim(true_sum_all_kf_temp)[1])){
    true_sum_all_kf_temp$MaxN_cum_sum[j] <- max(true_sum_all_kf_temp$MaxN[true_sum_all_kf_temp$split_number<=j])
    true_sum_all_kf_temp$AvgN_cum_sum[j] <- mean(true_sum_all_kf_temp$AvgN[true_sum_all_kf_temp$split_number<=j])
    
  }
  kf_1_min_dat[kf_1_min_dat$Class_video_name ==levels(kf_1_min_dat$Class_video_name)[i],] <- true_sum_all_kf_temp
}

kf_1_min_dat <- kf_1_min_dat %>% select(-Class_video_name)

write.csv(kf_1_min_dat,"results/goal_2_dat_kf.csv")

#convert to long format
#kf_1_min_dat <- read.csv("results/goal_2_dat_kf.csv")
#tb_1_min_dat <- read.csv("results/goal_2_dat_tb.csv")


goal_2_dat_kf_long <-  kf_1_min_dat %>% select(-MaxN) %>%
  pivot_longer(
    cols = c("AvgN","AvgN_cum_sum","MaxN_cum_sum"), 
    names_to = "measure",
    values_to = "value"
  )

goal_2_dat_tb_long <-  tb_1_min_dat %>% select(-MaxN) %>%
  pivot_longer(
    cols = c("AvgN","AvgN_cum_sum","MaxN_cum_sum"), 
    names_to = "measure",
    values_to = "value"
  )


write.csv(goal_2_dat_kf_long,"results/goal_2_dat_kf_long.csv")
write.csv(goal_2_dat_tb_long,"results/goal_2_dat_tb_long.csv")
