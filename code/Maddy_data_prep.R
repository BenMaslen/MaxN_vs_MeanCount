#load libraries
library(tidyverse)

#read data
tb_1_min_dat <- read_csv("data/Tassie_BRUV_RUV_pred_1_min.csv")

#inspect
str(tb_1_min_dat)
head(tb_1_min_dat)

####clean data

#rename columns
colnames(tb_1_min_dat)[2:3] <- c("AvgN", "MaxN")

#remove first appeared metrics for now
tb_1_min_dat <- tb_1_min_dat %>% select(-c("First appeared","First appeared prop"))

#replace NA with 0
tb_1_min_dat$MaxN <- tb_1_min_dat$MaxN %>% replace_na(replace=0)

#create split numbers
tb_1_min_dat$split_number_fac <- as.factor(tb_1_min_dat$split_number)


#work out how many splits to keep to maximise video time

goal_2_4_dat_tb_num_splits <- tb_1_min_dat %>% select(Video_name,split_number) %>%
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

split_filt_find_func(goal_2_4_dat_tb_num_splits,150)
#42 - 2142

#calculate data lost
original_amount <- sum(goal_2_4_dat_tb_num_splits$number_of_splits)
original_amount/60
time_lost <- original_amount - (2142)
time_lost/60


max_split_dat_tb <-  tb_1_min_dat %>% group_by(Video_name) %>% summarise(max_split=max(split_number))
tb_1_min_dat <- left_join(tb_1_min_dat,max_split_dat_tb,by="Video_name")


tb_1_min_dat <- tb_1_min_dat %>% filter(split_number <= 42)



#create data for goals 2 and 4
true_sum_all_tb = tb_1_min_dat %>% group_by(Video_name,Class) %>% summarise(AvgN = sum(AvgN*split_length/(sum(split_length))), MaxN = max(MaxN),adjusted_video_length=max(split_number))

vid_metadata_tb         <- read_csv("data/BRUV_RUV_metadata.csv")
species_metadata_tb     <- read_csv("data/TB_species_metadata.csv")

colnames(vid_metadata_tb)[1] <- "Video_name"
maddy_data <- left_join(true_sum_all_tb,vid_metadata_tb,by="Video_name")
maddy_data <- left_join(maddy_data,species_metadata_tb,by="Class")

maddy_data <- maddy_data %>% mutate(Class = as.factor(Class), 
                                    Video_name = as.factor(Video_name),
                                    location = as.factor(location),
                                    block = as.factor(block),
                                    treatment = as.factor(treatment),
                                    bait = as.factor(bait),
                                    Mobility = as.factor(Mobility),
                                    Schooling = as.factor(Schooling))

write.csv(maddy_data,"results/TB_Maddy_data.csv")









