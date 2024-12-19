#abundance comparison


library(tidyverse)


#read in data (generated from Goal_1_data_prep)
goal_4_dat_tb <- read_csv("results/goal_1_2_4_dat_tb_long.csv")
goal_4_dat_kf <- read_csv("results/goal_1_2_4_dat_kf_long.csv")

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


test_stat_comp = rbind(goal_4_dat_tb %>% filter(measure=="AvgN") %>% 
        select(Class,value),goal_4_dat_kf %>% 
        filter(measure=="AvgN") %>% select(Class,value)) %>% group_by(Class) %>%
  summarise(MeanCount = mean(value)) %>% arrange(desc(MeanCount))


#read in test statistic subsampled data

kf_or <- read.csv("results/goal_4_test_statistic_dat_kf.csv")  
tb_or <- read.csv("results/goal_4_test_statistic_dat_tb.csv")  
kf_30 <- read.csv("results/30_seconds/goal_4_test_statistic_dat_kf.csv")  
tb_30 <- read.csv("results/30_seconds/goal_4_test_statistic_dat_tb.csv")  
kf_60 <- read.csv("results/60_seconds/goal_4_test_statistic_dat_kf.csv")  
tb_60 <- read.csv("results/60_seconds/goal_4_test_statistic_dat_tb.csv")  


chi_or <- rbind(kf_or %>% select(Class,diff),tb_or %>% select(Class,diff) )
chi_30 <- rbind(kf_30 %>% select(Class,diff),tb_30 %>% select(Class,diff) )
chi_60 <- rbind(kf_60 %>% select(Class,diff),tb_60 %>% select(Class,diff) )

colnames(chi_or)[2] <- "diff_or"
colnames(chi_30)[2] <- "diff_30"
colnames(chi_60)[2] <- "diff_60"


test_stat_comp = test_stat_comp %>% left_join(chi_or,by="Class") %>%
  left_join(chi_30,by="Class") %>%
  left_join(chi_60,by="Class")
  

View(test_stat_comp)
sum(test_stat_comp$diff_30>0)
sum(test_stat_comp$diff_60>0)



test_stat_comp$diff_30_alt <- c(test_stat_comp$diff_30[1:16],test_stat_comp$diff_or[17:26])
test_stat_comp$diff_60_alt <- c(test_stat_comp$diff_60[1:16],test_stat_comp$diff_or[17:26])

sum(test_stat_comp$diff_30_alt>0)
sum(test_stat_comp$diff_60_alt>0)


test_stat_comp$diff_30_alt <- c(test_stat_comp$diff_30[1:13],test_stat_comp$diff_or[14:26])
test_stat_comp$diff_60_alt <- c(test_stat_comp$diff_60[1:13],test_stat_comp$diff_or[14:26])

sum(test_stat_comp$diff_30_alt>0)
sum(test_stat_comp$diff_60_alt>0)


sum(test_stat_comp$diff_or[19:28]>0)

  
  