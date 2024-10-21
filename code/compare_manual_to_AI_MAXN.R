#Load libraries
library(tidyverse)


#read in data (generated from Goal_1_data_prep)
goal_4_dat_tb <- read_csv("results/goal_1_2_4_dat_tb_AI_vs_Manual.csv")
goal_4_dat_kf <- read_csv("results/goal_1_2_4_dat_kf_AI_vs_Manual.csv")

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




goal_4_dat_kf_avg = goal_4_dat_kf %>% filter(transect != "Transect 6") %>% group_by(Class) %>% summarise(MaxN = mean(MaxN))

goal_4_dat_kf_avg %>% 
mutate(Class = fct_reorder(Class, desc(MaxN))) %>%
ggplot(aes(y=MaxN+1,x=Class)) + ylab("MaxN + 1 (log10 scale)") +
  geom_col() +theme_classic() + scale_y_log10(limits=c(1,15)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


goal_4_dat_tb$Video_name[1]



#Tassie

tassie_raw_maxN = read.csv("data/fish_maxn.csv")

tassie_raw_maxN <- tassie_raw_maxN %>% dplyr::select(opcode, species, family, maxn)


tassie_raw_maxN_sum <- tassie_raw_maxN %>% group_by(species) %>% summarise(MaxN = sum(maxn)/61)

tassie_raw_maxN_sum <- tassie_raw_maxN_sum %>% filter(species %in% c("Arripis truttaceus","Cephaloscyllium laticeps",
                                                                     "Chyrosophyrs auratus","Meuschenia australis",
                                                                     "Moridae","Platycephalus bassensis",
                                                                     "Rajidae","Sillaginodes punctata"))


goal_4_dat_tb_avg = goal_4_dat_tb %>% group_by(Class) %>% summarise(MaxN = mean(MaxN))

goal_4_dat_tb_avg %>% 
  #mutate(Class = fct_reorder(Class, desc(MaxN))) %>%
  ggplot(aes(y=MaxN+1,x=Class)) + ylab("MaxN + 1 (log10 scale)") +
  geom_col() +theme_classic() + scale_y_log10(limits=c(1,15)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

tassie_raw_maxN_sum %>% 
  #mutate(species = fct_reorder(species, desc(MaxN))) %>%
  ggplot(aes(y=MaxN+1,x=species)) + ylab("MaxN + 1 (log10 scale)") +
  geom_col() +theme_classic() + scale_y_log10(limits=c(1,15)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





