#Load libraries
library(tidyverse)


#read in data (generated from Goal_1_data_prep)
goal_2_dat_tb <- read_csv("results/60_seconds/goal_2_dat_tb_long.csv")
goal_2_dat_kf <- read_csv("results/60_seconds/goal_2_dat_kf_long.csv")

#read in metadata
vid_metadata_tb         <- read_csv("data/BRUV_RUV_metadata.csv")
vid_metadata_kf         <- read_csv("data/KF_video_meta_data.csv")
species_metadata_tb     <- read_csv("data/TB_species_metadata.csv")
species_metadata_kf     <- read_csv("data/KF_species_metadata.csv")

#clean metadata
colnames(vid_metadata_tb)[1] <- "Video_name"
colnames(vid_metadata_kf)[1] <- "Video_name"


#join with metadata
goal_2_dat_tb <- left_join(goal_2_dat_tb,vid_metadata_tb,by="Video_name")
goal_2_dat_kf <- left_join(goal_2_dat_kf,vid_metadata_kf,by="Video_name")

goal_2_dat_tb <- left_join(goal_2_dat_tb,species_metadata_tb,by="Class")
goal_2_dat_kf <- left_join(goal_2_dat_kf,species_metadata_kf,by="Class")


#convert into factors
goal_2_dat_tb <- goal_2_dat_tb %>% mutate(Class = as.factor(Class), 
                                          Video_name = as.factor(Video_name),
                                          location = as.factor(location),
                                          block = as.factor(block),
                                          treatment = as.factor(treatment),
                                          bait = as.factor(bait),
                                          Mobility = as.factor(Mobility),
                                          Schooling = as.factor(Schooling),
                                          measure = as.factor(measure))
goal_2_dat_kf <- goal_2_dat_kf %>% mutate(Class = as.factor(Class), 
                                          Video_name = as.factor(Video_name),
                                          river = as.factor(river),
                                          transect = as.factor(transect),
                                          location = as.factor(location),
                                          year = as.factor(year),
                                          Mobility = as.factor(Mobility),
                                          Schooling = as.factor(Schooling),
                                          measure = as.factor(measure))



#Start with EDA
#goal_2_dat_tb <- goal_2_dat_tb %>% filter(EDA=="yes")
#goal_2_dat_kf <- goal_2_dat_kf %>% filter(EDA=="yes")
goal_2_dat_tb <- goal_2_dat_tb %>% filter(EDA=="no",QC=="PASS") %>% droplevels()
goal_2_dat_kf <- goal_2_dat_kf %>% filter(EDA=="no") %>% droplevels()




#plot against time

#plot params
maxn_tol = 0.5
a_clarity = 0.2
avgn_tol_tb = min(goal_2_dat_tb$value[goal_2_dat_tb$value!=0 & goal_2_dat_tb$measure == "AvgN"])/2
avgn_cs_tol_tb = min(goal_2_dat_tb$value[goal_2_dat_tb$value!=0 & goal_2_dat_tb$measure == "AvgN_cum_sum"])/2
avgn_tol_kf = min(goal_2_dat_kf$value[goal_2_dat_kf$value!=0 & goal_2_dat_kf$measure == "AvgN"])/2
avgn_cs_tol_kf = min(goal_2_dat_kf$value[goal_2_dat_kf$value!=0 & goal_2_dat_kf$measure == "AvgN_cum_sum"])/2

avgn_cs_tol_kf = min(avgn_cs_tol_kf,avgn_cs_tol_tb)
avgn_cs_tol_tb = min(avgn_cs_tol_kf,avgn_cs_tol_tb)

goal_2_dat_kf$value_tol <- goal_2_dat_kf$value + (goal_2_dat_kf$measure == "AvgN_cum_sum")*avgn_cs_tol_kf +
  (goal_2_dat_kf$measure == "AvgN")*avgn_tol_kf + (goal_2_dat_kf$measure == "MaxN_cum_sum")*maxn_tol

goal_2_dat_tb$value_tol <- goal_2_dat_tb$value + (goal_2_dat_tb$measure == "AvgN_cum_sum")*avgn_cs_tol_tb +
  (goal_2_dat_tb$measure == "AvgN")*avgn_tol_tb + (goal_2_dat_tb$measure == "MaxN_cum_sum")*maxn_tol


#join them together

goal_2_dat_tb_sum <- goal_2_dat_tb %>% group_by(measure,bait,split_number) %>% summarise(value = mean(value_tol))
goal_2_dat_tb_sum_sp <- goal_2_dat_tb %>% group_by(measure,bait,split_number,Class) %>% summarise(value = mean(value_tol))

goal_2_dat_kf_sum <- goal_2_dat_kf %>% group_by(measure,split_number) %>% summarise(value = mean(value_tol))
goal_2_dat_kf_sum_sp <- goal_2_dat_kf %>% group_by(measure,split_number,Class) %>% summarise(value = mean(value_tol))


goal_2_dat_kf$group <- "Kakadu - unbaited"
goal_2_dat_tb$group <- "Tassie - unbaited"
goal_2_dat_tb$group[goal_2_dat_tb$bait=="BRUV"] <- "Tassie - baited"

goal_2_dat_kf_sum$group <- "Kakadu - unbaited"
goal_2_dat_tb_sum$group <- "Tassie - unbaited"
goal_2_dat_tb_sum$group[goal_2_dat_tb_sum$bait=="BRUV"] <- "Tassie - baited"

goal_2_dat_kf_sum_sp$group <- "Kakadu - unbaited"
goal_2_dat_tb_sum_sp$group <- "Tassie - unbaited"
goal_2_dat_tb_sum_sp$group[goal_2_dat_tb_sum_sp$bait=="BRUV"] <- "Tassie - baited"



goal_2_dat_kf_small <- goal_2_dat_kf %>% dplyr::select(split_number,value_tol,Video_name,Class,measure,group) %>% filter(measure!="AvgN") %>% droplevels()
goal_2_dat_tb_small <- goal_2_dat_tb %>% dplyr::select(split_number,value_tol,Video_name,Class,measure,group) %>% filter(measure!="AvgN") %>% droplevels()

goal_2_dat_combined <- rbind(goal_2_dat_kf_small,goal_2_dat_tb_small)

goal_2_dat_kf_sum_small <- goal_2_dat_kf_sum %>% dplyr::select(split_number,value,measure,group) %>% filter(measure!="AvgN") %>% droplevels()
goal_2_dat_tb_sum_small <- goal_2_dat_tb_sum %>% dplyr::select(split_number,value,measure,group) %>% filter(measure!="AvgN") %>% droplevels()

goal_2_dat_sum_combined <- rbind(goal_2_dat_kf_sum_small,goal_2_dat_tb_sum_small)

goal_2_dat_kf_sum_sp_small <- goal_2_dat_kf_sum_sp %>% dplyr::select(split_number,value,Class,measure,group) %>% filter(measure!="AvgN") %>% droplevels()
goal_2_dat_tb_sum_sp_small <- goal_2_dat_tb_sum_sp %>% dplyr::select(split_number,value,Class,measure,group) %>% filter(measure!="AvgN") %>% droplevels()

goal_2_dat_sum_sp_combined <- rbind(goal_2_dat_kf_sum_sp_small,goal_2_dat_tb_sum_sp_small)

goal_2_dat_combined$group <- factor(goal_2_dat_combined$group, levels = c("Tassie - baited","Tassie - unbaited","Kakadu - unbaited")) 
levels(goal_2_dat_combined$measure) <- c("MeanCount","MaxN")

goal_2_dat_sum_combined$group <- factor(goal_2_dat_sum_combined$group, levels = c("Tassie - baited","Tassie - unbaited","Kakadu - unbaited")) 
levels(goal_2_dat_sum_combined$measure) <- c("MeanCount","MaxN")

goal_2_dat_sum_sp_combined$group <- factor(goal_2_dat_sum_sp_combined$group, levels = c("Tassie - baited","Tassie - unbaited","Kakadu - unbaited")) 
levels(goal_2_dat_sum_sp_combined$measure) <- c("MeanCount","MaxN")

cols <- c("Overall Avg" = "#66C2A5","Avg per species" = "#8DA0CB", "Video:Species" = "#FC8D62")

# ggplot(goal_2_dat_combined,aes(x=split_number,y=value_tol,colour="Video:Species",group=Video_name:Class)) + 
#   geom_line(alpha=0.25)  + scale_y_log10() + facet_grid(measure~group,scales = "free",switch="y") +
#   geom_line(data=goal_2_dat_sum_combined,mapping=aes(x=split_number,y=value,group=measure,colour="Overall Avg"),
#             size=1.6) + 
#   geom_line(data=goal_2_dat_sum_sp_combined,mapping=aes(x=split_number,y=value,group=measure:Class,colour="Avg per species"),
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




#read in simulation data
sim_goal_2_dat <- read.csv("results/60_seconds/sim_goal_2_dat.csv")

#clean data
sim_goal_2_dat$Video_name <- factor(sim_goal_2_dat$Video_name)
sim_goal_2_dat$group <- "Simulations - unbaited"



maxn_tol_sim = 0.5
meancount_tol_sim = min(sim_goal_2_dat$value[sim_goal_2_dat$value!=0 & sim_goal_2_dat$measure == "meancount"])/2
meancount_tol_sim = min(meancount_tol_sim,avgn_cs_tol_kf,avgn_cs_tol_tb)

sim_goal_2_dat$value_tol <- sim_goal_2_dat$value + 
  (sim_goal_2_dat$measure == "meancount")*meancount_tol_sim + (sim_goal_2_dat$measure == "MaxN")*maxn_tol_sim


sim_goal_2_dat <- sim_goal_2_dat %>% dplyr::select(split_number,value_tol,Video_name,Class,measure,group)

sim_goal_2_dat$measure <- factor(sim_goal_2_dat$measure)
levels(sim_goal_2_dat$measure) <- c("MaxN","MeanCount")
sim_goal_2_dat$Class <- factor(sim_goal_2_dat$Class)

sim_goal_2_dat_sum <- sim_goal_2_dat %>% group_by(measure,split_number,group) %>% summarise(value = mean(value_tol))
sim_goal_2_dat_sum_sp <- sim_goal_2_dat %>% group_by(measure,split_number,Class,group) %>% summarise(value = mean(value_tol))

#join data

goal_2_dat_combined_sims <- rbind(goal_2_dat_combined,sim_goal_2_dat)
goal_2_dat_sum_combined_sims <- rbind(goal_2_dat_sum_combined,sim_goal_2_dat_sum)
goal_2_dat_sum_sp_combined_sims <- rbind(goal_2_dat_sum_sp_combined,sim_goal_2_dat_sum_sp)

goal_2_dat_combined_sims$group <- factor(goal_2_dat_combined_sims$group,levels=c("Tassie - baited","Tassie - unbaited","Kakadu - unbaited","Simulations - unbaited"))
goal_2_dat_sum_combined_sims$group <- factor(goal_2_dat_sum_combined_sims$group,levels=c("Tassie - baited","Tassie - unbaited","Kakadu - unbaited","Simulations - unbaited"))
goal_2_dat_sum_sp_combined_sims$group <- factor(goal_2_dat_sum_sp_combined_sims$group,levels=c("Tassie - baited","Tassie - unbaited","Kakadu - unbaited","Simulations - unbaited"))

#filter to only use 100 videos per group.
set.seed(1234)
goal_2_dat_combined_sims %>% group_by(measure,group) %>% summarise(length(unique(Video_name:Class)))

#number of Videos:Species to include
n_vs <- 100

Tassie_sp_vids <- goal_2_dat_combined_sims %>% filter(group=="Tassie - baited") %>%
  droplevels() %>% mutate(samp_levels=Video_name:Class) %>% dplyr::select(samp_levels) %>% 
  unique() %>% unlist() %>% sample(size=n_vs) %>% droplevels() %>% levels()

Tassie__unb_sp_vids <- goal_2_dat_combined_sims %>% filter(group=="Tassie - unbaited") %>%
  droplevels() %>% mutate(samp_levels=Video_name:Class) %>% dplyr::select(samp_levels) %>% 
  unique() %>% unlist() %>% sample(size=n_vs) %>% droplevels() %>% levels()

Kakadu_sp_vids <- goal_2_dat_combined_sims %>% filter(group=="Kakadu - unbaited") %>%
  droplevels() %>% mutate(samp_levels=Video_name:Class) %>% dplyr::select(samp_levels) %>% 
  unique() %>% unlist() %>% sample(size=n_vs) %>% droplevels() %>% levels()

Sim_sp_vids <- goal_2_dat_combined_sims %>% filter(group=="Simulations - unbaited") %>%
  droplevels() %>% mutate(samp_levels=Video_name:Class) %>% dplyr::select(samp_levels) %>% 
  unique() %>% unlist() %>% sample(size=n_vs) %>% droplevels() %>% levels()


goal_2_dat_combined_sims_filt <- goal_2_dat_combined_sims %>% filter(Video_name:Class %in% c(Tassie_sp_vids,Tassie__unb_sp_vids,
                                                                                             Kakadu_sp_vids,Sim_sp_vids))
levels(goal_2_dat_combined_sims_filt$measure) <- c("MeanCount (log)","MaxN (log)")
levels(goal_2_dat_sum_combined_sims$measure) <- c("MeanCount (log)","MaxN (log)")
levels(goal_2_dat_sum_sp_combined_sims$measure) <- c("MeanCount (log)","MaxN (log)")

#plot the data

cols <- c("Overall Avg" = "#66C2A5","Avg per species" = "#8DA0CB", "Video:Species" = "#FC8D62")

ggplot(goal_2_dat_combined_sims_filt,aes(x=split_number,y=value_tol,colour="Video:Species",group=Video_name:Class)) + 
  geom_line(alpha=0.3,size=0.15)  + scale_y_log10() + facet_grid(measure~group,scales = "free",switch="y") +
  geom_line(data=goal_2_dat_sum_sp_combined_sims,mapping=aes(x=split_number,y=value,group=measure:Class,colour="Avg per species"),
            size=0.41,alpha=0.7)+ theme_bw() + guides(colour=guide_legend(title="Legend")) + 
  geom_line(data=goal_2_dat_sum_combined_sims,mapping=aes(x=split_number,y=value,group=measure,colour="Overall Avg"),
            size=1.65) + 
  xlab("video length (minutes)") + ylab("value") + 
  scale_color_manual(values = cols,limits=c("Overall Avg","Avg per species","Video:Species"),
                     labels = c("Overall Avg","Avg per species","Video:Species")) +
  theme(axis.title.y=element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title  = element_blank(),
        strip.text.x = element_text(hjust=0.5,size=10))

ggsave("plots/60_seconds/abund_video_length_sims.png",height=1.1*5.12,width=1.1*5.79*(0.98/0.62))



