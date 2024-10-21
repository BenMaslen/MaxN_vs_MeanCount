#Load libraries
library(tidyverse)


#read in data (generated from Goal_1_data_prep)
goal_1_dat_tb <- read_csv("results/30_seconds/goal_1_2_4_dat_tb.csv")
goal_1_dat_kf <- read_csv("results/30_seconds/goal_1_2_4_dat_kf.csv")

#read in metadata
vid_metadata_tb         <- read_csv("data/BRUV_RUV_metadata.csv")
vid_metadata_kf         <- read_csv("data/KF_video_meta_data.csv")
species_metadata_tb     <- read_csv("data/TB_species_metadata.csv")
species_metadata_kf     <- read_csv("data/KF_species_metadata.csv")

#clean metadata
colnames(vid_metadata_tb)[1] <- "Video_name"
colnames(vid_metadata_kf)[1] <- "Video_name"


#join with metadata
goal_1_dat_tb <- left_join(goal_1_dat_tb,vid_metadata_tb,by="Video_name")
goal_1_dat_kf <- left_join(goal_1_dat_kf,vid_metadata_kf,by="Video_name")

goal_1_dat_tb <- left_join(goal_1_dat_tb,species_metadata_tb,by="Class")
goal_1_dat_kf <- left_join(goal_1_dat_kf,species_metadata_kf,by="Class")

#convert into factors
goal_1_dat_tb <- goal_1_dat_tb %>% mutate(Class = as.factor(Class), 
                                          Video_name = as.factor(Video_name),
                                          location = as.factor(location),
                                          block = as.factor(block),
                                          treatment = as.factor(treatment),
                                          bait = as.factor(bait),
                                          Mobility = as.factor(Mobility),
                                          Schooling = as.factor(Schooling))
goal_1_dat_kf <- goal_1_dat_kf %>% mutate(Class = as.factor(Class), 
                                          Video_name = as.factor(Video_name),
                                          river = as.factor(river),
                                          transect = as.factor(transect),
                                          location = as.factor(location),
                                          year = as.factor(year),
                                          Mobility = as.factor(Mobility),
                                          Schooling = as.factor(Schooling))

#Start with EDA
#goal_1_dat_tb <- goal_1_dat_tb %>% filter(EDA=="yes")
goal_1_dat_tb <- goal_1_dat_tb %>% filter(EDA=="no",QC=="PASS")
#goal_1_dat_kf <- goal_1_dat_kf %>% filter(EDA=="yes")
goal_1_dat_kf <- goal_1_dat_kf %>% filter(EDA=="no")
                                          
                                        

#combined plot
goal_1_dat_kf_small <- goal_1_dat_kf %>% dplyr::select(AvgN,MaxN,Mobility,Schooling)
goal_1_dat_tb_small <- goal_1_dat_tb %>% dplyr::select(AvgN,MaxN,Mobility,Schooling)

goal_1_dat_combined <- rbind(goal_1_dat_kf_small,goal_1_dat_tb_small)
goal_1_dat_combined$data <- rep(c("Kakadu", "Tassie"),c(nrow(goal_1_dat_kf_small),nrow(goal_1_dat_tb_small)))


#read in simulation data
goal_1_sim_dat <- read.csv("results/30_seconds/simulation_goal1_dat.csv")

goal_1_sim_dat_small <- goal_1_sim_dat %>% dplyr::select(AvgN,MaxN,Mobility,Schooling)

goal_1_sim_dat_small$data <- "Simulations"

goal_1_sim_dat_small$Mobility <- factor(goal_1_sim_dat_small$Mobility)
goal_1_sim_dat_small$Schooling <- factor(goal_1_sim_dat_small$Schooling)

levels(goal_1_sim_dat_small$Mobility) <- c("high","low")
levels(goal_1_sim_dat_small$Schooling) <- c("high","low")

#combine the plot with simulation data

goal_1_dat_combined <- rbind(goal_1_dat_combined,goal_1_sim_dat_small)

goal_1_dat_combined$data <- factor(goal_1_dat_combined$data,levels=c("Tassie","Kakadu","Simulations"))


goal_1_dat_combined$AvgN_tol = goal_1_dat_combined$AvgN+min(goal_1_dat_combined$AvgN[goal_1_dat_combined$AvgN!=0])/2
goal_1_dat_combined$MaxN_tol = goal_1_dat_combined$MaxN+0.5


ggplot(goal_1_dat_combined,aes(x=AvgN_tol,y=jitter(MaxN,1.7),colour = Schooling:Mobility)) + geom_point(alpha=0.3,size=0.75) +
  scale_y_log10(limits=c(0.8,50)) + scale_x_log10() + scale_color_brewer(palette = "Set2") + ylab("MaxN (log)") +
  theme_bw()+ xlab("MeanCount (log)") +
  facet_grid(~data,scales="free") + theme(strip.background = element_blank(),
                                          strip.placement = "outside",
                                          panel.grid.major = element_blank(),
                                          panel.grid.minor = element_blank(),
                                          strip.text.x = element_text(hjust=0.5,size=11),
                                          legend.position = "bottom")


ggsave("plots/30_seconds/MaxN_AvgN_scatter_sims.png",height=1.1*5.12,width=1.1*5.79*(0.98/0.62))



