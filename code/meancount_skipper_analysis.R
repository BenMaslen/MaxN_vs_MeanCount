library(tidyverse)

#read in data
KF_am <- read.csv("data/KF_meancount_skipper/abundance_metrics.csv")
KF_am_1 <- read.csv("data/KF_meancount_skipper/abundance_metrics_1_seconds.csv")
KF_am_2 <- read.csv("data/KF_meancount_skipper/abundance_metrics_2_seconds.csv")
KF_am_5 <- read.csv("data/KF_meancount_skipper/abundance_metrics_5_seconds.csv")
KF_am_10 <- read.csv("data/KF_meancount_skipper/abundance_metrics_10_seconds.csv")
KF_am_15 <- read.csv("data/KF_meancount_skipper/abundance_metrics_15_seconds.csv")
KF_am_20 <- read.csv("data/KF_meancount_skipper/abundance_metrics_20_seconds.csv")
KF_am_30 <- read.csv("data/KF_meancount_skipper/abundance_metrics_30_seconds.csv")
KF_am_60 <- read.csv("data/KF_meancount_skipper/abundance_metrics_60_seconds.csv")
KF_am_120 <- read.csv("data/KF_meancount_skipper/abundance_metrics_120_seconds.csv")
KF_am_300 <- read.csv("data/KF_meancount_skipper/abundance_metrics_300_seconds.csv")


TB_am <- read.csv("data/TB_meancount_skipper/abundance_metrics.csv")
TB_am_1 <- read.csv("data/TB_meancount_skipper/abundance_metrics_1_seconds.csv")
TB_am_2 <- read.csv("data/TB_meancount_skipper/abundance_metrics_2_seconds.csv")
TB_am_5 <- read.csv("data/TB_meancount_skipper/abundance_metrics_5_seconds.csv")
TB_am_10 <- read.csv("data/TB_meancount_skipper/abundance_metrics_10_seconds.csv")
TB_am_15 <- read.csv("data/TB_meancount_skipper/abundance_metrics_15_seconds.csv")
TB_am_20 <- read.csv("data/TB_meancount_skipper/abundance_metrics_20_seconds.csv")
TB_am_30 <- read.csv("data/TB_meancount_skipper/abundance_metrics_30_seconds.csv")
TB_am_60 <- read.csv("data/TB_meancount_skipper/abundance_metrics_60_seconds.csv")
TB_am_120 <- read.csv("data/TB_meancount_skipper/abundance_metrics_120_seconds.csv")
TB_am_300 <- read.csv("data/TB_meancount_skipper/abundance_metrics_300_seconds.csv")

#clean data

colnames(KF_am)[2] <- "meancount"
colnames(KF_am_1)[2] <- "meancount_1"
colnames(KF_am_2)[2] <- "meancount_2"
colnames(KF_am_5)[2] <- "meancount_5"
colnames(KF_am_10)[2] <- "meancount_10"
colnames(KF_am_15)[2] <- "meancount_15"
colnames(KF_am_20)[2] <- "meancount_20"
colnames(KF_am_30)[2] <- "meancount_30"
colnames(KF_am_60)[2] <- "meancount_60"
colnames(KF_am_120)[2] <- "meancount_120"
colnames(KF_am_300)[2] <- "meancount_300"

colnames(TB_am)[2] <- "meancount"
colnames(TB_am_1)[2] <- "meancount_1"
colnames(TB_am_2)[2] <- "meancount_2"
colnames(TB_am_5)[2] <- "meancount_5"
colnames(TB_am_10)[2] <- "meancount_10"
colnames(TB_am_15)[2] <- "meancount_15"
colnames(TB_am_20)[2] <- "meancount_20"
colnames(TB_am_30)[2] <- "meancount_30"
colnames(TB_am_60)[2] <- "meancount_60"
colnames(TB_am_120)[2] <- "meancount_120"
colnames(TB_am_300)[2] <- "meancount_300"

#filter out problem video
KF_am <- KF_am %>% filter(Video_name!="2016_Channels_2016_Sandy_Videos_Sandy_Transect_5_Location_1_Camera_14")
KF_am_1 <- KF_am_1 %>% filter(Video_name!="2016_Channels_2016_Sandy_Videos_Sandy_Transect_5_Location_1_Camera_14")
KF_am_2 <- KF_am_2 %>% filter(Video_name!="2016_Channels_2016_Sandy_Videos_Sandy_Transect_5_Location_1_Camera_14")
KF_am_5 <- KF_am_5 %>% filter(Video_name!="2016_Channels_2016_Sandy_Videos_Sandy_Transect_5_Location_1_Camera_14")
KF_am_10 <- KF_am_10 %>% filter(Video_name!="2016_Channels_2016_Sandy_Videos_Sandy_Transect_5_Location_1_Camera_14")
KF_am_15 <- KF_am_15 %>% filter(Video_name!="2016_Channels_2016_Sandy_Videos_Sandy_Transect_5_Location_1_Camera_14")
KF_am_20 <- KF_am_20 %>% filter(Video_name!="2016_Channels_2016_Sandy_Videos_Sandy_Transect_5_Location_1_Camera_14")
KF_am_30 <- KF_am_30 %>% filter(Video_name!="2016_Channels_2016_Sandy_Videos_Sandy_Transect_5_Location_1_Camera_14")
KF_am_60 <- KF_am_60 %>% filter(Video_name!="2016_Channels_2016_Sandy_Videos_Sandy_Transect_5_Location_1_Camera_14")
KF_am_120 <- KF_am_120 %>% filter(Video_name!="2016_Channels_2016_Sandy_Videos_Sandy_Transect_5_Location_1_Camera_14")
KF_am_300 <- KF_am_300 %>% filter(Video_name!="2016_Channels_2016_Sandy_Videos_Sandy_Transect_5_Location_1_Camera_14")



#join data

KF_all <- KF_am %>% 
  left_join(KF_am_1,by=c("Class","Video_name")) %>%
  left_join(KF_am_2,by=c("Class","Video_name")) %>%
  left_join(KF_am_5,by=c("Class","Video_name")) %>%
  left_join(KF_am_10,by=c("Class","Video_name")) %>%
  left_join(KF_am_15,by=c("Class","Video_name")) %>%
  left_join(KF_am_20,by=c("Class","Video_name")) %>%
  left_join(KF_am_30,by=c("Class","Video_name")) %>%
  left_join(KF_am_60,by=c("Class","Video_name")) %>%
  left_join(KF_am_120,by=c("Class","Video_name")) %>%
  left_join(KF_am_300,by=c("Class","Video_name"))

TB_all <- TB_am %>% 
  left_join(TB_am_1,by=c("Class","Video_name")) %>%
  left_join(TB_am_2,by=c("Class","Video_name")) %>%
  left_join(TB_am_5,by=c("Class","Video_name")) %>%
  left_join(TB_am_10,by=c("Class","Video_name")) %>%
  left_join(TB_am_15,by=c("Class","Video_name")) %>%
  left_join(TB_am_20,by=c("Class","Video_name")) %>%
  left_join(TB_am_30,by=c("Class","Video_name")) %>%
  left_join(TB_am_60,by=c("Class","Video_name")) %>%
  left_join(TB_am_120,by=c("Class","Video_name")) %>%
  left_join(TB_am_300,by=c("Class","Video_name"))

KF_all$data <- "KF"
TB_all$data <- "TB"

skip_dat <- rbind(KF_all,TB_all)

#long format
skip_dat_long <- skip_dat %>% pivot_longer(cols = starts_with("meancount_"),names_to = "meancount_n_seconds",values_to = "meancount_est")
skip_dat_long$diff <- skip_dat_long$meancount - skip_dat_long$meancount_est


skip_dat_long <- skip_dat_long  %>% filter(meancount!=0)

skip_dat_long$meancount_n_seconds <- factor(skip_dat_long$meancount_n_seconds,levels=c("meancount_1","meancount_2",
                                                                                       "meancount_5","meancount_10",                                                                                  skip_dat_long                                                                                       "meancount_15","meancount_20",
                                                                                       "meancount_30","meancount_60",
                                                                                       "meancount_120","meancount_300"))

levels(skip_dat_long$meancount_n_seconds) <- c("1","2","5","10","15","20","30","60","120","300")

ggplot(skip_dat_long,aes(y=diff,x=meancount_n_seconds)) + geom_jitter(alpha=0.2,size = 0.5) +
  geom_hline(yintercept=sd(skip_dat$meancount),linetype="dashed", color = "red") +
  geom_hline(yintercept=-sd(skip_dat$meancount),linetype="dashed", color = "red") + 
  geom_hline(yintercept=2*sd(skip_dat$meancount),linetype="dashed", color = "red") + 
  geom_hline(yintercept=-2*sd(skip_dat$meancount),linetype="dashed", color = "red") + 
  geom_hline(yintercept=0, color = "black") + 
  annotate(geom="text", x=0.55, y=sd(skip_dat$meancount)+0.03, label="1sd",
           color="red",size=2.5)+
  annotate(geom="text", x=0.55, y=2*sd(skip_dat$meancount)+0.03, label="2sd",
           color="red",size=2.5)+
  annotate(geom="text", x=0.55, y=-2*sd(skip_dat$meancount)+0.03, label="2sd",
           color="red",size=2.5)+
  annotate(geom="text", x=0.55, y=-sd(skip_dat$meancount)+0.03, label="1sd",
           color="red",size=2.5)+
  theme_classic()


ggsave("plots/meancount_skipper.png",height=5.12,width=5.79*(0.98/0.62))

#percentages less then 1 and 2sd
skip_dat_long %>% group_by(meancount_n_seconds) %>% summarise(mean_diff = mean(diff),perc_less1sd=mean(abs(diff)<sd(skip_dat$meancount)),
                                                              perc_less2sd=mean(abs(diff)<2*sd(skip_dat$meancount)))



