#Load libraries
library(tidyverse)
library(patchwork)
library(RColorBrewer)
library(ggh4x)

#read in raw data (generated from Goal_1_data_prep)
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


goal_4_dat_tb <- goal_4_dat_tb %>% filter(EDA=="no",QC=="PASS") %>% droplevels()
goal_4_dat_kf <- goal_4_dat_kf %>% filter(EDA=="no") %>% droplevels()


#Plotting

#plot params
maxn_tol = 0.5
avgn_tol_kf = min(goal_4_dat_kf$value[goal_4_dat_kf$value!=0&goal_4_dat_kf$measure=="AvgN"])/2 #half the minimum non-zero - paper 
avgn_tol_tb = min(goal_4_dat_tb$value[goal_4_dat_tb$value!=0&goal_4_dat_tb$measure=="AvgN"])/2 #half the minimum non-zero - paper 

abundant_species_list_kf_dat <- goal_4_dat_kf %>% group_by(Class) %>% filter(measure=="AvgN") %>%
  summarise(AvgN=mean(value)) %>% 
  arrange(AvgN) %>% filter(AvgN > 0.0014)%>% droplevels()
abundant_species_list_kf <- abundant_species_list_kf_dat$Class

#apply the tol
goal_4_dat_kf$value_tol <- goal_4_dat_kf$value+(goal_4_dat_kf$measure=="AvgN")*avgn_tol_kf +
  (goal_4_dat_kf$measure=="MaxN")*maxn_tol

goal_4_dat_tb$value_tol <- goal_4_dat_tb$value+(goal_4_dat_tb$measure=="AvgN")*avgn_tol_tb +
  (goal_4_dat_tb$measure=="MaxN")*maxn_tol

#read in test statistic dat
test_statistic_dat_kf = read.csv("results/goal_4_test_statistic_dat_kf.csv")  
test_statistic_dat_tb = read.csv("results/goal_4_test_statistic_dat_tb.csv")  

test_statistic_dat_kf <- test_statistic_dat_kf %>% dplyr::select(Class,MaxN,AvgN,diff)
test_statistic_dat_tb <- test_statistic_dat_tb %>% dplyr::select(Class,MaxN,AvgN,diff)

test_statistic_dat_kf <- left_join(test_statistic_dat_kf,species_metadata_kf,by="Class")
test_statistic_dat_tb <- left_join(test_statistic_dat_tb,species_metadata_tb,by="Class")

test_statistic_dat_kf <- test_statistic_dat_kf %>% mutate(Schooling = factor(Schooling),
                                                          Mobility = factor(Mobility))

test_statistic_dat_tb <- test_statistic_dat_tb %>% mutate(Schooling = factor(Schooling),
                                                          Mobility = factor(Mobility))

#read in p-value dat

p_values_dat_kf <- read.csv("results/p_values_dat_kf.csv")
p_values_dat_tb <- read.csv("results/p_values_dat_tb.csv")

p_values_dat_kf$Class <- p_values_dat_kf$species
p_values_dat_tb$Class <- p_values_dat_tb$species

p_values_dat_kf <- left_join(p_values_dat_kf,species_metadata_kf,by="Class")
p_values_dat_tb <- left_join(p_values_dat_tb,species_metadata_tb,by="Class")

p_values_dat_kf <- p_values_dat_kf %>% mutate(Schooling = factor(Schooling),
                                                          Mobility = factor(Mobility))

p_values_dat_tb <- p_values_dat_tb %>% mutate(Schooling = factor(Schooling),
                                                          Mobility = factor(Mobility))



#read in simulation power curve data
pow_4 <- read.csv("data/power_dat/power_10_20_4_1000.csv")
pow_8 <- read.csv("data/power_dat/power_10_20_8_1000.csv")
pow_12 <- read.csv("data/power_dat/power_10_20_12_1000.csv")
pow_16 <- read.csv("data/power_dat/power_10_20_16_1000.csv")
pow_20 <- read.csv("data/power_dat/power_10_20_20_1000.csv")
pow_24 <- read.csv("data/power_dat/power_10_20_24_1000.csv")
pow_28 <- read.csv("data/power_dat/power_10_20_28_1000.csv")
pow_32 <- read.csv("data/power_dat/power_10_20_32_1000.csv")
pow_36 <- read.csv("data/power_dat/power_10_20_36_1000.csv")
pow_40 <- read.csv("data/power_dat/power_10_20_40_1000.csv")


#join data
power_dat <- rbind(pow_4,pow_8,pow_12,pow_16,pow_20,
                   pow_24,pow_28,pow_32,pow_36,pow_40)

power_dat$data <- rep(seq(4,40,4),each=8)

power_dat$measure <- factor(power_dat$measure,levels=c("MaxN","meancount"))
levels(power_dat$measure) <- c("MaxN","MeanCount")

power_dat_mean <- power_dat %>% group_by(measure,data) %>% summarise(power=mean(power))


#read in simulation boxplot data
raw_12 <- read.csv("data/power_dat/raw_data_10_20_12_1000.csv")

min_X = (raw_12 %>% group_by(Mobility,Schooling,means) %>% summarise(min_X=min(X)))$min_X
num_samps = 40


raw_12_filt <- rbind(raw_12 %>% filter(X>=min_X[1],X<min_X[1]+num_samps),
                     raw_12 %>% filter(X>=min_X[2],X<min_X[2]+num_samps),
                     raw_12 %>% filter(X>=min_X[3],X<min_X[3]+num_samps),
                     raw_12 %>% filter(X>=min_X[4],X<min_X[4]+num_samps),
                     raw_12 %>% filter(X>=min_X[5],X<min_X[5]+num_samps),
                     raw_12 %>% filter(X>=min_X[6],X<min_X[6]+num_samps),
                     raw_12 %>% filter(X>=min_X[7],X<min_X[7]+num_samps),
                     raw_12 %>% filter(X>=min_X[8],X<min_X[8]+num_samps))

raw_12_clean <- raw_12_filt %>% mutate(Mobility = factor(Mobility),
                                       Schooling = factor(Schooling),
                                       means = factor(means),
                                       MaxN = MaxN + 0.5,
                                       MeanCount = MeanCount + min(MeanCount[MeanCount!=0])/2)



raw_12_long <- raw_12_clean %>% pivot_longer(cols=c(MaxN,MeanCount),names_to = "Measure",values_to = "Value")


raw_12_long$Measure <- factor(raw_12_long$Measure,levels=c("MeanCount","MaxN"))


#Create list of goal_4_plots to join together
goal_4_plots <- list()

### Test Statistic Plots
# 
# test_stat_plots <- list()
# 
# test_stat_plots[[1]] <- ggplot(test_statistic_dat_kf, aes(y = diff,x=1,colour=Schooling:Mobility)) +
#                           geom_violin(color="black",linewidth=0.1,alpha=0.5) + ylab("MeanCount - MaxN (Chi.Sq Test Statistic)") +
#                           geom_point(size = 1.5) +scale_color_brewer(palette = "Dark2")+
#                           theme_classic()+ylim(-15,35)+
#                           geom_hline(yintercept = 0,colour="red",linewidth=0.2,alpha=0.5)+
#                           ggtitle("Kakadu")+
#                           theme(axis.title.x=element_blank(),
#                                 axis.text.x=element_blank(),
#                                 axis.ticks.x=element_blank(),
#                                 plot.title=element_text(size=12,hjust=0.5))
# 
# test_stat_plots[[1]]
# 
# test_stat_plots[[2]] <- ggplot(test_statistic_dat_tb, aes(y = diff,x=1,colour=Schooling:Mobility)) +
#                           geom_violin(color="black",linewidth=0.1,alpha=0.5) + ylab("MeanCount - MaxN (Chi.Sq Test Statistic)") +
#                           geom_point(size = 1.5) +scale_color_brewer(palette = "Dark2",guide="none")+
#                           theme_classic()+ylim(-15,35)+
#                           geom_hline(yintercept = 0,colour="red",linewidth=0.2,alpha=0.5)+
#                           ggtitle("Tassie")+
#                           theme(axis.title.x=element_blank(),
#                                 axis.text.x=element_blank(),
#                                 axis.ticks.x=element_blank(),
#                                 axis.ticks.y=element_blank(),
#                                 axis.line.y = element_blank(),
#                                 axis.text.y=element_blank(),
#                                 legend.position = "none",
#                                 plot.title=element_text(size=12,hjust=0.5))
# 
# test_stat_plots[[2]]
# 
# 
# wrap_plots(plotlist = test_stat_plots, ncol = 2,nrow=1,guides ='collect') + plot_layout(axis_titles = "collect") & theme(legend.position = 'bottom')
# 
# ggsave("plots/All_test_stats.png",height=1.1*5.12,width=1.1*5.79*(0.98/0.62))


#test statistic plot

test_stat_plots_alt <- list()

test_stat_plots_alt[[2]] <- ggplot(test_statistic_dat_kf, aes(y = MaxN,x=AvgN,colour=Schooling:Mobility)) + 
  geom_point(size=1) + xlab(parse(text = expression({chi^2}~statistic ~~(MeanCount))))  +  
  scale_y_continuous(limits=c(0,35),expand=c(0.025,0.025)) +
  scale_x_continuous(limits=c(0,35),expand=c(0.025,0.025)) +
  ylab(parse(text = expression({chi^2}~statistic ~~(MaxN)))) +  
  theme_classic()+  geom_abline(colour="black",linewidth=0.2,alpha=0.5) + 
  geom_hline(yintercept = 7.815,linetype="dashed",linewidth=0.3,alpha=0.5,color="#a590ad")+
  geom_vline(xintercept = 7.815,linetype="dashed",linewidth=0.3,alpha=0.5,color="#3e6478")+
  annotate(geom="text", x=7.815+1.3, y=31, label="p [MeanCount] < 0.05",
           color="#3e6478",size=3.5,parse=TRUE,angle=270)+
  annotate(geom="text", x=7.815-1.3, y=31, label="p [MeanCount] > 0.05",
           color="#3e6478",size=3.5,parse=TRUE,angle=270)+
  annotate(geom="text", x=31, y=7.815+0.9, label="p [MaxN] < 0.05",
           color="#a590ad",size=3.5,parse=TRUE)+
  annotate(geom="text", x=31, y=7.815-0.9, label="p [MaxN] > 0.05",
           color="#a590ad",size=3.5,parse=TRUE)+
  theme(legend.position = "bottom",
        plot.title=element_text(size=12,hjust=0.5),
        axis.title.y = element_blank()) + 
  ggtitle("Kakadu") + scale_color_brewer(palette = "Dark2")

goal_4_plots[[2]] <- test_stat_plots_alt[[2]]

#test_stat_plots_alt[[2]]


test_stat_plots_alt[[1]] <- ggplot(test_statistic_dat_tb, aes(y = MaxN,x=AvgN,colour=Schooling:Mobility)) + 
  geom_jitter(width=0.15,height=0.15,size=1) + xlab(parse(text = expression({chi^2}~statistic ~~(MeanCount))))  +  
  scale_y_continuous(limits=c(0,20),expand=c(0.02,0.02)) +
  scale_x_continuous(limits=c(0,20),expand=c(0.02,0.02)) +
  ylab(parse(text = expression({chi^2}~statistic ~~(MaxN)))) +  
  theme_classic()+  geom_abline(colour="black",linewidth=0.2,alpha=0.5) + 
  geom_hline(yintercept = 9.488,linetype="dashed",linewidth=0.3,alpha=0.5,color="#a590ad")+
  geom_vline(xintercept = 9.488,linetype="dashed",linewidth=0.3,alpha=0.5,color="#3e6478")+
  annotate(geom="text", x=9.488+0.8, y=17.5, label="p [MeanCount] < 0.05",
           color="#3e6478",size=3.5,parse=TRUE,angle=270)+
  annotate(geom="text", x=17.5, y=9.488+0.5, label="p [MaxN] < 0.05",
           color="#a590ad",size=3.5,parse=TRUE)+
  annotate(geom="text", x=9.488-0.8, y=17.5, label="p [MeanCount] > 0.05",
           color="#3e6478",size=3.5,parse=TRUE,angle=270)+
  annotate(geom="text", x=17.5, y=9.488-0.5, label="p [MaxN] > 0.05",
           color="#a590ad",size=3.5,parse=TRUE)+
  theme(legend.position = "bottom",
        plot.title=element_text(size=12,hjust=0.5)) + 
  ggtitle("Tassie") + scale_color_manual(values = brewer.pal(n = 4, name = "Dark2")[c(1,3,4)],guide="none")

goal_4_plots[[1]] <- test_stat_plots_alt[[1]]
goal_4_plots[[1]]
#test_stat_plots_alt[[1]]

goal_4_plots[[1]] + geom_jitter(width=1)
#wrap_plots(plotlist = test_stat_plots_alt, ncol = 2,nrow=1,guides ='collect') + plot_layout(axis_titles = "collect") & theme(legend.position = 'bottom')

#ggsave("plots/Test_stat_alt.png",height=1.1*5.12,width=1.1*5.79*(0.98/0.62))



### P-value Plots

# p_val_plots <- list()
# 
# 
# # cols_rect <- c("MeanCount <0.05" = "#66C2A5","MaxN <0.05" = "#FC8D62")
# # 
# # rect_df <- data.frame(xmin = c(0, 0),ymin  = c(0,0), xmax = c(0.05, 1),ymax=c(1,0.05),
# #                       labs = c("MeanCount <0.05", "MaxN <0.05"))
# 
# 
# p_val_plots[[1]] <- ggplot(p_values_dat_kf, aes(y = MaxN,x=AvgN,colour=Schooling:Mobility)) + 
#                       geom_point(size=1) + scale_y_log10(limits=c(1*10^(-20),NA),expand=c(0,0.5)) +
#                       scale_x_log10(limits=c(1*10^(-20),NA),expand=c(0,0.5)) + xlab("MeanCount")+
#                       theme_classic()+  geom_abline(colour="red",linewidth=0.2,alpha=0.5) + 
#                       geom_hline(yintercept = 0.05,linetype="dotted",linewidth=0.3,alpha=0.5)+
#                       geom_vline(xintercept = 0.05,linetype="dotted",linewidth=0.3,alpha=0.5)+
#                       annotate(geom="text", x=0.05-0.04965, y=1*10^(-20), label="p [MeanCount] < 0.05",
#                       color="black",size=3,parse=TRUE)+
#                       annotate(geom="text", x=1.5*10^(-19), y=0.05-0.03, label="p [MaxN] < 0.05",
#                       color="black",size=3,parse=TRUE)+
#                       #geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = labs), 
#                       #          alpha = 0.2, data = rect_df, inherit.aes = FALSE) +   
#                       # scale_fill_manual(values = cols_rect,
#                       #                   limits = c("MeanCount <0.05", "MaxN <0.05"),
#                       #                   labels = c("MeanCount <0.05", "MaxN <0.05"),
#                       #                   name = "Legend") + 
#                       theme(legend.position = "bottom",
#                             plot.title=element_text(size=12,hjust=0.5)) + 
#                       ggtitle("Kakadu") + scale_color_brewer(palette = "Dark2")
# 
# p_val_plots[[1]]
# 
# 
# p_val_plots[[2]] <- ggplot(p_values_dat_tb, aes(y = MaxN,x=AvgN,colour=Schooling:Mobility)) + 
#                       geom_point(size=1) + scale_y_log10(limits=c(6*10^(-7),NA),expand=c(0,0.03)) +
#                       scale_x_log10(limits=c(6*10^(-7),NA),expand=c(0,0.03)) + xlab("MeanCount")+
#                       theme_classic()+  geom_abline(colour="red",linewidth=0.2,alpha=0.5) +
#                       geom_hline(yintercept = 0.05,linetype="dotted",linewidth=0.3,alpha=0.5)+
#                       geom_vline(xintercept = 0.05,linetype="dotted",linewidth=0.3,alpha=0.5)+
#                       annotate(geom="text", x=0.05-0.038, y=8*10^(-7), label="p [MeanCount] < 0.05",
#                                color="black",size=3,parse=TRUE)+
#                       annotate(geom="text", x=2*10^(-6), y=0.05-0.013, label="p [MaxN] < 0.05",
#                                color="black",size=3,parse=TRUE)+
#                       # geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = labs), 
#                       #           alpha = 0.2, data = rect_df, inherit.aes = FALSE) +   
#                       # scale_fill_manual(values = cols_rect,
#                       #                   limits = c("MeanCount <0.05", "MaxN <0.05"),
#                       #                   labels = c("MeanCount <0.05", "MaxN <0.05"),
#                       #                   name = "Legend") + 
#                       theme(legend.position = "bottom",
#                             plot.title=element_text(size=12,hjust=0.5)) + 
#                       ggtitle("Tassie") + scale_color_brewer(palette = "Dark2",guide="none")
# 
# 
# p_val_plots[[2]]
# 
# 
# wrap_plots(plotlist = p_val_plots, ncol = 2,nrow=1,guides ='collect') + plot_layout(axis_titles = "collect") & theme(legend.position = 'bottom')
# 
# ggsave("plots/All_p_vals.png",height=1.1*5.12,width=1.1*5.79*(0.98/0.62))

#power curve plot

# 
# ggplot(power_dat_mean,aes(x=data,y=power,colour=measure,shape=measure)) + geom_point() + geom_line() +
#   theme_classic() + geom_hline(yintercept=0.8,linetype="dashed", color = "red") +
#   scale_color_manual(values=c("#a590ad","#3e6478")) + 
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         legend.position = "bottom",
#         legend.title  = element_blank())
# 
# 
# 
# ggsave("plots/sim_goal_4_power_curve_mean.png",height=5.12,width=5.79*(0.98/0.62))
# 



power_dat$Class <- factor(power_dat$Class)
levels(power_dat$Class) <- c("High S : High M","Low S : High M",
                             "High S : Low M","Low S : Low M")

power_dat$Class <- fct_relevel(power_dat$Class,c("High S : High M","High S : Low M",
                                                 "Low S : High M","Low S : Low M"))

goal_4_plots[[3]] <- ggplot(power_dat,aes(x=data*2,y=power,shape=measure,colour=measure)) + geom_point() + geom_line() +
  theme_classic() + #geom_hline(yintercept=0.8,linetype="dashed", color = "red") + 
  facet_wrap2(~Class,strip=strip_themed(background_x=elem_list_rect(fill=c( "#66C2A5", "#FC8D62","#8DA0CB","#E78AC3")))) + 
  xlab("number of videos") + scale_color_manual(values=c("#a590ad","#3e6478")) + 
  ggtitle("Simulations")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title  = element_blank(),
        plot.title=element_text(size=12,hjust=0.5))

#goal_4_plots[[3]]

#ggsave("plots/sim_goal_4_power_curve.png",height=5.12,width=5.79*(0.98/0.62))

#boxplots

#filter down to the top 4 species with the largest differences in their test statistics

strong_effect_species <- test_statistic_dat_kf$Class[abs(test_statistic_dat_kf$diff)>sort(abs(test_statistic_dat_kf$diff))[14]]


goal_4_dat_kf$Class_short <-  goal_4_dat_kf$Class
levels(goal_4_dat_kf$Class_short) =c("A.agra","A.macl","A.perc","C.stur","D.band",
                                     "G.apri","L.calc","L.unic","L.ord","M.nigr","M.sple",
                                     "M.mogu","N.ereb","N1.spp","N2.spp","O.line","S.jard",
                                     "S.kref","S.butl","T.chat")


levels(goal_4_dat_kf$measure) <- c("MeanCount","MaxN")

#get colour values


plot_cols_kf <- brewer.pal(n = 4, name = "Set2")[match(goal_4_dat_kf %>% filter(Class %in% strong_effect_species) %>% droplevels() %>% 
                                          mutate(Schooling_Mobility = Schooling:Mobility) %>% dplyr::select(Schooling_Mobility) %>% 
                                          unlist() %>% factor() %>% unique(),levels(goal_4_dat_kf$Schooling:goal_4_dat_kf$Mobility)) %>% sort()]

plot_cols_kf2 <- brewer.pal(n = 4, name = "Dark2")[match(goal_4_dat_kf %>% filter(Class %in% strong_effect_species) %>% droplevels() %>% 
                                          mutate(Schooling_Mobility = Schooling:Mobility) %>% dplyr::select(Schooling_Mobility) %>% 
                                          unlist() %>% factor() %>% unique(),levels(goal_4_dat_kf$Schooling:goal_4_dat_kf$Mobility)) %>% sort()]




levels(goal_4_dat_kf$river) <- c("Mudgi","Sandy")
                                     
goal_4_plots_2 <- list()

goal_4_plots_2[[2]] <- ggplot(goal_4_dat_kf %>% filter(Class %in% strong_effect_species), aes(y=value_tol,x=river,fill=river:Schooling:Mobility)) + 
  geom_boxplot() + scale_y_log10() + facet_grid(measure~Class_short,scales = "free_y",switch = "y") +theme_bw() + 
  scale_fill_manual(values = c(plot_cols_kf,plot_cols_kf2),guide = "none") + ylab("value") + xlab("river") +
  #scale_fill_manual(values = c("#FDBF6F", "#FF7F00"),name = "River:") + ylab("value") +
  #scale_color_manual(values=plot_cols_kf,guide="none")+
  theme(axis.text.x=element_text(size = 6.5),
    #axis.ticks.x=element_blank(),
    panel.spacing = unit(0.2, "lines"),
    #axis.ticks.length.x = unit(0, "cm"),
    strip.background = element_blank(),
    strip.placement = "outside",
    axis.title.y=element_blank(),
    axis.title.x=element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text.y = element_blank(),
    strip.text.x = element_text(size = 6.5))


#goal_4_plots_2[[2]]

#ggsave("plots/Kakadu_strong_effect_sp.png")

#filter down to the top 4 species with the largest differences in their test statistics
strong_effect_species_tb <- test_statistic_dat_tb$Class[abs(test_statistic_dat_tb$diff)>sort(abs(test_statistic_dat_tb$diff))[4]]


goal_4_dat_tb$Class_short <-  goal_4_dat_tb$Class
levels(goal_4_dat_tb$Class_short) =c("A.spp","C.laticeps","C.auratus",
                                     "M.spp","Moridae","P.bassensis","S.whitleyi","S.punctata")

#bring in schooling and mobility
goal_4_dat_tb$measure <- factor(goal_4_dat_tb$measure)
levels(goal_4_dat_tb$measure) <- c("MeanCount","MaxN")

plot_cols_tb <- brewer.pal(n = 4, name = "Set2")[match(goal_4_dat_tb %>% filter(Class %in% strong_effect_species_tb) %>% droplevels() %>% 
                                                       mutate(Schooling_Mobility = Schooling:Mobility) %>% dplyr::select(Schooling_Mobility) %>% 
                                                       unlist() %>% factor() %>% levels(),levels(goal_4_dat_tb$Schooling:goal_4_dat_tb$Mobility)) %>%
                                                   sort()]

plot_cols_tb2 <- c("#41B08E","#EB7632","#8188BF","#E75AA7")[match(goal_4_dat_tb %>% filter(Class %in% strong_effect_species_tb) %>% droplevels() %>% 
                                                          mutate(Schooling_Mobility = Schooling:Mobility) %>% dplyr::select(Schooling_Mobility) %>% 
                                                          unlist() %>% factor() %>% levels(),levels(goal_4_dat_tb$Schooling:goal_4_dat_tb$Mobility)) %>%
                                                    sort()]


plot_cols_tb3 <- brewer.pal(n = 4, name = "Dark2")[match(goal_4_dat_tb %>% filter(Class %in% strong_effect_species_tb) %>% droplevels() %>% 
                                                       mutate(Schooling_Mobility = Schooling:Mobility) %>% dplyr::select(Schooling_Mobility) %>% 
                                                       unlist() %>% factor() %>% levels(),levels(goal_4_dat_tb$Schooling:goal_4_dat_tb$Mobility)) %>%
                                                     sort()]




levels(goal_4_dat_tb$treatment) <- c("chain","ctrl","EFM")


goal_4_plots_2[[1]] <- ggplot(goal_4_dat_tb %>% filter(Class %in% strong_effect_species_tb), aes(y=value_tol,x=treatment,fill=treatment:Schooling:Mobility)) + 
  geom_boxplot() + scale_y_log10() + facet_grid(measure~Class_short,scales = "free_y",switch = "y") +theme_bw() + 
  #scale_fill_manual(values = c("#B2DF8A", "#33A02C"),name = "Bait:") + ylab("value") +
  scale_fill_manual(values = c(plot_cols_tb, plot_cols_tb2,plot_cols_tb3),guide="none") + ylab("value") +
  #scale_colour_manual(values=plot_cols_tb,guide="none")+xlab("bait") +
  theme(axis.text.x=element_text(size = 6.5),
    #axis.ticks.x=element_blank(),
    panel.spacing = unit(0.2, "lines"),
    #axis.ticks.length.x = unit(0, "cm"),
    strip.background = element_blank(),
    strip.placement = "outside",
    axis.title.y=element_blank(),
    axis.title.x=element_text(size = 9),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text.x = element_text(size = 6.5))
#goal_4_plots_2[[1]]

#ggsave("plots/Tassie_strong_effect_sp.png",height = 5.12,width=3.41)


goal_4_dat_kf %>% filter(Class %in% strong_effect_species) %>% group_by(Schooling,Mobility) %>% summarise(length(unique(Class))) #%>% table(Class,Schooling:Mobility)
goal_4_dat_tb %>% filter(Class %in% strong_effect_species_tb) %>% group_by(Schooling,Mobility) %>% summarise(length(unique(Class))) #%>% table(Class,Schooling:Mobility)
goal_4_dat_tb %>% group_by(Schooling,Mobility) %>% summarise(length(unique(Class))) #%>% table(Class,Schooling:Mobility)
goal_4_dat_kf %>% group_by(Schooling,Mobility) %>% summarise(length(unique(Class))) #%>% table(Class,Schooling:Mobility)


#simulation boxplot

levels(raw_12_long$Mobility) <- c("High M", "Low M")
levels(raw_12_long$Schooling) <- c("High S", "Low S")

goal_4_plots_2[[3]] <- ggplot(raw_12_long,aes(x=means,y=Value,fill=means:Schooling:Mobility)) + geom_boxplot() + 
  facet_grid2(Measure~Schooling:Mobility,scales = "free_y",switch = "y") + 
  #theme_bw()+scale_y_log10()+ scale_fill_manual(values = c("#A6CEE3", "#1F78B4"),name = "Population mean ")+
  theme_bw()+scale_y_log10()+ scale_fill_manual(values = c(brewer.pal(n = 4, name = "Set2"),brewer.pal(n = 4, name = "Dark2")),guide = "none")+
  scale_color_brewer(palette="Dark2",guide="none")+xlab("simulated population mean")+
  theme(axis.text.x=element_text(size = 6.5),
        #axis.ticks.x=element_blank(),
        panel.spacing = unit(0.2, "lines"),
        #axis.ticks.length.x = unit(0, "cm"),
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        strip.text.y = element_blank(),
        strip.text.x = element_text(size = 6.5))

#goal_4_plots_2[[3]]

#brewer.pal(n = 12, name = "Paired")


#join all the plots together

wrap_plots(plotlist = goal_4_plots, ncol = 3,nrow=1,guides ='collect') + plot_layout(axis_titles = "collect") & theme(legend.position = 'bottom')


ggsave("plots/goal_4_combined_plot.png",height=1.1*5.12,width=1.1*5.79*(0.98/0.62))



wrap_plots(plotlist = goal_4_plots_2, ncol = 3,nrow=1,guides ='collect') + plot_layout(axis_titles = "collect") & theme(legend.position = 'bottom')


ggsave("plots/goal_4_combined_plot2.png",height=1.1*5.12,width=1.1*5.79*(0.98/0.62))


