library(tidyverse)

#read in data
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


ggplot(power_dat_mean,aes(x=data,y=power,colour=measure)) + geom_point() + geom_line() +
  theme_classic() + geom_hline(yintercept=0.8,linetype="dashed", color = "red") +
  scale_color_manual(values=c("#A6761D","#66A61E")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title  = element_blank())



ggsave("plots/sim_goal_4_power_curve_mean.png",height=5.12,width=5.79*(0.98/0.62))



library(RColorBrewer)
library(ggh4x)

power_dat$Class <- factor(power_dat$Class)
levels(power_dat$Class) <- c("High Schooling : High Mobility","Low Schooling : High Mobility",
                             "High Schooling : Low Mobility","Low Schooling : Low Mobility")

power_dat$Class <- fct_relevel(power_dat$Class,c("High Schooling : High Mobility","High Schooling : Low Mobility",
                                                 "Low Schooling : High Mobility","Low Schooling : Low Mobility"))

ggplot(power_dat,aes(x=data,y=power,shape=measure)) + geom_point() + geom_line() +
  theme_classic() + geom_hline(yintercept=0.8,linetype="dashed", color = "red") + 
  facet_wrap2(~Class,strip=strip_themed(background_x=elem_list_rect(fill=c( "#66C2A5", "#FC8D62","#8DA0CB","#E78AC3")))) + 
  xlab("sample size") + scale_color_manual(values=c("#A6761D","#66A61E")) + 
  theme(panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  legend.position = "bottom",
  legend.title  = element_blank())


ggsave("plots/sim_goal_4_power_curve.png",height=5.12,width=5.79*(0.98/0.62))


#comparative boxplots

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


raw_12_long$Measure <- factor(raw_12_long$Measure)

ggplot(raw_12_long,aes(x=means,y=Value,fill=means)) + geom_boxplot() + 
  facet_grid(Measure~Mobility:Schooling,scales = "free_y",switch = "both") + 
  theme_bw()+scale_y_log10()+ scale_fill_manual(values = c("#A6CEE3", "#1F78B4"),name = "Population mean ")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.spacing = unit(0.2, "lines"),
        axis.ticks.length.x = unit(0, "cm"),
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.box.spacing = margin(0))
        #strip.text.y = element_blank())

brewer.pal(n = 12, name = "Paired")
