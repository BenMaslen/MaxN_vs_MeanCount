#Load libraries
library(tidyverse)
library(patchwork)
library(RColorBrewer)

#read in data (generated from goal_3_data_prep)
goal_3_dat_tb <- read_csv("results/60_seconds/goal_3_dat_tb.csv")
goal_3_dat_kf <- read_csv("results/60_seconds/goal_3_dat_kf.csv")

#read in metadata
vid_metadata_tb         <- read_csv("data/BRUV_RUV_metadata.csv")
vid_metadata_kf         <- read_csv("data/KF_video_meta_data.csv")
species_metadata_tb     <- read_csv("data/TB_species_metadata.csv")
species_metadata_kf     <- read_csv("data/KF_species_metadata.csv")

#clean metadata
colnames(vid_metadata_tb)[1] <- "Video_name"
colnames(vid_metadata_kf)[1] <- "Video_name"


#join with metadata
goal_3_dat_tb <- left_join(goal_3_dat_tb,vid_metadata_tb,by="Video_name")
goal_3_dat_kf <- left_join(goal_3_dat_kf,vid_metadata_kf,by="Video_name")

goal_3_dat_tb <- left_join(goal_3_dat_tb,species_metadata_tb,by="Class")
goal_3_dat_kf <- left_join(goal_3_dat_kf,species_metadata_kf,by="Class")


#Start with EDA
#goal_3_dat_tb <- goal_3_dat_tb %>% filter(EDA=="yes")
#goal_3_dat_kf <- goal_3_dat_kf %>% filter(EDA=="yes")
goal_3_dat_tb <- goal_3_dat_tb %>% filter(EDA=="no",QC=="PASS")
goal_3_dat_kf <- goal_3_dat_kf %>% filter(EDA=="no")

#filter out non-existent species in Kakadu
goal_3_dat_kf <- goal_3_dat_kf %>% filter(Class!="Melanotaenia nigrans",
                                          Class!="Mogurnda mogurnda")

#convert into factors
goal_3_dat_tb <- goal_3_dat_tb %>% mutate(Class = as.factor(Class), 
                                          Video_name = as.factor(Video_name),
                                          location = as.factor(location),
                                          block = as.factor(block),
                                          treatment = as.factor(treatment),
                                          bait = as.factor(bait),
                                          Mobility = as.factor(Mobility),
                                          Schooling = as.factor(Schooling),
                                          measure = as.factor(measure))
goal_3_dat_kf <- goal_3_dat_kf %>% mutate(Class = as.factor(Class), 
                                          Video_name = as.factor(Video_name),
                                          river = as.factor(river),
                                          transect = as.factor(transect),
                                          location = as.factor(location),
                                          year = as.factor(year),
                                          Mobility = as.factor(Mobility),
                                          Schooling = as.factor(Schooling),
                                          measure = as.factor(measure))

levels(goal_3_dat_tb$measure) <- c("MeanCount","MaxN")
levels(goal_3_dat_kf$measure) <- c("MeanCount","MaxN")


#relative bias
ggplot(goal_3_dat_kf, aes(y=bias_rel,x=measure,fill=Class)) + geom_boxplot() + theme(legend.position="none") + 
  theme_bw() + ylab("relative bias") + guides(fill = "none")#in appendices



#Tassy_BRUV
ggplot(goal_3_dat_tb, aes(y=bias_rel,x=measure,fill=Class)) + geom_boxplot() + 
  theme_bw() + ylab("relative bias")#in appendices



### BIAS - Mean plot combined

#set ylims for AvgN and MaxN 

neq0 <- function(x){
  return(x!=0)
}

goal_3_dat_kf_scale_eval <- goal_3_dat_kf %>% group_by(measure) %>% summarise(max_abs_bias = max(abs(bias)), max_mean = max(mean),
                                                                              ratio_abs = max(abs(bias))/max(mean),max_var = max((CoV*mean)^2,na.rm=T),
                                                                              ratio_var = max((CoV*mean)^2,na.rm=T)/max(mean),
                                                                              var_min = min(Filter(neq0,(CoV*mean)^2),na.rm=T))

goal_3_dat_tb_scale_eval <- goal_3_dat_tb %>% group_by(measure) %>% summarise(max_abs_bias = max(abs(bias)), max_mean = max(mean),
                                                                              ratio_abs = max(abs(bias))/max(mean),max_var = max((CoV*mean)^2,na.rm=T),
                                                                              ratio_var = max((CoV*mean)^2,na.rm=T)/max(mean),
                                                                              var_min = min(Filter(neq0,(CoV*mean)^2),na.rm=T))


Avgn_ylim_bias_kf = goal_3_dat_kf_scale_eval$ratio_abs[goal_3_dat_kf_scale_eval$measure=="MaxN"]*goal_3_dat_kf_scale_eval$max_mean[goal_3_dat_kf_scale_eval$measure=="MeanCount"]
MaxN_ylim_bias_kf = goal_3_dat_kf_scale_eval$max_abs_bias[goal_3_dat_kf_scale_eval$measure=="MaxN"]

Avgn_ylim_bias_tb = goal_3_dat_tb_scale_eval$ratio_abs[goal_3_dat_tb_scale_eval$measure=="MaxN"]*goal_3_dat_tb_scale_eval$max_mean[goal_3_dat_tb_scale_eval$measure=="MeanCount"]
MaxN_ylim_bias_tb = goal_3_dat_tb_scale_eval$max_abs_bias[goal_3_dat_tb_scale_eval$measure=="MaxN"]

#apply tolerance

#apply tolerance
AvgN_tol_kf <- min(goal_3_dat_kf$mean[goal_3_dat_kf$mean!=0 & goal_3_dat_kf$measure=="MeanCount"])/2
AvgN_tol_tb <- min(goal_3_dat_tb$mean[goal_3_dat_tb$mean!=0 & goal_3_dat_tb$measure=="MeanCount"])/2
AvgN_tol <- min(AvgN_tol_kf,AvgN_tol_tb)
MaxN_tol <- 0.5


goal_3_dat_kf$mean_tol <- goal_3_dat_kf$mean+AvgN_tol*(goal_3_dat_kf$measure=="MeanCount") + MaxN_tol*(goal_3_dat_kf$measure=="MaxN")
goal_3_dat_tb$mean_tol <- goal_3_dat_tb$mean+AvgN_tol*(goal_3_dat_tb$measure=="MeanCount") + MaxN_tol*(goal_3_dat_tb$measure=="MaxN")


#plotting colours
cols <- c('high:high' = "#66C2A5",'high:low' = "#FC8D62",'low:high' = "#8DA0CB", 'low:low' = "#E78AC3")

#Kakadu plots

# Split data by 'measure'
split_data_kf <- split(goal_3_dat_kf, goal_3_dat_kf$measure)


# Create a list to store plots
plot_list_kf <- list()


# Loop through the list of data frames
for(i in 1:length(split_data_kf)) {
  p <- ggplot(split_data_kf[[i]], aes(y=mean,x=mean+bias,colour=Schooling:Mobility)) + 
    geom_abline(size=0.2) +
    geom_point(alpha=0.6,show.legend=T) + 
    geom_smooth(se=F,size=0.5,method = "gam",formula = y~s(x,k=3),show.legend=T) +
    theme_bw() + 
    scale_color_manual(values = cols,limits=c('high:high','high:low','low:high','low:low'),
                       labels = c('high:high','high:low','low:high','low:low')) #+
    #ggtitle(paste0(names(split_data_kf)[i], " - Kakadu"))  # Add title to each plot
  
  # Remove legend for all but the first plot
  if(i == 1) {
    p <- p  + ylab(expression(atop("MeanCount","bias"))) + 
    ggtitle("Kakadu") + xlab("sample estimate (log)") + theme(plot.title = element_text(size = 12,hjust=0.5),
                                                   legend.position="none",axis.title.x = element_blank(),
                                                   axis.title.y = element_blank(),
                                                   panel.grid.major = element_blank(),
                                                   panel.grid.minor = element_blank()) +
      scale_x_log10(limits=c(AvgN_tol,8)) + scale_y_log10(limits=c(AvgN_tol,8))
    #ylim(-Avgn_ylim_bias_kf, Avgn_ylim_bias_kf)+ 
  }else{
    p<- p   + ylab(expression(atop("MaxN","bias"))) + 
       xlab("sample estimate (log)") + theme(plot.title = element_text(size = 12,hjust=0.5),
                                  legend.position="none",axis.title.y = element_blank(),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank()) + 
      scale_x_log10(limits=c(MaxN_tol,max(split_data_kf[[2]]$mean+split_data_kf[[2]]$bias)+1)) + 
      scale_y_log10(limits=c(MaxN_tol,max(split_data_kf[[2]]$mean+split_data_kf[[2]]$bias)+1))
    #+ ylim(-MaxN_ylim_bias_kf, MaxN_ylim_bias_kf)
  }

  
  # Add the plot to the list
  plot_list_kf[[i]] <- p
}



# Split data by 'measure'
split_data_tb <- split(goal_3_dat_tb, goal_3_dat_tb$measure)

# Create a list to store plots
plot_list_tb <- list()


# Loop through the list of data frames
for(i in 1:length(split_data_tb)) {
  p <- ggplot(split_data_tb[[i]], aes(y=mean,x=mean+bias,colour=Schooling:Mobility)) + 
    geom_abline(size=0.2)+ geom_point(alpha=0.6,show.legend = T) + 
    geom_smooth(se=F,size=0.5,method = "gam",formula = y~s(x,k=3),show.legend = T) +
    theme_bw() + 
    scale_color_manual(values = cols,limits=c('high:high','high:low','low:high','low:low'),
                       labels = c('high:high','high:low','low:high','low:low')) #+
    #ggtitle(paste0(names(split_data_tb)[i], " - Tassie"))  # Add title to each plot + 
    
  
  # Remove legend for all but the first plot
  if(i == 1) {
    #p <- p   +ylab(expression(atop("MeanCount (log)"))) +
    p <- p   +ylab("MeanCount (log)") +
      ggtitle("Tassie") + xlab("mean (log)") + theme(plot.title = element_text(size = 12,hjust=0.5),
                                                  legend.position="none",axis.title.x = element_blank(),
                                                  panel.grid.major = element_blank(),
                                                  panel.grid.minor = element_blank()) +
      scale_x_log10(limits=c(AvgN_tol,8)) + scale_y_log10(limits=c(AvgN_tol,8))
    #+ ylim(limits = c(-Avgn_ylim_bias_tb, Avgn_ylim_bias_tb))
  }else{
    #p<- p  + ylab(expression(atop("MaxN (log)"))) +
    p<- p  + ylab("MaxN (log)") +
      xlab("sample estimate (log)") + theme(plot.title = element_text(size = 12,hjust=0.5),
                                 legend.position="none",
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank()) + 
      scale_x_log10(limits=c(MaxN_tol,max(split_data_kf[[2]]$mean+split_data_kf[[2]]$bias)+1)) + 
      scale_y_log10(limits=c(MaxN_tol,max(split_data_kf[[2]]$mean+split_data_kf[[2]]$bias)+1))
    # + ylim(limits = c(-MaxN_ylim_bias_tb, MaxN_ylim_bias_tb))
  }
  
  # Add the plot to the list
  plot_list_tb[[i]] <- p
}

# Combine tb plots
plot_list_all_bias <- list()
plot_list_all_bias[[2]] = plot_list_kf[[1]]
plot_list_all_bias[[4]] = plot_list_kf[[2]]
plot_list_all_bias[[1]] = plot_list_tb[[1]]
plot_list_all_bias[[3]] = plot_list_tb[[2]]


# Combine all 4 plots
combined_plot_all_bias <- wrap_plots(plotlist = plot_list_all_bias, ncol = 2,nrow=2,guides ='collect') + plot_layout(axis_titles = "collect")
#combined_plot_all_bias


#ggsave("plots/BIAS_boot_plot.png",plot=combined_plot_all_bias,scale=1.2)

### Variance - Mean plot combined


Avgn_ylim_var_kf = goal_3_dat_kf_scale_eval$ratio_var[goal_3_dat_kf_scale_eval$measure=="MaxN"]*goal_3_dat_kf_scale_eval$max_mean[goal_3_dat_kf_scale_eval$measure=="MeanCount"]
MaxN_ylim_var_kf = goal_3_dat_kf_scale_eval$max_var[goal_3_dat_kf_scale_eval$measure=="MaxN"]

Avgn_ylim_var_tb = goal_3_dat_tb_scale_eval$ratio_var[goal_3_dat_tb_scale_eval$measure=="MaxN"]*goal_3_dat_tb_scale_eval$max_mean[goal_3_dat_tb_scale_eval$measure=="MeanCount"]
MaxN_ylim_var_tb = goal_3_dat_tb_scale_eval$max_var[goal_3_dat_tb_scale_eval$measure=="MaxN"]

#plotting colours
cols <- c('high:high' = "#66C2A5",'high:low' = "#FC8D62",'low:high' = "#8DA0CB", 'low:low' = "#E78AC3")


#Kakadu plots

# Split data by 'measure'
split_data_kf <- split(goal_3_dat_kf, goal_3_dat_kf$measure)

# Create a list to store plots
plot_list_kf <- list()


# Loop through the list of data frames
for(i in 1:length(split_data_kf)) {
  #p <- ggplot(split_data_kf[[i]], aes(y=CoV,x=mean,colour=Schooling:Mobility)) + 
  p <- ggplot(split_data_kf[[i]], aes(y=(CoV*mean)^2,x=mean,colour=Schooling:Mobility)) + 
    geom_point(alpha=0.4) + 
    scale_x_log10() +
    geom_smooth(se=F,size=0.5,method = "gam",formula = y~s(x,k=3)) +
    theme_bw()  +
    scale_color_manual(values = cols,limits=c('high:high','high:low','low:high','low:low'),
                       labels = c('high:high','high:low','low:high','low:low')) #+
    #ylab("variance")+
    #ggtitle(paste0(names(split_data_kf)[i], " - Kakadu"))  # Add title to each plot
  
  # Remove legend for all but the first plot
  if(i == 1) {
    p <- p  + scale_y_log10(limits = c(goal_3_dat_kf_scale_eval$var_min[1], Avgn_ylim_var_kf)) +
      xlab("mean (log)") +ylab(expression(atop("MeanCount","variance (log)"))) + theme(plot.title = element_text(size = 12,hjust=0.5),
                                                                                       legend.position="none",axis.title.y = element_blank(),
                                                                                       axis.title.x = element_blank(),
                                                                                       panel.grid.major = element_blank(),
                                                                                       panel.grid.minor = element_blank()) + ggtitle("Kakadu")#scale_y_log10(limits = c(0.1,1)) #
  }else{
    p<- p  + scale_y_log10(limits = c(goal_3_dat_kf_scale_eval$var_min[2], MaxN_ylim_var_kf)) +
      xlab("mean (log)") + ylab(expression(atop("MaxN","variance (log)"))) + theme(plot.title = element_text(size = 10),
                                                                                   legend.position="none",axis.title.y = element_blank(),
                                                                                   panel.grid.major = element_blank(),
                                                                                   panel.grid.minor = element_blank())#scale_y_log10(limits = c(0.1,1))#
  }
  
  # Add the plot to the list
  plot_list_kf[[i]] <- p
}


# Split data by 'measure'
split_data_tb <- split(goal_3_dat_tb, goal_3_dat_tb$measure)

# Create a list to store plots
plot_list_tb <- list()


# Loop through the list of data frames
for(i in 1:length(split_data_tb)) {
  p <- ggplot(split_data_tb[[i]], aes(y=(CoV*mean)^2,x=mean,colour=Schooling:Mobility)) + 
  #p <- ggplot(split_data_tb[[i]], aes(y=(CoV*mean)^2,x=mean,colour=Schooling:Mobility)) + 
    geom_point(alpha=0.4,show.legend = T) + 
    scale_x_log10() +
    geom_smooth(se=F,size=0.5,method = "gam",formula = y~s(x,k=3),show.legend = T) +
    theme_bw()  +
    scale_color_manual(values = cols,limits=c('high:high','high:low','low:high','low:low'),
                       labels = c('high:high','high:low','low:high','low:low'))# +
    #ylab("variance")#+
    #ggtitle(paste0(names(split_data_tb)[i], " - Tassie"))  # Add title to each plot
  
  # Remove legend for all but the first plot
  if(i == 1) {
    p <- p  + scale_y_log10(limits = c(goal_3_dat_tb_scale_eval$var_min[1], Avgn_ylim_var_tb)) +
      xlab("mean (log)") + ylab(expression(atop("MeanCount","variance (log)"))) + theme(plot.title = element_text(size = 12,hjust=0.5),
                                                                                        legend.position="none",axis.title.x = element_blank(),
                                                                                        panel.grid.major = element_blank(),
                                                                                        panel.grid.minor = element_blank()) + ggtitle("Tassie")#scale_y_log10(limits = c(0.1,1)) #
  }else{
    p<- p   + scale_y_log10(limits = c(goal_3_dat_tb_scale_eval$var_min[2], MaxN_ylim_var_tb))  +
      xlab("mean (log)") + ylab(expression(atop("MaxN","variance (log)"))) + theme(plot.title = element_text(size = 10),
                                                                                   legend.position="none",
                                                                                   panel.grid.major = element_blank(),
                                                                                   panel.grid.minor = element_blank())#scale_y_log10(limits = c(0.1,1))#
  }
  
  # Add the plot to the list
  plot_list_tb[[i]] <- p
}

# Combine tb plots
plot_list_all_var <- list()
plot_list_all_var[[2]] = plot_list_kf[[1]]
plot_list_all_var[[4]] = plot_list_kf[[2]]
plot_list_all_var[[1]] = plot_list_tb[[1]]
plot_list_all_var[[3]] = plot_list_tb[[2]]


# Combine all 4 plots
combined_plot_all_var <- wrap_plots(plotlist = plot_list_all_var, ncol = 2,nrow=2,guides ='collect') + plot_layout(axis_titles = "collect")
#combined_plot_all_var

#ggsave("plots/var_boot_plot.png",plot=combined_plot_all_var,scale=1.2)


# Combine all 8 plots
plot_list_all <- c(plot_list_all_bias,plot_list_all_var)


combined_plot_all_var <- wrap_plots(plotlist = plot_list_all, ncol = 2,nrow=4,guides ='collect') + plot_layout(axis_titles = "collect")
#combined_plot_all_var

#ggsave("plots/bias_var_boot_plot.png",plot=combined_plot_all_var,scale=1.2)




mean_var_dat <- read.csv("results/60_seconds/simulation_goal3_dat.csv")
mean_var_dat$Mobility <- factor(mean_var_dat$Mobility)
mean_var_dat$Schooling <- factor(mean_var_dat$Schooling)
mean_var_dat <- mean_var_dat %>% select(-X)

mean_var_dat_summary <- mean_var_dat %>% group_by(Mobility,Schooling,pop_means) %>% summarise(MaxN_mean = mean(MaxN),MaxN_var = sd(MaxN)^2,
                                                                                              AvgN_mean = mean(AvgN),AvgN_var = sd(AvgN)^2,
                                                                                              PopN_mean = mean(PopN),PopN_var = sd(PopN)^2)
pop_mean_overall = mean(mean_var_dat$PopN)
AvgN_mean_overall = mean(mean_var_dat$AvgN)
MaxN_mean_overall = mean(mean_var_dat$MaxN)


plot_list_sim <- list()


plot_list_sim[[2]] <- ggplot(mean_var_dat_summary, aes(y=AvgN_var,x=AvgN_mean,colour=Schooling:Mobility)) + 
  geom_point() + scale_x_log10() +scale_y_log10(limits=c(0.01,(max(mean_var_dat_summary$MaxN_var)/max(mean_var_dat_summary$MaxN_mean))*max(mean_var_dat_summary$AvgN_mean)))  +theme_bw() + 
  theme(legend.position = "none",axis.title.y = element_blank(),axis.title.x = element_blank(),plot.title = element_text(size = 12,hjust=0.5),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_color_manual(values = brewer.pal(n = 4, name = "Set2")[c(1,2,3,4)]) + 
  ylab(expression(atop("MeanCount","variance (log)")))+xlab("mean (log)") +
  geom_smooth(se=F,size=0.5,method = "gam",formula = y~s(x,k=3)) + ggtitle("Simulations")
#plot_list_sim[[2]]

#ggsave("plots/sim_goal3_meancount.png",height=5.12,width=5.79*(0.98/0.62))

plot_list_sim[[4]] <- ggplot(mean_var_dat_summary, aes(y=MaxN_var,x=MaxN_mean,colour=Schooling:Mobility)) + 
  geom_point() + scale_x_log10()+scale_y_log10()   +theme_bw() +  theme(legend.position = "none",axis.title.y = element_blank(),panel.grid.major = element_blank(),
                                                                        panel.grid.minor = element_blank())+
  scale_color_manual(values = brewer.pal(n = 4, name = "Set2")[c(1,2,3,4)]) + 
  ylab(expression(atop("MaxN","variance (log)")))+xlab("mean (log)")+ 
  geom_smooth(se=F,size=0.5,method = "gam",formula = y~s(x,k=3)) 
#plot_list_sim[[4]]

#ggsave("plots/sim_goal3_maxn.png",height=5.12,width=5.79*(0.98/0.62))
#min(mean_var_dat_summary$AvgN_mean)/2

fit_meancount <- lm(AvgN~0 + PopN,data=mean_var_dat)
summary(fit_meancount)
confint(fit_meancount)
pred_data_meancount <- data.frame(x1=seq(0,200,0.001),y1=predict(fit_meancount,newdata=data.frame(PopN=seq(0,200,0.001))))


plot_list_sim[[1]] <- ggplot(mean_var_dat_summary, aes(x=pop_means,y=AvgN_mean,colour=Schooling:Mobility))   +theme_bw() + 
  scale_color_manual(values = brewer.pal(n = 4, name = "Set2")[c(1,2,3,4)]) + ylab("mean")+xlab("population size (log)")+ 
  geom_line(data=pred_data_meancount,aes(x=x1,y=y1),colour="black")+
  geom_point(data=mean_var_dat,aes(x=PopN,y=AvgN,colour=Schooling:Mobility),size=0.1,alpha=0.1) +  
  geom_smooth(se=F,size=0.5,method = "gam",formula = y~s(x,k=3)) +
  ggtitle("Simulations")+ 
  geom_point() + theme(legend.position = "none",plot.title = element_text(size = 12,hjust=0.5),axis.title.x = element_blank(),
  axis.title.y = element_blank(),panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()) +scale_y_log10(limits=c(5*coef(fit_meancount),200*coef(fit_meancount)),expand=c(0,0)) + 
  scale_x_log10(limits=c(5,200),expand=c(0,0))# xlim(0,150) +ylim(0,3)
plot_list_sim[[1]]


#ggsave("plots/sim_goal3_meancount_bias.png",height=5.12,width=5.79*(0.98/0.62))

fit_maxn <- lm(MaxN~0 + PopN,data=mean_var_dat)
pred_data_maxn <- data.frame(x1=seq(0,200,0.001),y1=predict(fit_maxn,newdata=data.frame(PopN=seq(0,200,0.001))))



plot_list_sim[[3]] <- ggplot(mean_var_dat_summary, aes(y=MaxN_mean,x=pop_means,colour=Schooling:Mobility))  +theme_bw() + 
  scale_color_manual(values = brewer.pal(n = 4, name = "Set2")[c(1,2,3,4)]) + ylab("mean")+xlab("population size (log)") +
  geom_line(data=pred_data_maxn,aes(x=x1,y=y1),colour="black")+
  geom_point(data=mean_var_dat,aes(x=PopN,y=MaxN,colour=Schooling:Mobility),size=0.1,alpha=0.1) + 
  geom_smooth(se=F,size=0.5,method = "gam",formula = y~s(x,k=3))  + 
  scale_x_log10(limits=c(5,200),expand=c(0,0)) +
  scale_y_log10(limits=c(5*coef(fit_maxn),200*coef(fit_maxn)),expand=c(0,0)) +
  geom_point() + theme(legend.position = "none",axis.title.y = element_blank(),panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank())
plot_list_sim[[3]]

#ggsave("plots/sim_goal3_maxn_bias.png",height=5.12,width=5.79*(0.98/0.62))


combined_plot_all <- wrap_plots(plotlist = plot_list_sim, ncol = 2,nrow=2,guides ='collect') #+ plot_layout(axis_titles = "collect")
#combined_plot_all

#ggsave("plots/sim_goal3.png",height=5.12,width=5.79*(0.98/0.62))


#appendix table - seperate estimates of MaxN to population
library(emmeans)
fit_meancount_allsp <- lm(AvgN~0+PopN*Schooling*Mobility,data=mean_var_dat)
emtrends(fit_meancount_allsp,~PopN|Schooling:Mobility,var="PopN")

fit_maxn_allsp <- lm(MaxN~0+PopN*Schooling:Mobility,data=mean_var_dat)
fit_maxn_allsp <- lm(MaxN_mean ~0+pop_means:Schooling:Mobility,data=mean_var_dat_summary)

summary(fit_maxn_allsp)

#emtrends(fit_maxn_allsp,~PopN|Schooling:Mobility,var="PopN")
emtrends(fit_maxn_allsp,~pop_means|Schooling:Mobility,var="pop_means")



summary(fit_meancount_allsp)
pred_data_meancount <- data.frame(x1=seq(0,200,0.001),y1=predict(fit_meancount,newdata=data.frame(PopN=seq(0,200,0.001))))



0.0508*10
0.0508*100


#combine everything

# plot_list_all_sims <- list()
# 
# plot_list_all_sims[[1]] <- plot_list_all_bias[[1]]
# plot_list_all_sims[[2]] <- plot_list_all_bias[[2]]
# plot_list_all_sims[[4]] <- plot_list_all_bias[[3]]
# plot_list_all_sims[[5]] <- plot_list_all_bias[[4]]
# plot_list_all_sims[[7]] <- plot_list_all_var[[1]]
# plot_list_all_sims[[8]] <- plot_list_all_var[[2]]
# plot_list_all_sims[[10]] <- plot_list_all_var[[3]]
# plot_list_all_sims[[11]] <- plot_list_all_var[[4]]
# plot_list_all_sims[[3]] <- plot_list_sim[[1]]
# plot_list_all_sims[[9]] <- plot_list_sim[[2]]
# plot_list_all_sims[[6]] <- plot_list_sim[[3]]
# plot_list_all_sims[[12]] <- plot_list_sim[[4]]
# 
# 
# 
# combined_plot_all_sims <- wrap_plots(plotlist = plot_list_all_sims, ncol = 3,nrow=4,guides ='collect') #+ plot_layout(axis_titles = "collect")
#combined_plot_all_sims

#ggsave("plots/goal3_plot.png",height=3*1.1*5.12,width=1.1*5.79*(0.98/0.62))


#let's turn this into two plots
#bias plot

plot_list_bias_sims <- list()

plot_list_bias_sims[[1]] <- plot_list_all_bias[[1]]
plot_list_bias_sims[[2]] <- plot_list_all_bias[[2]]
plot_list_bias_sims[[4]] <- plot_list_all_bias[[3]]
plot_list_bias_sims[[5]] <- plot_list_all_bias[[4]]
plot_list_bias_sims[[3]] <- plot_list_sim[[1]]
plot_list_bias_sims[[6]] <- plot_list_sim[[3]]


combined_plot_bias_sims <- wrap_plots(plotlist = plot_list_bias_sims, ncol = 3,nrow=2,guides ='collect') #+ plot_layout(axis_titles = "collect")
combined_plot_bias_sims

ggsave("plots/60_seconds/goal3_plot_bias.png",height=1.1*5.12,width=1.1*5.79*(0.98/0.62))


#variance plot

plot_list_var_sims <- list()

plot_list_var_sims[[1]] <- plot_list_all_var[[1]]
plot_list_var_sims[[2]] <- plot_list_all_var[[2]]
plot_list_var_sims[[4]] <- plot_list_all_var[[3]]
plot_list_var_sims[[5]] <- plot_list_all_var[[4]]
plot_list_var_sims[[3]] <- plot_list_sim[[2]]
plot_list_var_sims[[6]] <- plot_list_sim[[4]]


combined_plot_var_sims <- wrap_plots(plotlist = plot_list_var_sims, ncol = 3,nrow=2,guides ='collect') #+ plot_layout(axis_titles = "collect")
combined_plot_var_sims

ggsave("plots/60_seconds/goal3_plot_var.png",height=1.1*5.12,width=1.1*5.79*(0.98/0.62))





mean_var_dat_summary_10_100 <- mean_var_dat_summary %>% filter(pop_means %in% c(10,100))

mean_var_dat_summary_10_100[2,c(4,6)]/mean_var_dat_summary_10_100[1,c(4,6)]
mean_var_dat_summary_10_100[4,c(4,6)]/mean_var_dat_summary_10_100[3,c(4,6)]
mean_var_dat_summary_10_100[6,c(4,6)]/mean_var_dat_summary_10_100[5,c(4,6)]
mean_var_dat_summary_10_100[8,c(4,6)]/mean_var_dat_summary_10_100[7,c(4,6)]


#for the appendix
#relative bias
goal_3_dat_kf$CoV[is.na(goal_3_dat_kf$CoV)] <- 0
ggplot(goal_3_dat_kf, aes(y=CoV,x=measure,fill=Class)) + geom_boxplot() + theme(legend.position="none") + 
  theme_bw() + ylab("CoV") + guides(fill = "none")#in appendices

goal_3_dat_kf %>% group_by(measure) %>% summarise(mean_CoV = mean(CoV))

#Tassy_BRUV
goal_3_dat_tb$CoV[is.na(goal_3_dat_tb$CoV)] <- 0
ggplot(goal_3_dat_tb, aes(y=bias_rel,x=measure,fill=Class)) + geom_boxplot() + 
  theme_bw() + ylab("CoV")#in appendices

goal_3_dat_tb %>% group_by(measure) %>% summarise(mean_CoV = mean(CoV))





