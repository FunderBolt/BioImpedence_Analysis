######### BIA with Rosalie 
library(ggplot2)
library(dplyr)
library(here)
library(cowplot)
library(mgsub)


### read in dataset
dataset <- read.csv(here("3-Data","2020-05-20_All.BIA.data_v23.csv"), row.names = 1)
dataset$subid<-row.names(dataset)
colnames(dataset)

# set group.number as a factor
dataset$Participant<-as.factor(dataset$Participant)
dataset$Group<-as.factor(dataset$Group)
dataset$Alive<-ordered(dataset$Alive, levels=c("yes","no"))

dataset<-dataset[!rownames(dataset) == "30001702",]


### pull variables of interest
colnames(dataset)
idata<-dataset %>% select(c("subid","Participant","Group","Alive","AgeMonths_Cal","kwash",
                              "haz_adm", "waz_adm", "whz_adm", "muac",
                              "AgeAdm_months_cal_test2","oedemaTest2_Y_N","zwfl_T2","zwei_T2","zlen_T2","muac1_FollowUp",
                              "mean_r_ohm_1", "mean_xc_ohm_1",  "mean_ph_degrees_1",
                              "mean_r_ohm_2", "mean_xc_ohm_2","mean_ph_degrees_2", "R.H_1", "R.H_2" ,"Xc.H_1",
                              "Xc.H_2", "dR.H.2.1.", "dXc.H.2.1."))


##### To color by group - create a new variable, split by edema 
colnames(idata)


# split by SAM type
idata$group_kwash<-paste0(idata$Participant,"_",idata$kwash) 
idata$group_kwash<-mgsub(idata$group_kwash, c("inhospital_yes","inhospital_no","community_particip_arm_1_no"), c("kwash","maras","CP"))
idata$group_kwash<-as.factor(idata$group_kwash )
summary(idata$group_kwash)


###########################################
# Plot all participants
colnames(idata)

#CP_data<-idata[idata$group_kwash == "CP",]
#W_data<-idata[idata$group_kwash == "maras",]
CP_W_data<-idata[!idata$group_kwash == "kwash",]
E_data<-idata[idata$group_kwash == "kwash",]


# look at data range
range(idata$whz_adm, na.rm=TRUE)
range(idata$zwfl_T2, na.rm=TRUE)

range(idata$R.H_1, na.rm=TRUE)
range(idata$R.H_2, na.rm=TRUE)


p1<-ggplot()+
  geom_point(aes(y=idata$R.H_1, x=idata$whz_adm, colour=idata$group_kwash, shape = idata$Alive))+
  scale_colour_manual(values=c("cyan","salmon","grey50"))+
  scale_x_continuous(limits = c(-8, 4))+
  scale_y_continuous(limits = c(0, 2200))+
  scale_shape_manual(values=c(16,21))+
  labs(x = "WHZ, z-score", y = "R/H, ohm/m")+
theme_bw()+
  theme(axis.title = element_text(size = 14), axis.text=element_text(color = "black", size=10),legend.position = "none")+
#stat_smooth(aes(y=CP_data$R.H_1, x=CP_data$whz_adm), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)+
#stat_smooth(aes(y=W_data$R.H_1, x=W_data$whz_adm), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)+
stat_smooth(aes(y=E_data$R.H_1, x=E_data$whz_adm), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)+
stat_smooth(aes(y=CP_W_data$R.H_1, x=CP_W_data$whz_adm), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)
p1



p2<-ggplot()+
  geom_point(aes(y=idata$R.H_2, x=idata$zwfl_T2, colour=idata$group_kwash, shape = idata$Alive))+
  scale_colour_manual(values=c("cyan","salmon","grey50"))+
  scale_x_continuous(limits = c(-8, 4))+
  scale_y_continuous(limits = c(0, 2200))+
  scale_shape_manual(values=c(16,21))+
  labs(x = "WHZ, z-score", y = "R/H, ohm/m")+
  theme_bw()+
  theme(axis.title = element_text(size = 14), axis.text=element_text(color = "black", size=10), legend.position = "none")+
  #stat_smooth(aes(y=CP_data$R.H_2, x=CP_data$zwfl_T2), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)+
  #stat_smooth(aes(y=W_data$R.H_2, x=W_data$zwfl_T2), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)+
  stat_smooth(aes(y=E_data$R.H_2, x=E_data$zwfl_T2), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)+
  stat_smooth(aes(y=CP_W_data$R.H_2, x=CP_W_data$zwfl_T2), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)
p2



range(idata$mean_xc_ohm_1, na.rm=TRUE)
range(idata$mean_xc_ohm_2, na.rm=TRUE)


p3<-ggplot()+
  geom_point(aes(y=idata$Xc.H_1, x=idata$whz_adm, colour=idata$group_kwash, shape = idata$Alive))+
  scale_colour_manual(values=c("cyan","salmon","grey50"))+
  scale_x_continuous(limits = c(-8, 4))+
  scale_y_continuous(limits = c(0, 120))+
  scale_shape_manual(values=c(16,21))+
  labs(x = "WHZ, z-score", y = "Xc/H, ohm/m")+
  theme_bw()+
  theme(axis.title = element_text(size = 14), axis.text=element_text(color = "black", size=10), legend.position = "none")+
  #stat_smooth(aes(y=CP_data$Xc.H_1, x=CP_data$whz_adm), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)+
  #stat_smooth(aes(y=W_data$Xc.H_1, x=W_data$whz_adm), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)+
  stat_smooth(aes(y=E_data$Xc.H_1, x=E_data$whz_adm), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)+
  stat_smooth(aes(y=CP_W_data$Xc.H_1, x=CP_W_data$whz_adm), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)
p3



p4<-ggplot()+
  geom_point(aes(y=idata$Xc.H_2, x=idata$zwfl_T2, colour=idata$group_kwash, shape = idata$Alive))+
  scale_colour_manual(values=c("cyan","salmon","grey50"))+
  scale_x_continuous(limits = c(-8, 4))+
  scale_y_continuous(limits = c(0, 120))+
  scale_shape_manual(values=c(16,21))+
  labs(x = "WHZ, z-score", y = "Xc/H, ohm/m")+
  theme_bw()+
  theme(axis.title = element_text(size = 14), axis.text=element_text(color = "black", size=10), legend.position = "none")+
  #stat_smooth(aes(y=CP_data$Xc.H_2, x=CP_data$zwfl_T2), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)+
  #stat_smooth(aes(y=W_data$Xc.H_2, x=W_data$zwfl_T2), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)+
  stat_smooth(aes(y=E_data$Xc.H_2, x=E_data$zwfl_T2), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)+
  stat_smooth(aes(y=CP_W_data$Xc.H_2, x=CP_W_data$zwfl_T2), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)
p4


range(log(idata$mean_ph_degrees_1), na.rm=TRUE)
range(idata$mean_ph_degrees_2, na.rm=TRUE)


p5<-ggplot()+
  geom_point(aes(y=log(idata$mean_ph_degrees_1), x=idata$whz_adm, colour=idata$group_kwash, shape = idata$Alive))+
  scale_colour_manual(values=c("cyan","salmon","grey50"))+
  scale_x_continuous(limits = c(-8, 4))+
  scale_y_continuous(limits = c(0, 2.5))+
  scale_shape_manual(values=c(16,21))+
  labs(x = "WHZ, z-score", y = "log(PA), degree")+
  theme_bw()+
  theme(axis.title = element_text(size = 14), axis.text=element_text(color = "black", size=10), legend.position = "none")+
  #stat_smooth(aes(y=CP_data$mean_ph_degrees_1, x=CP_data$whz_adm), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)+
  #stat_smooth(aes(y=W_data$mean_ph_degrees_1, x=W_data$whz_adm), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)+
  stat_smooth(aes(y=log(E_data$mean_ph_degrees_1), x=E_data$whz_adm), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)+
  stat_smooth(aes(y=log(CP_W_data$mean_ph_degrees_1), x=CP_W_data$whz_adm), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)
p5

p6<-ggplot()+
  geom_point(aes(y=log(idata$mean_ph_degrees_2), x=idata$zwfl_T2, colour=idata$group_kwash, shape = idata$Alive))+
  scale_colour_manual(values=c("cyan","salmon","grey50"))+
  scale_x_continuous(limits = c(-8, 4))+
  scale_y_continuous(limits = c(0, 2.5))+
  scale_shape_manual(values=c(16,21))+
  labs(x = "WHZ, z-score", y = "log(PA), degree")+
  theme_bw()+
  theme(axis.title = element_text(size = 14), axis.text=element_text(color = "black", size=10), legend.position = "none")+
  #stat_smooth(aes(y=CP_data$mean_ph_degrees_2, x=CP_data$zwfl_T2), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)+
  #stat_smooth(aes(y=W_data$mean_ph_degrees_2, x=W_data$zwfl_T2), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)+
  stat_smooth(aes(y=log(E_data$mean_ph_degrees_2), x=E_data$zwfl_T2), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)+
  stat_smooth(aes(y=log(CP_W_data$mean_ph_degrees_2), x=CP_W_data$zwfl_T2), method="lm", col="grey30",lwd=1, se=TRUE,alpha=0.2)
p6




p_all<-plot_grid(p1,p2,p3,p4,p5,p6, cols=2)
# saves as .svg size 1000 800
ggsave(file="SuplFig3_Correlation_Athro_byGroups.svg", plot=p_all, width=8, height=10)
