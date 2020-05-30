# Making BIVA plots for BIA data and testing differences with Hotteling test

library(Hotelling)
library(FactoMineR)
library(factoextra)
library(missMDA)
library(ggplot2)
library(dplyr)
library(here)
library(mgsub)
library(reshape2)
library(ggpubr)


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
idata<-dataset %>% select(c("subid","Participant","Group","Alive","AgeMonths_Cal","kwash","oedema_nr",
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



### make long dataset
dput(colnames(idata))
RH_timepoint<-idata %>% select(c("subid","R.H_1", "R.H_2","group_kwash")) %>% melt
RH_timepoint$time_point<-c(rep("T1", nrow(RH_timepoint)/2), rep("T2", nrow(RH_timepoint)/2))
colnames(RH_timepoint)<-c("subid","group_kwash","RH_TimePoint","RH","time_point")

XcH_timepoint<-idata %>% select(c("subid","Xc.H_1", "Xc.H_2")) %>% melt
colnames(XcH_timepoint)<-c("subid","XcH_TimePoint","XcH")
idata_long<-left_join(RH_timepoint, XcH_timepoint)
idata_long$group_kwash_time<-paste0(idata_long$group_kwash, idata_long$time_point )
head(idata_long)

### set controls T2 to NA
idata_long[idata_long$group_kwash_time == "CPT2",]<-NA
idata_long<-idata_long[complete.cases(idata_long$subid),]

#idata_long<-idata_long[complete.cases(idata_long$),]
range(idata_long$RH, na.rm=T)
range(idata_long$XcH, na.rm=T)

p1<-ggplot(aes(x=idata_long$RH, y=idata_long$XcH), data=idata_long)+
 geom_point(aes(col=idata_long$group_kwash_time, fill=idata_long$group_kwash_time), shape=21, size=3)+
  scale_shape_manual(values=c(19,21,3,19,21,3))+
  scale_x_continuous(lim=c(0,2200))+
  scale_y_continuous(lim=c(0,130))+
  scale_color_manual(values=c("black","salmon","salmon","grey60","grey60"))+
  scale_fill_manual(values=alpha(c("#01FDF6","salmon","white","grey60","white"), 0.5))+
  labs(x = "Resistance index, ohm/m", y = "Reactance index, ohm/m")+
  theme_bw()+
  theme(axis.title = element_text(size = 14), axis.text=element_text(color = "black", size=12), legend.position = "right")+
 # stat_ellipse(aes(group = na.omit(idata_long$group_kwash_time)), na.rm = FALSE, show.legend = NA, level = 0.95) +
  stat_conf_ellipse(aes(group = na.omit(idata_long$group_kwash_time)), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, level = 0.95, npoint = 100, bary = TRUE) +
  stat_mean(geom = "point", aes(group = idata_long$group_kwash_time), na.rm = TRUE, show.legend = NA,
            inherit.aes = TRUE)
p1

p2<-ggplot(aes(x=idata_long$RH, y=idata_long$XcH), data=idata_long)+
  geom_point(aes(col=idata_long$group_kwash_time, fill=idata_long$group_kwash_time), shape=21, size=3)+
  scale_shape_manual(values=c(19,21,3,19,21,3))+
  scale_x_continuous(lim=c(0,2200))+
  scale_y_continuous(lim=c(0,130))+
  scale_color_manual(values=c("black","salmon","salmon","grey60","grey60"))+
  scale_fill_manual(values=alpha(c("#01FDF6","salmon","white","grey60","white"), 0.5))+
  labs(x = "Resistance index, ohm/m", y = "Reactance index, ohm/m")+
  theme_bw()+
  theme(axis.title = element_text(size = 14), axis.text=element_text(color = "black", size=12), legend.position = "right")+
  stat_ellipse(aes(group = na.omit(idata_long$group_kwash_time)), na.rm = FALSE, show.legend = NA, level = 0.95) +
  stat_conf_ellipse(aes(group = na.omit(idata_long$group_kwash_time)), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, level = 0.95, npoint = 100, bary = TRUE) +
  stat_mean(geom = "point", aes(group = idata_long$group_kwash_time), na.rm = TRUE, show.legend = NA,
            inherit.aes = TRUE)
p2

# saves as .svg size 1000 800
ggsave(file="Fig1_BIVA_byGroups_95CImean.svg", plot=p1, width=10, height=8)
ggsave(file="Fig1_BIVA_byGroups_95CImean.pdf", plot=p1, width=10, height=8)


# saves as .svg size 1000 800
ggsave(file="Fig1_BIVA_byGroups_95CImean95CI.svg", plot=p2, width=10, height=8)
ggsave(file="Fig1_BIVA_byGroups_95CImean95CI.pdf", plot=p2, width=10, height=8)



############## Hotelling test
### difference between time points in edema case
e_data<-idata_long[idata_long$group_kwash =="kwash",]
head(e_data)
fit = hotelling.test(.~group_kwash_time, e_data[,c("group_kwash_time","RH","XcH")], perm = TRUE)
fit
plot(fit, col = "lightblue")


### difference between time points in marasmus case
m_data<-idata_long[idata_long$group_kwash =="maras",]
head(m_data)
fit = hotelling.test(.~group_kwash_time, m_data[,c("group_kwash_time","RH","XcH")], perm = TRUE)
fit
plot(fit, col = "lightblue")



SAM_data<-idata[!idata$group_kwash =="CP",]
head(SAM_data)
fit = hotelling.test(.~kwash, SAM_data[,c("kwash","R.H_1","Xc.H_1")], perm = TRUE)
fit
plot(fit, col = "lightblue")


### difference between edema and controls at timepoint 2
SAM_data<-idata[!idata$group_kwash =="maras",]
head(SAM_data)
fit = hotelling.test(.~kwash, SAM_data[,c("kwash","R.H_2","Xc.H_2")], perm = TRUE)
fit
plot(fit, col = "lightblue")







############################################
### Death survival 

dput(colnames(idata))
iidata<-idata %>% select(c("subid","R.H_1","Xc.H_1", "Alive","group_kwash", "oedema_nr"))

iidata$group_kwash_alive<-paste0(iidata$Alive,iidata$group_kwash)
iidata$group_kwash_alive<-mgsub(iidata$group_kwash_alive, c("yesCP","yesmaras","nomaras", "yeskwash","nokwash"), c("CP", "yes","no","yes","no"))


range(iidata$R.H_1, na.rm=T)
range(iidata$Xc.H_1, na.rm=T)

### restrict to maras
m_data<-subset(iidata, !iidata$group_kwash=="kwash")

p3<-ggplot(aes(x=m_data$R.H_1, y=m_data$Xc.H_1), data=m_data)+
  geom_point(aes(col=m_data$group_kwash_alive, fill=m_data$group_kwash_alive), shape=21, size=3)+
  scale_shape_manual(values=c(19,21,3,19,21,3))+
  scale_x_continuous(lim=c(0,2200))+
  scale_y_continuous(lim=c(0,130))+
  scale_color_manual(values=c("black","grey60","grey60"))+
  scale_fill_manual(values=alpha(c("#01FDF6","black","white"), 0.5))+
  labs(x = "Resistance index, ohm/m", y = "Reactance index, ohm/m")+
  theme_bw()+
  theme(axis.title = element_text(size = 14), axis.text=element_text(color = "black", size=12), legend.position = "right")+
  #  stat_ellipse(aes(group = na.omit(m_data$group_kwash_alive)), na.rm = FALSE, show.legend = NA, level = 0.95) +
  stat_conf_ellipse(aes(group = na.omit(m_data$group_kwash_alive)), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, level = 0.95, npoint = 100, bary = TRUE) +
  stat_mean(geom = "point", aes(group = m_data$group_kwash_alive), na.rm = TRUE, show.legend = NA,
            inherit.aes = TRUE)
p3



### restrict to edema
e_data<-subset(iidata, !iidata$group_kwash=="maras")


p4<-ggplot(aes(x=e_data$R.H_1, y=e_data$Xc.H_1), data=e_data)+
  geom_point(aes(col=e_data$group_kwash_alive, fill=e_data$group_kwash_alive), shape=21, size=3)+
  scale_shape_manual(values=c(19,21,3,19,21,3))+
  scale_x_continuous(lim=c(0,2200))+
  scale_y_continuous(lim=c(0,130))+
  scale_color_manual(values=c("black","salmon","salmon"))+
  scale_fill_manual(values=alpha(c("#01FDF6","black","white"), 0.5))+
  labs(x = "Resistance index, ohm/m", y = "Reactance index, ohm/m")+
  theme_bw()+
  theme(axis.title = element_text(size = 14), axis.text=element_text(color = "black", size=12), legend.position = "right")+
  #stat_ellipse(aes(group = na.omit(e_data$group_kwash_alive)), na.rm = FALSE, show.legend = NA, level = 0.95) +
  stat_conf_ellipse(aes(group = na.omit(e_data$group_kwash_alive)), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, level = 0.95, npoint = 100, bary = TRUE) +
  stat_mean(geom = "point", aes(group = e_data$group_kwash_alive), na.rm = TRUE, show.legend = NA,
            inherit.aes = TRUE)
p4


e_data_only<-subset(iidata, iidata$group_kwash=="kwash")
e_data_only$oedema_nr<-as.factor(e_data_only$oedema_nr)

p5<-ggplot(aes(x=e_data_only$R.H_1, y=e_data_only$Xc.H_1), data=e_data_only)+
  geom_point(aes(col=e_data_only$oedema_nr, fill=e_data_only$oedema_nr), shape=21, size=3)+
  scale_shape_manual(values=c(19,21,3,19,21,3))+
  scale_x_continuous(lim=c(0,2200))+
  scale_color_manual(values=c("salmon","salmon","black"))+
  scale_fill_manual(values=alpha(c("white","salmon","red"), 0.5))+
  labs(x = "Resistance index, ohm/m", y = "Reactance index, ohm/m")+
  theme_bw()+
  theme(axis.title = element_text(size = 14), axis.text=element_text(color = "black", size=12), legend.position = "right")
  #stat_ellipse(aes(group = na.omit(e_data_only$oedema_nr)), na.rm = FALSE, show.legend = NA, level = 0.95) +
  #stat_conf_ellipse(aes(group = na.omit(e_data_only$oedema_nr)), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, level = 0.95, npoint = 100, bary = TRUE) +
  #stat_mean(geom = "point", aes(group = e_data_only$oedema_nr), na.rm = TRUE, show.legend = NA, inherit.aes = TRUE)
p5


p_all<-ggarrange(p3,"", p4,p5, ncol = 2, nrow =2, common.legend = FALSE, legend = "none") 
p_all


# saves as .svg size 1000 800
ggsave(file="SuplFig4_BIVA_byGroups_Mortality_95CImean.svg", plot=p_all, width=10, height=8)
ggsave(file="SuplFig4_BIVA_byGroups_Mortality_95CImean.pdf", plot=p_all, width=10, height=8)




### difference between edema and controls at timepoint 2
SAM_data<-idata[idata$group_kwash =="maras",]
head(SAM_data)
fit = hotelling.test(.~Alive, SAM_data[,c("Alive","R.H_2","Xc.H_2")], perm = TRUE)
fit
plot(fit, col = "lightblue")


### difference between edema and controls at timepoint 2
SAM_data<-idata[idata$group_kwash =="kwash",]
head(SAM_data)
fit = hotelling.test(.~Alive, SAM_data[,c("Alive","R.H_2","Xc.H_2")], perm = TRUE)
fit
plot(fit, col = "lightblue")
