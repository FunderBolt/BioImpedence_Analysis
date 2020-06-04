#### PLS-DA  | Mortality
####################################################
#remotes::install_github("mixOmicsTeam/mixOmics")
library(mixOmics)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(dplyr)
library(here)
library(mgsub)
library(reshape2)
library(ggpubr)
library(ggforce)
library(ggROC)
library(pROC)
library(directlabels)



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



## running PCA
#pull data of interest
colnames(idata)
### all variables
fit1<-PCA(idata[,c("group_kwash","haz_adm","waz_adm","whz_adm","muac","mean_ph_degrees_1","R.H_1","Xc.H_1")],quali.sup =c(1))
plot(fit1,habillage=1, col.hab=c("red","black","green"), label="none")
## just anthropometry
fit1<-PCA(idata[,c("group_kwash","haz_adm","waz_adm","whz_adm","muac")],quali.sup =c(1))
plot(fit1,habillage=1, col.hab=c("red","black","green"), label="none")
## just BIA
fit1<-PCA(idata[,c("group_kwash","mean_ph_degrees_1","R.H_1","Xc.H_1")],quali.sup =c(1))
plot(fit1,habillage=1, col.hab=c("red","black","green"), label="none")


## for mortality
colnames(idata)
fit1<-PCA(idata[,c("Alive","haz_adm","waz_adm","whz_adm","muac","R.H_1","Xc.H_1","mean_ph_degrees_1")],quali.sup =c(1))
plot(fit1,habillage=1, col.hab=c("red","black"), label="none")



#####################################################################
### data with only antrho

## restrict to only SAM
idata<-idata[!idata$Group==3,]

#only anthro
X<-as.matrix(idata[,c("haz_adm","waz_adm","whz_adm","muac")])

### log all variables
X<-log(X+10)
outcome<-as.factor(idata$Alive)
### plsda between groups
out.plsDA<-plsda(X,outcome, ncomp=2, scale=TRUE, mode="regression")
out.plsDA$explained_variance

#perf.out<-mixOmics::perf(out.plsDA, validation = c("loo"), progressBar = TRUE, auc=TRUE)
#summary(perf.out)

# get.confusion_matrix(truth = idata$Group_Kwa, predicted=perf.out$predict)
# perf.out$error.rate
# perf.out$error.rate.class
# perf.out$predict
# perf.out$class
# plot(perf.out, overlay = 'measure', sd=TRUE)
# plot(perf.out$error.rate.class, overlay = 'measure', sd=TRUE)



vars.in<-plotVar(out.plsDA, plot=TRUE,cex=3,cutoff=0.3)
vars.in

plot_Data_anthro<-vars.in[,c("names","x","y")]
plot_Data_anthro$names<-c("HAZ","WAZ", "WHZ", "MUAC")
colnames(plot_Data_anthro)

p1<-ggplot(aes(x=x, y=y), data=plot_Data_anthro)+
  geom_point()+
  # geom_circle(r = 1) +
  scale_x_continuous(lim=c(-1.2,1.2))+
  scale_y_continuous(lim=c(-1.2,1.2))+
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE, colour="grey30", size=0.75)+
  geom_circle(aes(x0 = 0, y0 = 0, r = 0.5), inherit.aes = FALSE, colour="grey50", size=0.5, linetype=2)+
  geom_hline(yintercept=0, linetype="dashed",color="grey") +
  geom_vline(xintercept=0, linetype="dashed", color="grey") +
  geom_segment(aes(xend = x, yend = y, x = 0, y = 0),size = 0.75, colour = "black", data = plot_Data_anthro,
               arrow = arrow(length = unit(10, "points"), type = "open", angle = 25))+
  geom_dl(aes(x = x, y = y, label = names),
          method = list(dl.trans(x = x - 0.2, y = y + 0), "first.bumpup", cex = 1.2)) +
  labs(x = "Component-1", y = "Component-2")+
  theme_bw()+
  theme(axis.title = element_text(size = 14), axis.text=element_text(color = "grey30", size=14))
p1




vars.in_anthro<-plotIndiv(out.plsDA)[[1]]


p2<-ggplot(aes(x=x, y=y), data=vars.in_anthro)+
  geom_point(aes(col=group, fill=group), shape=21, size=3)+
  scale_shape_manual(values=c(19,21,3,19,21,3))+
  #scale_x_continuous(lim=c(0,2200))+
  #scale_y_continuous(lim=c(0,130))+
  scale_color_manual(values=c("black","red"))+
  scale_fill_manual(values=alpha(c("black","red"), 0.5))+
  labs(x = paste0("Variate-1 (", 100*round(out.plsDA$explained_variance$X[1],2) , "% explained variance)"), y = paste0("Variate-2 (", 100*round(out.plsDA$explained_variance$X[2],2) , "% explained variance)"))+
  theme_bw()+
  theme(axis.title = element_text(size = 14), axis.text=element_text(color = "grey30", size=14), legend.position = "right")+
  stat_ellipse(aes(group = na.omit(group)), na.rm = FALSE, show.legend = NA, level = 0.95) +
  #stat_conf_ellipse(aes(group = na.omit(group)), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, level = 0.95, npoint = 100, bary = TRUE) +
  stat_mean(aes(group = group),size=2, shape=8, color= c("black","salmon"), na.rm = TRUE, show.legend = NA,
            inherit.aes = TRUE)
p2





#####################################################################
### data with antrho and BIA

#only anthro
X<-as.matrix(idata[,c("haz_adm","waz_adm","whz_adm","muac", "R.H_1","Xc.H_1","mean_ph_degrees_1")])

### log all variables
X<-log(X+10)
outcome<-as.factor(idata$Alive)
### plsda between groups
out.plsDA<-plsda(X,outcome, ncomp=2, scale=TRUE, mode="regression")
out.plsDA$explained_variance

#perf.out<-mixOmics::perf(out.plsDA, validation = c("loo"), progressBar = TRUE, auc=TRUE)
#summary(perf.out)

# get.confusion_matrix(truth = pls.data$Group_Kwa, predicted=perf.out$predict)
# 
# perf.out$error.rate
# perf.out$error.rate.class
# perf.out$predict
# perf.out$class
# plot(perf.out, overlay = 'measure', sd=TRUE)
# plot(perf.out$error.rate.class, overlay = 'measure', sd=TRUE)


vars.in_BIA<-plotVar(out.plsDA, plot=TRUE, cex=3, cutoff=0.3)
vars.in_BIA

plot_Data_BIA<-vars.in_BIA[,c("names","x","y")]
plot_Data_BIA$names<-c("HAZ","WAZ", "WHZ", "MUAC", "R/H", "Xc/H", "PA")
colnames(plot_Data)

p3<-ggplot(aes(x=x, y=y), data=plot_Data_BIA)+
  geom_point()+
  # geom_circle(r = 1) +
  scale_x_continuous(lim=c(-1.2,1.2))+
  scale_y_continuous(lim=c(-1.2,1.2))+
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE, colour="grey30", size=0.75)+
  geom_circle(aes(x0 = 0, y0 = 0, r = 0.5), inherit.aes = FALSE, colour="grey50", size=0.5, linetype=2)+
  geom_hline(yintercept=0, linetype="dashed",color="grey") +
  geom_vline(xintercept=0, linetype="dashed", color="grey") +
  geom_segment(aes(xend = x, yend = y, x = 0, y = 0),size = 0.75, colour = "black", data = plot_Data_BIA,
               arrow = arrow(length = unit(10, "points"), type = "open", angle = 25))+
  geom_dl(aes(x = x, y = y, label = names),
          method = list(dl.trans(x = x - 0.2, y = y + 0), "first.bumpup", cex = 1.2)) +
  labs(x = "Component-1", y = "Component-2")+
  theme_bw()+
  theme(axis.title = element_text(size = 14), axis.text=element_text(color = "grey30", size=14))
p3





vars.in_BIA<-plotIndiv(out.plsDA)[[1]]


p4<-ggplot(aes(x=x, y=y), data=vars.in_BIA)+
  geom_point(aes(col=group, fill=group), shape=21, size=3)+
  scale_shape_manual(values=c(19,21,3,19,21,3))+
  #scale_x_continuous(lim=c(0,2200))+
  #scale_y_continuous(lim=c(0,130))+
  scale_color_manual(values=c("black","red"))+
  scale_fill_manual(values=alpha(c("black","red"), 0.5))+
  labs(x = paste0("Variate-1 (", 100*round(out.plsDA$explained_variance$X[1],2) , "% explained variance)"), y = paste0("Variate-2 (", 100*round(out.plsDA$explained_variance$X[2],2) , "% explained variance)"))+
  theme_bw()+
  theme(axis.title = element_text(size = 14), axis.text=element_text(color = "grey30", size=14), legend.position = "right")+
  stat_ellipse(aes(group = na.omit(group)), na.rm = FALSE, show.legend = NA, level = 0.95) +
  #stat_conf_ellipse(aes(group = na.omit(group)), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, level = 0.95, npoint = 100, bary = TRUE) +
  stat_mean(aes(group = group),size=2, shape=8, color= c("black","red"), na.rm = TRUE, show.legend = NA,
            inherit.aes = TRUE)
p4


p_all<-ggarrange(p1, p3, p2,p4, ncol = 2, nrow =2, common.legend = TRUE, legend = "bottom") 
p_all



# saves as .svg size 1000 800
ggsave(file="SuplFig4_PLSDA_anthroOnly_vs_withBIVA_byMortality.svg", plot=p_all, width=10, height=10)
ggsave(file="SuplFig4_PLSDA_anthroOnly_vs_withBIVA_byMortality.pdf", plot=p_all, width=10, height=10)





#### Making Roc curves
colnames(idata)

#####################################################################
###  MArasmus vs CP

### data with only antrho
X<-as.matrix(idata[,c("haz_adm","waz_adm","whz_adm","muac")])
### log all variables
X<-log(X+10)
outcome<-as.factor(idata$Alive)
### plsda between groups 
out.plsDA<-plsda(X,outcome, ncomp=2, scale=TRUE, mode="regression")

auc_SvsD<-auroc(out.plsDA,roc.comp=1)
auc_SvsD_data<-auc_SvsD$graph.Comp1$data


#####################################################################
### data with antrho and BIA
X<-as.matrix(idata[,c("haz_adm","waz_adm","whz_adm","muac", "R.H_1","Xc.H_1","mean_ph_degrees_1")])
### log all variables
X<-log(X+10)
outcome<-as.factor(idata$Alive)
### plsda between groups
out.plsDA<-plsda(X,outcome, ncomp=2, scale=TRUE, mode="regression")

auc_SvsD_BIA<-auroc(out.plsDA,roc.comp=1)
auc_SvsD_BIA_data<-auc_SvsD_BIA$graph.Comp1$data



auc_SvsD_plot<-ggplot()+
  geom_line(aes(x=Specificity, y=Sensitivity), color="grey10",size = 1.25, alpha=0.75, data=auc_SvsD_data)+
  geom_line(aes(x=Specificity, y=Sensitivity),color="grey60", size = 1.25, alpha=0.75, data=auc_SvsD_BIA_data)+
  geom_segment(aes(xend = 100, yend = 100, x = 0, y = 0), size = 0.75, colour = "grey60", linetype="dashed")+
  labs(title= "Survival vs. Death", x = "100 - Specificity (%)", 
       y = "Sensitivity (%)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        title = element_text(size = 16),
        axis.title = element_text(size = 14), 
        axis.text=element_text(color = "grey30", size=14), legend.position = "right")

auc_SvsD_plot




# saves as .svg size 1000 800
ggsave(file="SuplFig4_PLSDA_anthroOnly_vs_withBIVA_byMortality_AUC.svg", plot=auc_SvsD_plot, width=3.33, height=5)
ggsave(file="SuplFig4_PLSDA_anthroOnly_vs_withBIVA_byMortality_AUC.pdf", plot=auc_SvsD_plot, width=3.33, height=5)


