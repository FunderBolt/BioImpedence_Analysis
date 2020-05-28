
######################
# BIA data calculate mean and SD for variables of different groups
library(dplyr)
library(here)
library(janitor)
library(reshape2)
library(lme4)

### read in dataset
dataset <- read.csv(here("3-Data","2020-05-20_All.BIA.data_v23.csv"), row.names = 1)
dataset$subid<-row.names(dataset)
colnames(dataset)

# set group.number as a factor
dataset$Participant<-as.factor(dataset$Participant)
dataset$Group<-as.factor(dataset$Group)

dataset<-dataset[!rownames(dataset) == "30001702",]

colnames(dataset)

#################### Table 1
Tbl_1<-data.frame(1:6)

### between controls and SAM
split_perGroup<- split(dataset, f=dataset$Participant)

Tbl_1[,1]<-c("Total","Resistance, ohm","Reactance, ohm", "Phase angle, degree","Resistance index, ohm/m","Reactance index, ohm/m")

Tbl_1[,2]<- c(paste0("Control ", "n=",length(split_perGroup[[1]]$mean_r_ohm_1[complete.cases(split_perGroup[[1]]$mean_r_ohm_1)])),
              paste0(round(mean(split_perGroup[[1]]$mean_r_ohm_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[1]]$mean_r_ohm_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[1]]$mean_xc_ohm_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$mean_xc_ohm_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$mean_ph_degrees_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$mean_ph_degrees_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$R.H_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[1]]$R.H_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[1]]$Xc.H_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$Xc.H_1, na.rm=T), digits=1)))

Tbl_1[,3]<- c(paste0("SAM ", "n=", length(split_perGroup[[2]]$mean_r_ohm_1[complete.cases(split_perGroup[[2]]$mean_r_ohm_1)])),
              paste0(round(mean(split_perGroup[[2]]$mean_r_ohm_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[2]]$mean_r_ohm_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[2]]$mean_xc_ohm_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$mean_xc_ohm_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$mean_ph_degrees_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$mean_ph_degrees_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$R.H_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[2]]$R.H_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[2]]$Xc.H_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$Xc.H_1, na.rm=T), digits=1)))


Tbl_1[,4]<- c("p", 
              signif(coef(summary(glm(dataset$Participant~dataset$mean_r_ohm_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(dataset$Participant~dataset$mean_xc_ohm_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(dataset$Participant~dataset$mean_ph_degrees_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(dataset$Participant~dataset$R.H_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(dataset$Participant~dataset$Xc.H_1,family=binomial)))[2,4], digits=2))


#### SAM split by phenotype
split_perGroup<- split(dataset[dataset$Participant == "inhospital",], f=dataset$kwash[dataset$Participant == "inhospital"])

Tbl_1[,5]<- c(paste0("Severe wasting ", "n=",length(split_perGroup[[1]]$mean_r_ohm_1[complete.cases(split_perGroup[[1]]$mean_r_ohm_1)])),
              paste0(round(mean(split_perGroup[[1]]$mean_r_ohm_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[1]]$mean_r_ohm_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[1]]$mean_xc_ohm_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$mean_xc_ohm_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$mean_ph_degrees_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$mean_ph_degrees_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$R.H_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[1]]$R.H_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[1]]$Xc.H_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$Xc.H_1, na.rm=T), digits=1)))
              
              

Tbl_1[,6]<- c(paste0("Edematous ", "n=",length(split_perGroup[[2]]$mean_r_ohm_1[complete.cases(split_perGroup[[2]]$mean_r_ohm_1)])),
              paste0(round(mean(split_perGroup[[2]]$mean_r_ohm_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[2]]$mean_r_ohm_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[2]]$mean_xc_ohm_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$mean_xc_ohm_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$mean_ph_degrees_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$mean_ph_degrees_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$R.H_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[2]]$R.H_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[2]]$Xc.H_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$Xc.H_1, na.rm=T), digits=1)))


#### SAM split by phenotype
SAM_data<-dataset[dataset$Participant == "inhospital",]
SAM_data$kwash<-as.factor(SAM_data$kwash)


Tbl_1[,7]<- c("p",
              signif(coef(summary(glm(SAM_data$kwash~SAM_data$mean_r_ohm_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$kwash~SAM_data$mean_xc_ohm_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$kwash~SAM_data$mean_ph_degrees_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$kwash~SAM_data$R.H_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$kwash~SAM_data$Xc.H_1,family=binomial)))[2,4], digits=2))


#### SAM split by phenotype
SAM_data <- dataset[dataset$kwash  == "no" | dataset$Group == 3 ,]
SAM_data$kwash<-as.factor(SAM_data$kwash)

Tbl_1[,8]<- c("p",
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$mean_r_ohm_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$mean_xc_ohm_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$mean_ph_degrees_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$R.H_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$Xc.H_1,family=binomial)))[2,4], digits=2))


#### SAM split by phenotype
SAM_data <- dataset[dataset$kwash  == "yes" | dataset$Group == 3 ,]
SAM_data$kwash<-as.factor(SAM_data$kwash)

Tbl_1[,9]<- c("p",
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$mean_r_ohm_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$mean_xc_ohm_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$mean_ph_degrees_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$R.H_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$Xc.H_1,family=binomial)))[2,4], digits=2))



#### SAM split by phenotype
split_perGroup<- split(dataset, f=dataset$Participant)

#colnames(dataset)
Tbl_1[,10]<- c(paste0("SAM ", "n=", length(split_perGroup[[2]]$mean_r_ohm_2[complete.cases(split_perGroup[[2]]$mean_r_ohm_2)])),
              paste0(round(mean(split_perGroup[[2]]$mean_r_ohm_2, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[2]]$mean_r_ohm_2, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[2]]$mean_xc_ohm_2, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$mean_xc_ohm_2, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$mean_ph_degrees_2, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$mean_ph_degrees_2, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$R.H_2, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[2]]$R.H_2, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[2]]$Xc.H_2, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$Xc.H_2, na.rm=T), digits=1)))



#### SAM split by phenotype
split_perGroup<- split(dataset[dataset$Participant == "inhospital",], f=dataset$kwash[dataset$Participant == "inhospital"])

Tbl_1[,11]<- c(paste0("Severe wasting ", "n=",length(split_perGroup[[1]]$mean_r_ohm_2[complete.cases(split_perGroup[[1]]$mean_r_ohm_2)])),
              paste0(round(mean(split_perGroup[[1]]$mean_r_ohm_2, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[1]]$mean_r_ohm_2, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[1]]$mean_xc_ohm_2, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$mean_xc_ohm_2, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$mean_ph_degrees_2, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$mean_ph_degrees_2, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$R.H_2, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[1]]$R.H_2, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[1]]$Xc.H_2, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$Xc.H_2, na.rm=T), digits=1)))


Tbl_1[,12]<- c(paste0("Edematous ", "n=",length(split_perGroup[[2]]$mean_r_ohm_2[complete.cases(split_perGroup[[2]]$mean_r_ohm_2)])),
              paste0(round(mean(split_perGroup[[2]]$mean_r_ohm_2, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[2]]$mean_r_ohm_2, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[2]]$mean_xc_ohm_2, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$mean_xc_ohm_2, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$mean_ph_degrees_2, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$mean_ph_degrees_2, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$R.H_2, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[2]]$R.H_2, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[2]]$Xc.H_2, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$Xc.H_2, na.rm=T), digits=1)))


#### SAM split by phenotype
SAM_data<-dataset[dataset$Participant == "inhospital",]
SAM_data$kwash<-as.factor(SAM_data$kwash)


Tbl_1[,13]<- c("p",
              signif(coef(summary(glm(SAM_data$kwash~SAM_data$mean_r_ohm_2,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$kwash~SAM_data$mean_xc_ohm_2,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$kwash~SAM_data$mean_ph_degrees_2,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$kwash~SAM_data$R.H_2,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$kwash~SAM_data$Xc.H_2,family=binomial)))[2,4], digits=2))


#### SAM split by phenotype
SAM_data <- dataset[dataset$kwash  == "no" | dataset$Group == 3 ,]
SAM_data$kwash<-as.factor(SAM_data$kwash)

Tbl_1[,14]<- c("p",
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$mean_r_ohm_2,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$mean_xc_ohm_2,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$mean_ph_degrees_2,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$R.H_2,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$Xc.H_2,family=binomial)))[2,4], digits=2))


#### SAM split by phenotype
SAM_data <- dataset[dataset$kwash  == "yes" | dataset$Group == 3 ,]
SAM_data$kwash<-as.factor(SAM_data$kwash)

Tbl_1[,15]<- c("p",
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$mean_r_ohm_2,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$mean_xc_ohm_2,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$mean_ph_degrees_2,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$R.H_2,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Participant~SAM_data$Xc.H_2,family=binomial)))[2,4], digits=2))



#### Differences between time points


#### SAM in only wasting
SAM_data <- dataset[dataset$kwash  == "no",]


### make long dataset
#dput(colnames(SAM_data))
R_ohm_timepoint<-SAM_data %>% select(c("subid","mean_r_ohm_1", "mean_r_ohm_2")) %>% melt
Xc_ohm_timepoint<-SAM_data %>% select(c("subid","mean_xc_ohm_1", "mean_xc_ohm_2")) %>% melt
Ph_timepoint<-SAM_data %>% select(c("subid","mean_ph_degrees_1", "mean_ph_degrees_2")) %>% melt
RH_timepoint<-SAM_data %>% select(c("subid","R.H_1", "R.H_2")) %>% melt
XcH_timepoint<-SAM_data %>% select(c("subid","Xc.H_1", "Xc.H_2")) %>% melt
head(timepoint_data)


Tbl_1[,16]<- c("p",
               signif(coef(summary(glmer(variable~value + (1|subid), data=R_ohm_timepoint,family=binomial, na.action=na.exclude)))[2,4], digits=2),
               signif(coef(summary(glmer(variable~value + (1|subid), data=Xc_ohm_timepoint, family=binomial, na.action=na.exclude)))[2,4], digits=2),
               signif(coef(summary(glmer(variable~value + (1|subid), data=Ph_timepoint, family=binomial, na.action=na.exclude)))[2,4], digits=2),
               signif(coef(summary(glmer(variable~value + (1|subid), data=RH_timepoint, family=binomial, na.action=na.exclude)))[2,4], digits=2),
               signif(coef(summary(glmer(variable~value + (1|subid), data=XcH_timepoint, family=binomial, na.action=na.exclude)))[2,4], digits=2))


#### SAM in only wasting
SAM_data <- dataset[dataset$kwash  == "yes",]


### make long dataset
dput(colnames(SAM_data))
R_ohm_timepoint<-SAM_data %>% select(c("subid","mean_r_ohm_1", "mean_r_ohm_2")) %>% melt
Xc_ohm_timepoint<-SAM_data %>% select(c("subid","mean_xc_ohm_1", "mean_xc_ohm_2")) %>% melt
Ph_timepoint<-SAM_data %>% select(c("subid","mean_ph_degrees_1", "mean_ph_degrees_2")) %>% melt
RH_timepoint<-SAM_data %>% select(c("subid","R.H_1", "R.H_2")) %>% melt
XcH_timepoint<-SAM_data %>% select(c("subid","Xc.H_1", "Xc.H_2")) %>% melt
head(timepoint_data)


Tbl_1[,17]<- c("p",
               signif(coef(summary(glmer(variable~value  + (1|subid),data=R_ohm_timepoint, family=binomial)))[2,4], digits=2),
               signif(coef(summary(glmer(variable~value  + (1|subid), data=Xc_ohm_timepoint, family=binomial)))[2,4], digits=2),
               signif(coef(summary(glmer(variable~value  + (1|subid), data=Ph_timepoint,  family=binomial)))[2,4], digits=2),
               signif(coef(summary(glmer(variable~value  + (1|subid), data=RH_timepoint, family=binomial)))[2,4], digits=2),
               signif(coef(summary(glmer(variable~value  + (1|subid), data=XcH_timepoint, family=binomial)))[2,4], digits=2))


Tbl_1

write.csv(Tbl_1, file=paste0(Sys.Date(),"_Table2_BIAvalues_byGroups.csv"))







#################### Table 1
Tbl_2<-data.frame(1:6)

### between controls and SAM
split_perGroup<- split(dataset, f=dataset$Participant)

Tbl_2[,1]<-c("Total","Resistance, ohm","Reactance, ohm", "Phase angle, degree","Resistance index, ohm/m","Reactance index, ohm/m")

Tbl_2[,2]<- c(paste0("Control ", "n=",length(split_perGroup[[1]]$mean_r_ohm_1[complete.cases(split_perGroup[[1]]$mean_r_ohm_1)])),
              paste0(round(mean(split_perGroup[[1]]$mean_r_ohm_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[1]]$mean_r_ohm_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[1]]$mean_xc_ohm_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$mean_xc_ohm_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$mean_ph_degrees_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$mean_ph_degrees_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$R.H_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[1]]$R.H_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[1]]$Xc.H_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$Xc.H_1, na.rm=T), digits=1)))


### between controls and SAM
colnames(dataset)
split_perGroup<- split(dataset[!dataset$Group ==3,], f=dataset$Group[!dataset$Group ==3])


Tbl_2[,3]<- c(paste0("SAM ", "n=", length(split_perGroup[[1]]$mean_r_ohm_1[complete.cases(split_perGroup[[1]]$mean_r_ohm_1)])),
              paste0(round(mean(split_perGroup[[1]]$mean_r_ohm_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[1]]$mean_r_ohm_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[1]]$mean_xc_ohm_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$mean_xc_ohm_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$mean_ph_degrees_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$mean_ph_degrees_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$R.H_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[1]]$R.H_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[1]]$Xc.H_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$Xc.H_1, na.rm=T), digits=1)))


### between controls and SAM
colnames(dataset)
split_perGroup<- split(dataset[dataset$kwash =="no" & !dataset$Group ==3,], f=dataset$Group[dataset$kwash =="no" & !dataset$Group ==3])


Tbl_2[,4]<- c(paste0("SAM ", "n=", length(split_perGroup[[1]]$mean_r_ohm_1[complete.cases(split_perGroup[[1]]$mean_r_ohm_1)])),
              paste0(round(mean(split_perGroup[[1]]$mean_r_ohm_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[1]]$mean_r_ohm_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[1]]$mean_xc_ohm_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$mean_xc_ohm_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$mean_ph_degrees_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$mean_ph_degrees_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$R.H_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[1]]$R.H_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[1]]$Xc.H_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$Xc.H_1, na.rm=T), digits=1)))



### between controls and SAM
colnames(dataset)
split_perGroup<- split(dataset[dataset$kwash =="yes" & !dataset$Group ==3,], f=dataset$Group[dataset$kwash =="yes" & !dataset$Group ==3])

Tbl_2[,5]<- c(paste0("SAM ", "n=", length(split_perGroup[[1]]$mean_r_ohm_1[complete.cases(split_perGroup[[1]]$mean_r_ohm_1)])),
              paste0(round(mean(split_perGroup[[1]]$mean_r_ohm_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[1]]$mean_r_ohm_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[1]]$mean_xc_ohm_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$mean_xc_ohm_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$mean_ph_degrees_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$mean_ph_degrees_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$R.H_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[1]]$R.H_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[1]]$Xc.H_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$Xc.H_1, na.rm=T), digits=1)))




### between controls and SAM
colnames(dataset)
split_perGroup<- split(dataset[!dataset$Group ==3,], f=dataset$Group[!dataset$Group ==3])


Tbl_2[,6]<- c(paste0("SAM ", "n=", length(split_perGroup[[2]]$mean_r_ohm_1[complete.cases(split_perGroup[[2]]$mean_r_ohm_1)])),
              paste0(round(mean(split_perGroup[[2]]$mean_r_ohm_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[2]]$mean_r_ohm_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[2]]$mean_xc_ohm_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$mean_xc_ohm_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$mean_ph_degrees_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$mean_ph_degrees_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$R.H_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[2]]$R.H_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[2]]$Xc.H_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$Xc.H_1, na.rm=T), digits=1)))



### between controls and SAM
colnames(dataset)
split_perGroup<- split(dataset[dataset$kwash =="no" & !dataset$Group ==3,], f=dataset$Group[dataset$kwash =="no" & !dataset$Group ==3])


Tbl_2[,7]<- c(paste0("SAM ", "n=", length(split_perGroup[[2]]$mean_r_ohm_1[complete.cases(split_perGroup[[2]]$mean_r_ohm_1)])),
              paste0(round(mean(split_perGroup[[2]]$mean_r_ohm_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[2]]$mean_r_ohm_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[2]]$mean_xc_ohm_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$mean_xc_ohm_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$mean_ph_degrees_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$mean_ph_degrees_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$R.H_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[2]]$R.H_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[2]]$Xc.H_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$Xc.H_1, na.rm=T), digits=1)))



### between controls and SAM
colnames(dataset)
split_perGroup<- split(dataset[dataset$kwash =="yes" & !dataset$Group ==3,], f=dataset$Group[dataset$kwash =="yes" & !dataset$Group ==3])

Tbl_2[,8]<- c(paste0("SAM ", "n=", length(split_perGroup[[2]]$mean_r_ohm_1[complete.cases(split_perGroup[[2]]$mean_r_ohm_1)])),
              paste0(round(mean(split_perGroup[[2]]$mean_r_ohm_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[2]]$mean_r_ohm_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[2]]$mean_xc_ohm_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$mean_xc_ohm_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$mean_ph_degrees_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$mean_ph_degrees_1, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$R.H_1, na.rm=T),digits=0), " ± ", round(sd(split_perGroup[[2]]$R.H_1, na.rm=T), digits=0)),
              paste0(round(mean(split_perGroup[[2]]$Xc.H_1, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$Xc.H_1, na.rm=T), digits=1)))


SAMdata<-dataset[!dataset$Group ==3,]
SAMdata$Group<-as.factor(SAMdata$Group)

Tbl_2[,9]<- c("p",
              signif(coef(summary(glm(Group~mean_r_ohm_1, data=SAMdata,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(Group~mean_xc_ohm_1, data=SAMdata,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(Group~mean_ph_degrees_1, data=SAMdata,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(Group~R.H_1, data=SAMdata,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(Group~Xc.H_1, data=SAMdata,family=binomial)))[2,4], digits=2))


#### SAM split by phenotype
SAM_data <- dataset[dataset$kwash  == "no" | dataset$Group == 3 ,]

Tbl_2[,10]<- c("p",signif(coef(summary(glm(SAM_data$Group~SAM_data$mean_r_ohm_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Group~SAM_data$mean_xc_ohm_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Group~SAM_data$mean_ph_degrees_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Group~SAM_data$R.H_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Group~SAM_data$Xc.H_1,family=binomial)))[2,4], digits=2))




# split by phenotype
SAM_data <- dataset[dataset$kwash  == "yes" | dataset$Group == 3 ,]


Tbl_2[,11]<- c("p",
              signif(coef(summary(glm(SAM_data$Group~SAM_data$mean_r_ohm_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Group~SAM_data$mean_xc_ohm_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Group~SAM_data$mean_ph_degrees_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Group~SAM_data$R.H_1,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Group~SAM_data$Xc.H_1,family=binomial)))[2,4], digits=2))



write.csv(Tbl_2, file=paste0(Sys.Date(),"_SuplmTable3_BIAvalues_byMortality.csv"))




