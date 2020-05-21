######################
# BIA: Participant characteristics: calculate mean and SD for variables of different groups (with stat test)
library(dplyr)
library(here)
library(janitor)

### read in dataset
dataset <- read.csv(here("3-Data","2020-05-20_All.BIA.data_v23.csv"), row.names = 1)
colnames(dataset)

# set group.number as a factor
dataset$Participant<-as.factor(dataset$Participant)
dataset$Group<-as.factor(dataset$Group)



#################### Table 1
Tbl_1<-data.frame(1:12)

split_perGroup<- split(dataset, f=dataset$Participant)

Tbl_1[,1]<-c("Total","Male, n(%)","HIV reactive, n (%)", "Age, mon","Height-for-age, z-score","Weight-for-age, z-score", "Weight-for-height, z-score",
            " MUAC, cm", "Time to stabilization, days", "Duration of admission, days", "Death, n(%)","Time to death, days")

Tbl_1[,2]<- c(paste0("Control ", "n=",nrow(split_perGroup[[1]])),
              paste0(tabyl(split_perGroup[[1]]$sex)[2,c(2)]," (", round(100*tabyl(split_perGroup[[1]]$sex)[2,c(3)],digits=0),")"),
              paste0(tabyl(split_perGroup[[1]]$hivtest)[2,c(2)]," (", round(100*tabyl(split_perGroup[[1]]$hivtest)[2,c(4)],digits=0),")"),
              paste0(round(mean(split_perGroup[[1]]$AgeMonths_Cal, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$AgeMonths_Cal, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$haz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$haz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$waz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$waz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$whz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$whz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$muac, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$muac, na.rm=T), digits=1)),
              rep("-",4))

Tbl_1[,3]<- c(paste0("SAM ", "n=",nrow(split_perGroup[[2]])),
              paste0(tabyl(split_perGroup[[2]]$sex)[2,c(2)]," (", round(100*tabyl(split_perGroup[[2]]$sex)[2,c(3)],digits=0),")"),
              paste0(tabyl(split_perGroup[[2]]$hivtest)[2,c(2)]," (", round(100*tabyl(split_perGroup[[2]]$hivtest)[2,c(4)],digits=0),")"),
              paste0(round(mean(split_perGroup[[2]]$AgeMonths_Cal, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$AgeMonths_Cal, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$haz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$haz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$waz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$waz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$whz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$whz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$muac, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$muac, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$timeToStab, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$timeToStab, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$dur_adm[split_perGroup[[2]]$Alive_1Yes.2No == 1], na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$dur_adm[split_perGroup[[2]]$Alive_1Yes.2No == 1], na.rm=T), digits=1)),
              paste0(tabyl(split_perGroup[[2]]$Alive_1Yes.2No)[2,c(2)]," (", round(100*tabyl(split_perGroup[[2]]$Alive_1Yes.2No)[2,c(3)],digits=0),")"),
              paste0(round(mean(split_perGroup[[2]]$dur_adm[split_perGroup[[2]]$Alive_1Yes.2No == 2], na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$dur_adm[split_perGroup[[2]]$Alive_1Yes.2No == 2], na.rm=T), digits=1)))


Tbl_1[,4]<- c("p", 
              signif(fisher.test(dataset$Participant, dataset$sex)[[1]], digits=2),
              signif(fisher.test(dataset$Participant, dataset$hivtest)[[1]], digits=2),
              signif(coef(summary(glm(dataset$Participant~dataset$AgeMonths_Cal,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(dataset$Participant~dataset$haz_adm,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(dataset$Participant~dataset$waz_adm,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(dataset$Participant~dataset$whz_adm,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(dataset$Participant~dataset$muac,family=binomial)))[2,4], digits=2),
              rep("-",4))



split_perGroup<- split(dataset[dataset$Participant == "inhospital",], f=dataset$kwash[dataset$Participant == "inhospital"])

Tbl_1[,5]<- c(paste0("Severe wasting ", "n=",nrow(split_perGroup[[1]])),
              paste0(tabyl(split_perGroup[[1]]$sex)[2,c(2)]," (", round(100*tabyl(split_perGroup[[1]]$sex)[2,c(3)],digits=0),")"),
              paste0(tabyl(split_perGroup[[1]]$hivtest)[2,c(2)]," (", round(100*tabyl(split_perGroup[[1]]$hivtest)[2,c(4)],digits=0),")"),
              paste0(round(mean(split_perGroup[[1]]$AgeMonths_Cal, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$AgeMonths_Cal, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$haz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$haz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$waz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$waz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$whz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$whz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$muac, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$muac, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$timeToStab, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$timeToStab, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$dur_adm[split_perGroup[[1]]$Alive_1Yes.2No == 1], na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$dur_adm[split_perGroup[[1]]$Alive_1Yes.2No == 1], na.rm=T), digits=1)),
              paste0(tabyl(split_perGroup[[1]]$Alive_1Yes.2No)[2,c(2)]," (", round(100*tabyl(split_perGroup[[1]]$Alive_1Yes.2No)[2,c(3)],digits=0),")"),
              paste0(round(mean(split_perGroup[[1]]$dur_adm[split_perGroup[[1]]$Alive_1Yes.2No == 2], na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$dur_adm[split_perGroup[[1]]$Alive_1Yes.2No == 2], na.rm=T), digits=1)))

Tbl_1[,6]<- c(paste0("Edematous ", "n=",nrow(split_perGroup[[2]])),
              paste0(tabyl(split_perGroup[[2]]$sex)[2,c(2)]," (", round(100*tabyl(split_perGroup[[2]]$sex)[2,c(3)],digits=0),")"),
              paste0(tabyl(split_perGroup[[2]]$hivtest)[2,c(2)]," (", round(100*tabyl(split_perGroup[[2]]$hivtest)[2,c(4)],digits=0),")"),
              paste0(round(mean(split_perGroup[[2]]$AgeMonths_Cal, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$AgeMonths_Cal, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$haz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$haz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$waz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$waz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$whz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$whz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$muac, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$muac, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$timeToStab, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$timeToStab, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$dur_adm[split_perGroup[[2]]$Alive_1Yes.2No == 1], na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$dur_adm[split_perGroup[[2]]$Alive_1Yes.2No == 1], na.rm=T), digits=1)),
              paste0(tabyl(split_perGroup[[2]]$Alive_1Yes.2No)[2,c(2)]," (", round(100*tabyl(split_perGroup[[2]]$Alive_1Yes.2No)[2,c(3)],digits=0),")"),
              paste0(round(mean(split_perGroup[[2]]$dur_adm[split_perGroup[[2]]$Alive_1Yes.2No == 2], na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$dur_adm[split_perGroup[[2]]$Alive_1Yes.2No == 2], na.rm=T), digits=1)))

Tbl_1[,7]<- c("p", rep("-",11))


SAM_data<-dataset[dataset$Participant == "inhospital",]
SAM_data$kwash<-as.factor(SAM_data$kwash)


Tbl_1[,7]<- c("p", signif(fisher.test(SAM_data$kwash, SAM_data$sex)[[1]], digits=2),
              signif(fisher.test(SAM_data$kwash, SAM_data$hivtest)[[1]], digits=2),
              signif(coef(summary(glm(SAM_data$kwash~SAM_data$AgeMonths_Cal,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$kwash~SAM_data$haz_adm,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$kwash~SAM_data$waz_adm,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$kwash~SAM_data$whz_adm,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$kwash~SAM_data$muac,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$kwash~SAM_data$timeToStab,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$kwash[SAM_data$Alive_1Yes.2No == 1]~SAM_data$dur_adm[SAM_data$Alive_1Yes.2No == 1],family=binomial)))[2,4], digits=2),
              signif(fisher.test(SAM_data$kwash, SAM_data$Alive_1Yes.2No)[[1]], digits=2),
              signif(coef(summary(glm(SAM_data$kwash[SAM_data$Alive_1Yes.2No == 2]~SAM_data$dur_adm[SAM_data$Alive_1Yes.2No == 2],family=binomial)))[2,4], digits=2))


Tbl_1
  
write.csv(Tbl_1, file=paste0(Sys.Date(),"_Table1_Participant_Characteristics_byGroups.csv"))





#################### Supplemental Table 2
Tbl_2<-data.frame(1:12)

split_perGroup<- split(dataset, f=dataset$Group)

Tbl_2[,1]<-c("Total","Male, n(%)","HIV reactive, n (%)", "Edema, n(%)","Age, mon","Height-for-age, z-score","Weight-for-age, z-score", "Weight-for-height, z-score",
             " MUAC, cm", "Time to stabilization, days", "Duration of admission, days", "Time to death, days")

Tbl_2[,2]<- c(paste0("Control ", "n=",nrow(split_perGroup[[3]])),
              paste0(tabyl(split_perGroup[[3]]$sex)[2,c(2)]," (", round(100*tabyl(split_perGroup[[3]]$sex)[2,c(3)],digits=0),")"),
              paste0(tabyl(split_perGroup[[3]]$hivtest)[2,c(2)]," (", round(100*tabyl(split_perGroup[[3]]$hivtest)[2,c(4)],digits=0),")"),
              rep("-",1),
              paste0(round(mean(split_perGroup[[3]]$AgeMonths_Cal, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[3]]$AgeMonths_Cal, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[3]]$haz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[3]]$haz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[3]]$waz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[3]]$waz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[3]]$whz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[3]]$whz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[3]]$muac, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[3]]$muac, na.rm=T), digits=1)),
              rep("-",3))

Tbl_2[,3]<- c(paste0("Survived ", "n=",nrow(split_perGroup[[1]])),
              paste0(tabyl(split_perGroup[[1]]$sex)[2,c(2)]," (", round(100*tabyl(split_perGroup[[1]]$sex)[2,c(3)],digits=0),")"),
              paste0(tabyl(split_perGroup[[1]]$hivtest)[2,c(2)]," (", round(100*tabyl(split_perGroup[[1]]$hivtest)[2,c(4)],digits=0),")"),
              paste0(tabyl(split_perGroup[[1]]$kwash)[2,c(2)]," (", round(100*tabyl(split_perGroup[[1]]$kwash)[2,c(3)],digits=0),")"),
              paste0(round(mean(split_perGroup[[1]]$AgeMonths_Cal, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$AgeMonths_Cal, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$haz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$haz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$waz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$waz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$whz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$whz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$muac, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$muac, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$timeToStab, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$timeToStab, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$dur_adm[split_perGroup[[1]]$Alive_1Yes.2No == 1], na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$dur_adm[split_perGroup[[1]]$Alive_1Yes.2No == 1], na.rm=T), digits=1)),
              rep("-",1))

Tbl_2[,4]<- c(paste0("Died ", "n=",nrow(split_perGroup[[2]])),
              paste0(tabyl(split_perGroup[[2]]$sex)[2,c(2)]," (", round(100*tabyl(split_perGroup[[2]]$sex)[2,c(3)],digits=0),")"),
              paste0(tabyl(split_perGroup[[2]]$hivtest)[2,c(2)]," (", round(100*tabyl(split_perGroup[[2]]$hivtest)[2,c(4)],digits=0),")"),
              paste0(tabyl(split_perGroup[[2]]$kwash)[2,c(2)]," (", round(100*tabyl(split_perGroup[[2]]$kwash)[2,c(3)],digits=0),")"),
              paste0(round(mean(split_perGroup[[2]]$AgeMonths_Cal, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$AgeMonths_Cal, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$haz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$haz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$waz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$waz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$whz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$whz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$muac, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$muac, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$timeToStab, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$timeToStab, na.rm=T), digits=1)),
              rep("-",1),
              paste0(round(mean(split_perGroup[[2]]$dur_adm[split_perGroup[[2]]$Alive_1Yes.2No == 2], na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$dur_adm[split_perGroup[[2]]$Alive_1Yes.2No == 2], na.rm=T), digits=1)))



Tbl_2[,5]<- c("p", 
              signif(fisher.test(SAM_data$Group, SAM_data$sex)[[1]], digits=2),
              signif(fisher.test(SAM_data$Group, SAM_data$hivtest)[[1]], digits=2),
              signif(fisher.test(SAM_data$Group, SAM_data$kwash)[[1]], digits=2),
              signif(coef(summary(glm(SAM_data$Group~SAM_data$AgeMonths_Cal,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Group~SAM_data$haz_adm,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Group~SAM_data$waz_adm,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Group~SAM_data$whz_adm,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Group~SAM_data$muac,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_data$Group~SAM_data$timeToStab,family=binomial)))[2,4], digits=2),
              rep("-",2))



split_perGroup<- split(SAM_data[SAM_data$kwash == "no",], f=SAM_data$Alive_1Yes.2No[SAM_data$kwash == "no"])

Tbl_2[,6]<- c(paste0("Severe wasting, survived ", "n=",nrow(split_perGroup[[1]])),
              paste0(tabyl(split_perGroup[[1]]$sex)[2,c(2)]," (", round(100*tabyl(split_perGroup[[1]]$sex)[2,c(3)],digits=0),")"),
              paste0(tabyl(split_perGroup[[1]]$hivtest)[2,c(2)]," (", round(100*tabyl(split_perGroup[[1]]$hivtest)[2,c(4)],digits=0),")"),
              rep("-",1),
              paste0(round(mean(split_perGroup[[1]]$AgeMonths_Cal, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$AgeMonths_Cal, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$haz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$haz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$waz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$waz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$whz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$whz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$muac, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$muac, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$timeToStab, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$timeToStab, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$dur_adm[split_perGroup[[1]]$Alive_1Yes.2No == 1], na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$dur_adm[split_perGroup[[1]]$Alive_1Yes.2No == 1], na.rm=T), digits=1)),
              rep("-",1))


Tbl_2[,7]<- c(paste0("Severe wasting, Died ", "n=",nrow(split_perGroup[[2]])),
              paste0(tabyl(split_perGroup[[2]]$sex)[2,c(2)]," (", round(100*tabyl(split_perGroup[[2]]$sex)[2,c(3)],digits=0),")"),
              paste0(tabyl(split_perGroup[[2]]$hivtest)[2,c(2)]," (", round(100*tabyl(split_perGroup[[2]]$hivtest)[2,c(4)],digits=0),")"),
              rep("-",1),
              paste0(round(mean(split_perGroup[[2]]$AgeMonths_Cal, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$AgeMonths_Cal, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$haz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$haz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$waz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$waz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$whz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$whz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$muac, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$muac, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$timeToStab, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$timeToStab, na.rm=T), digits=1)),
              rep("-",1),
              paste0(round(mean(split_perGroup[[2]]$dur_adm[split_perGroup[[2]]$Alive_1Yes.2No == 2], na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$dur_adm[split_perGroup[[2]]$Alive_1Yes.2No == 2], na.rm=T), digits=1)))


Tbl_2[,8]<- c("p", rep("-",11))


SAM_data$Alive_1Yes.2No<-as.factor(SAM_data$Alive_1Yes.2No)
SAM_w<-SAM_data[SAM_data$kwash == "no",]

Tbl_2[,8]<- c("p", signif(fisher.test(SAM_w$Alive_1Yes.2No, SAM_w$sex)[[1]], digits=2),
              signif(fisher.test(SAM_w$Alive_1Yes.2No, SAM_w$hivtest)[[1]], digits=2),
              rep("-",1),
              signif(coef(summary(glm(SAM_w$Alive_1Yes.2No~SAM_w$AgeMonths_Cal,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_w$Alive_1Yes.2No~SAM_w$haz_adm,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_w$Alive_1Yes.2No~SAM_w$waz_adm,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_w$Alive_1Yes.2No~SAM_w$whz_adm,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_w$Alive_1Yes.2No~SAM_w$muac,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_w$Alive_1Yes.2No~SAM_w$timeToStab,family=binomial)))[2,4], digits=2),
              rep("-",2))






split_perGroup<- split(SAM_data[SAM_data$kwash == "yes",], f=SAM_data$Alive_1Yes.2No[SAM_data$kwash == "yes"])

Tbl_2[,9]<- c(paste0("Edematous, survived ", "n=",nrow(split_perGroup[[1]])),
              paste0(tabyl(split_perGroup[[1]]$sex)[2,c(2)]," (", round(100*tabyl(split_perGroup[[1]]$sex)[2,c(3)],digits=0),")"),
              paste0(tabyl(split_perGroup[[1]]$hivtest)[2,c(2)]," (", round(100*tabyl(split_perGroup[[1]]$hivtest)[2,c(3)],digits=0),")"),
              rep("-",1),
              paste0(round(mean(split_perGroup[[1]]$AgeMonths_Cal, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$AgeMonths_Cal, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$haz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$haz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$waz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$waz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$whz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$whz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$muac, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$muac, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$timeToStab, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$timeToStab, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[1]]$dur_adm[split_perGroup[[1]]$Alive_1Yes.2No == 1], na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$dur_adm[split_perGroup[[1]]$Alive_1Yes.2No == 1], na.rm=T), digits=1)),
              rep("-",1))




Tbl_2[,10]<- c(paste0("Edematous, died ", "n=",nrow(split_perGroup[[2]])),
              paste0(tabyl(split_perGroup[[2]]$sex)[2,c(2)]," (", round(100*tabyl(split_perGroup[[2]]$sex)[2,c(3)],digits=0),")"),
              paste0(tabyl(split_perGroup[[2]]$hivtest)[2,c(2)]," (", round(100*tabyl(split_perGroup[[2]]$hivtest)[2,c(4)],digits=0),")"),
              rep("-",1),
              paste0(round(mean(split_perGroup[[2]]$AgeMonths_Cal, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[1]]$AgeMonths_Cal, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$haz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$haz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$waz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$waz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$whz_adm, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$whz_adm, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$muac, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$muac, na.rm=T), digits=1)),
              paste0(round(mean(split_perGroup[[2]]$timeToStab, na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$timeToStab, na.rm=T), digits=1)),
              rep("-",1),
              paste0(round(mean(split_perGroup[[2]]$dur_adm[split_perGroup[[2]]$Alive_1Yes.2No == 2], na.rm=T),digits=1), " ± ", round(sd(split_perGroup[[2]]$dur_adm[split_perGroup[[2]]$Alive_1Yes.2No == 2], na.rm=T), digits=1)))

Tbl_2[,11]<- c("p", rep("-",11))



SAM_e<-SAM_data[SAM_data$kwash == "yes",]


Tbl_2[,11]<- c("p", signif(fisher.test(SAM_e$Alive_1Yes.2No, SAM_e$sex)[[1]], digits=2),
              signif(fisher.test(SAM_e$Alive_1Yes.2No, SAM_e$hivtest)[[1]], digits=2),
              rep("-",1),
              signif(coef(summary(glm(SAM_e$Alive_1Yes.2No~SAM_e$AgeMonths_Cal,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_e$Alive_1Yes.2No~SAM_e$haz_adm,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_e$Alive_1Yes.2No~SAM_e$waz_adm,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_e$Alive_1Yes.2No~SAM_e$whz_adm,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_e$Alive_1Yes.2No~SAM_e$muac,family=binomial)))[2,4], digits=2),
              signif(coef(summary(glm(SAM_e$Alive_1Yes.2No~SAM_e$timeToStab,family=binomial)))[2,4], digits=2),
              rep("-",2))


Tbl_2


write.csv(Tbl_2, file=paste0(Sys.Date(),"_SuplmTable2_Participant_Characteristics_bySurvival.csv"))



