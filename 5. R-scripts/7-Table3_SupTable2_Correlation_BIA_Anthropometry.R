
######################
# Table 3 - BIA correlation test --- Antropometry and BIA variables
library(dplyr)
library(here)
library(janitor)


### read in dataset
dataset <- read.csv(here("3-Data","2020-05-20_All.BIA.data_v23.csv"), row.names = 1)
dataset$subid<-row.names(dataset)
colnames(dataset)

# set group.number as a factor
dataset$Participant<-as.factor(dataset$Participant)
dataset$Group<-as.factor(dataset$Group)
dput(colnames(dataset))


### exclusion of control
dataset<-dataset[!rownames(dataset) == "30001702",]




#select data of interest: ALL children controls included 
dataset<-dataset %>% select(c("subid","Participant","Group","AgeMonths_Cal","kwash",
                            "haz_adm", "waz_adm", "whz_adm", "muac",
                "AgeAdm_months_cal_test2","oedemaTest2_Y_N","zwfl_T2","zwei_T2","zlen_T2","muac1_FollowUp",
                "mean_r_ohm_1", "mean_xc_ohm_1",  "mean_ph_degrees_1",
                "mean_r_ohm_2", "mean_xc_ohm_2","mean_ph_degrees_2", "R.H_1", "R.H_2" ,"Xc.H_1",
                "Xc.H_2", "dR.H.2.1.", "dXc.H.2.1."))


#################### Table 1
Tbl_1<-data.frame(1:14)

Tbl_1[,1]<-c("Admission","","Resistance index, ohm/m","Weight-for-height, z-score", "Weight-for-age, z-score","MUAC, cm", 
                         "Reactance index, ohm/m","Weight-for-height, z-score", "Weight-for-age, z-score","MUAC, cm", 
                         "Phase angle, degree","Weight-for-height, z-score", "Weight-for-age, z-score","MUAC, cm")



### restrict to SAM data
idata <- dataset[dataset$Participant == "inhospital", ]


###########################################
#any differences in resistance on admission
cor.resistance <- apply(idata[, c("whz_adm","waz_adm","muac")], 2, cor.test, log(idata$mean_r_ohm_1), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_1[1:6,2]<-c("SAM Patients","","r  (95% CI)", 
             paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
             paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
             paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
             )

### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_1[1:6,3]<-c("","","df", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_1[1:6,4]<-c("","" , "R2",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_1[1:6,5]<-c("","" ,"p" ,signif(p_value,2))



#any differences in resistance on admission
cor.resistance <- apply(idata[, c("whz_adm","waz_adm","muac")], 2, cor.test, log(idata$mean_xc_ohm_1), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_1[7:10,2]<-c("",
                paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)

### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_1[7:10,3]<-c("", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_1[7:10,4]<-c("",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_1[7:10,5]<-c("" ,signif(p_value,2))



#any differences in resistance on admission
cor.resistance <- apply(idata[, c("whz_adm","waz_adm","muac")], 2, cor.test, log(idata$mean_ph_degrees_1), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_1[11:14,2]<-c("",
                 paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                 paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                 paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)


### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_1[11:14,3]<-c("", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_1[11:14,4]<-c("",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_1[11:14,5]<-c("" ,signif(p_value,2))


Tbl_1



### restrict to SAM data with severe wasting
idata <- dataset[dataset$Participant == "inhospital" & dataset$kwash == "no", ]


###########################################
#any differences in resistance on admission
cor.resistance <- apply(idata[, c("whz_adm","waz_adm","muac")], 2, cor.test, log(idata$mean_r_ohm_1), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_1[1:6,6]<-c("Severe Wasting","","r  (95% CI)", 
                paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)

### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_1[1:6,7]<-c("","","df", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_1[1:6,8]<-c("","" , "R2",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_1[1:6,9]<-c("","" ,"p" ,signif(p_value,2))



#any differences in resistance on admission
cor.resistance <- apply(idata[, c("whz_adm","waz_adm","muac")], 2, cor.test, log(idata$mean_xc_ohm_1), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_1[7:10,6]<-c("",
                 paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                 paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                 paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)

### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_1[7:10,7]<-c("", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_1[7:10,8]<-c("",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_1[7:10,9]<-c("" ,signif(p_value,2))



#any differences in resistance on admission
cor.resistance <- apply(idata[, c("whz_adm","waz_adm","muac")], 2, cor.test, log(idata$mean_ph_degrees_1), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_1[11:14,6]<-c("",
                  paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                  paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                  paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)


### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_1[11:14,7]<-c("", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_1[11:14,8]<-c("",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_1[11:14,9]<-c("" ,signif(p_value,2))


Tbl_1








### restrict to SAM data with severe wasting
idata <- dataset[dataset$Participant == "inhospital" & dataset$kwash == "yes", ]


###########################################
#any differences in resistance on admission
cor.resistance <- apply(idata[, c("whz_adm","waz_adm","muac")], 2, cor.test, log(idata$mean_r_ohm_1), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_1[1:6,10]<-c("Edematous","","r  (95% CI)", 
                paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)

### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_1[1:6,11]<-c("","","df", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_1[1:6,12]<-c("","" , "R2",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_1[1:6,13]<-c("","" ,"p" ,signif(p_value,2))



#any differences in resistance on admission
cor.resistance <- apply(idata[, c("whz_adm","waz_adm","muac")], 2, cor.test, log(idata$mean_xc_ohm_1), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_1[7:10,10]<-c("",
                 paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                 paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                 paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)

### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_1[7:10,11]<-c("", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_1[7:10,12]<-c("",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_1[7:10,13]<-c("" ,signif(p_value,2))



#any differences in resistance on admission
cor.resistance <- apply(idata[, c("whz_adm","waz_adm","muac")], 2, cor.test, log(idata$mean_ph_degrees_1), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_1[11:14,10]<-c("",
                  paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                  paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                  paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)


### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_1[11:14,11]<-c("", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_1[11:14,12]<-c("",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_1[11:14,13]<-c("" ,signif(p_value,2))


Tbl_1







### restrict to controls
idata <- dataset[!dataset$Participant == "inhospital", ]


###########################################
#any differences in resistance on admission
cor.resistance <- apply(idata[, c("whz_adm","waz_adm","muac")], 2, cor.test, log(idata$mean_r_ohm_1), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_1[1:6,14]<-c("Controls","","r  (95% CI)", 
                 paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                 paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                 paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)

### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_1[1:6,15]<-c("","","df", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_1[1:6,16]<-c("","" , "R2",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_1[1:6,17]<-c("","" ,"p" ,signif(p_value,2))



#any differences in resistance on admission
cor.resistance <- apply(idata[, c("whz_adm","waz_adm","muac")], 2, cor.test, log(idata$mean_xc_ohm_1), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_1[7:10,14]<-c("",
                  paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                  paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                  paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)

### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_1[7:10,15]<-c("", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_1[7:10,16]<-c("",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_1[7:10,17]<-c("" ,signif(p_value,2))



#any differences in resistance on admission
cor.resistance <- apply(idata[, c("whz_adm","waz_adm","muac")], 2, cor.test, log(idata$mean_ph_degrees_1), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_1[11:14,14]<-c("",
                   paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                   paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                   paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)


### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_1[11:14,15]<-c("", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_1[11:14,16]<-c("",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_1[11:14,17]<-c("" ,signif(p_value,2))


Tbl_1



write.csv(Tbl_1, file=paste0(Sys.Date(),"_Table3_correlations_BIA_Anthro_admission.csv"))












#####################################
### After Stabilization
#################### 




Tbl_2<-data.frame(1:14)

Tbl_2[,1]<-c("After Stabilization","","Resistance index, ohm/m","Weight-for-height, z-score", "Weight-for-age, z-score","MUAC, cm", 
             "Reactance index, ohm/m","Weight-for-height, z-score", "Weight-for-age, z-score","MUAC, cm", 
             "Phase angle, degree","Weight-for-height, z-score", "Weight-for-age, z-score","MUAC, cm")



### restrict to SAM data
idata <- dataset[dataset$Participant == "inhospital", ]

colnames(idata)
###########################################
#any differences in resistance
cor.resistance <- apply(idata[, c("zwfl_T2","zwei_T2","muac1_FollowUp")], 2, cor.test, log(idata$mean_r_ohm_2), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_2[1:6,2]<-c("SAM Patients","","r  (95% CI)", 
                paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)

### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_2[1:6,3]<-c("","","df", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_2[1:6,4]<-c("","" , "R2",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_2[1:6,5]<-c("","" ,"p" ,signif(p_value,2))



#any differences in resistance on admission
cor.resistance <- apply(idata[, c("zwfl_T2","zwei_T2","muac1_FollowUp")], 2, cor.test, log(idata$mean_xc_ohm_2), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_2[7:10,2]<-c("",
                 paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                 paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                 paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)

### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_2[7:10,3]<-c("", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_2[7:10,4]<-c("",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_2[7:10,5]<-c("" ,signif(p_value,2))


#any differences in resistance 
cor.resistance <- apply(idata[, c("zwfl_T2","zwei_T2","muac1_FollowUp")], 2, cor.test, log(idata$mean_ph_degrees_2), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_2[11:14,2]<-c("",
                  paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                  paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                  paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)


### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_2[11:14,3]<-c("", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_2[11:14,4]<-c("",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_2[11:14,5]<-c("" ,signif(p_value,2))


Tbl_2





### restrict to SAM data with severe wasting
idata <- dataset[dataset$Participant == "inhospital" & dataset$kwash == "no", ]


###########################################
#any differences in resistance 
cor.resistance <- apply(idata[, c("zwfl_T2","zwei_T2","muac1_FollowUp")], 2, cor.test, log(idata$mean_r_ohm_2), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_2[1:6,6]<-c("Severe Wasting","","r  (95% CI)", 
                paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)

### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_2[1:6,7]<-c("","","df", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_2[1:6,8]<-c("","" , "R2",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_2[1:6,9]<-c("","" ,"p" ,signif(p_value,2))



#any differences in resistance on admission
cor.resistance <- apply(idata[, c("zwfl_T2","zwei_T2","muac1_FollowUp")], 2, cor.test, log(idata$mean_xc_ohm_2), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_2[7:10,6]<-c("",
                 paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                 paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                 paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)

### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_2[7:10,7]<-c("", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_2[7:10,8]<-c("",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_2[7:10,9]<-c("" ,signif(p_value,2))



#any differences in resistance on admission
cor.resistance <- apply(idata[, c("zwfl_T2","zwei_T2","muac1_FollowUp")], 2, cor.test, log(idata$mean_ph_degrees_2), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_2[11:14,6]<-c("",
                  paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                  paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                  paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)


### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_2[11:14,7]<-c("", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_2[11:14,8]<-c("",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_2[11:14,9]<-c("" ,signif(p_value,2))


Tbl_2





### restrict to SAM data with severe wasting
idata <- dataset[dataset$Participant == "inhospital" & dataset$kwash == "yes", ]


###########################################
#any differences in resistance on admission
cor.resistance <- apply(idata[, c("zwfl_T2","zwei_T2","muac1_FollowUp")], 2, cor.test, log(idata$mean_r_ohm_2), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_2[1:6,10]<-c("Edematous","","r  (95% CI)", 
                 paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                 paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                 paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)

### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_2[1:6,11]<-c("","","df", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_2[1:6,12]<-c("","" , "R2",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_2[1:6,13]<-c("","" ,"p" ,signif(p_value,2))



#any differences in resistance on admission
cor.resistance <- apply(idata[, c("zwfl_T2","zwei_T2","muac1_FollowUp")], 2, cor.test, log(idata$mean_xc_ohm_2), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_2[7:10,10]<-c("",
                  paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                  paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                  paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)

### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_2[7:10,11]<-c("", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_2[7:10,12]<-c("",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_2[7:10,13]<-c("" ,signif(p_value,2))



#any differences in resistance on admission
cor.resistance <- apply(idata[, c("zwfl_T2","zwei_T2","muac1_FollowUp")], 2, cor.test, log(idata$mean_ph_degrees_2), method="pearson",conf.level = 0.95)
estimate<-sapply(cor.resistance, "[[", "estimate")
estimate<- round(estimate,2)
confint<-sapply(cor.resistance, "[[", "conf.int")
confint<-round(confint,2)

Tbl_2[11:14,10]<-c("",
                   paste0(estimate[1], " (", confint[1,1],",", confint[2,1], ")"),
                   paste0(estimate[2], " (", confint[1,2],",", confint[2,2], ")"),
                   paste0(estimate[3], " (", confint[1,3],",", confint[2,3], ")")
)


### df
df_value<-sapply(cor.resistance, "[[", "parameter")
Tbl_2[11:14,11]<-c("", df_value)
### R2
r_square<-sapply(cor.resistance, "[[", "estimate")^2
Tbl_2[11:14,12]<-c("",round(r_square,2))
### p
p_value<-sapply(cor.resistance, "[[", "p.value")
Tbl_2[11:14,13]<-c("" ,signif(p_value,2))


Tbl_2



write.csv(Tbl_2, file=paste0(Sys.Date(),"_SuplTable2_correlations_BIA_Anthro_AfterStabilization.csv"))


