# BIA data calculate and add the extra variables
#i.e mean and SD for variables
library(dplyr)
library(here)
library(matrixStats)
library(stringr)

### read in dataset
dataset <- read.csv(here("3-Data","2020-05-19_All.BIA.data_v18.csv"), row.names = 1)
colnames(dataset)

dput(colnames(dataset))
idata<-dataset %>% select("r_ohm_1A","r_ohm_1B","r_ohm_1C","r_ohm_1D","r_ohm_1E","r_ohm_1F",
                          "xc_ohm_1A","xc_ohm_1B","xc_ohm_1C","xc_ohm_1D","xc_ohm_1E","xc_ohm_1F",
                          "ph_degrees_1A", "ph_degrees_1B", "ph_degrees_1C","ph_degrees_1D","ph_degrees_1E","ph_degrees_1F",
                          "r_ohm_2A","r_ohm_2B","r_ohm_2C","r_ohm_2D","r_ohm_2E","r_ohm_2F",
                          "xc_ohm_2A", "xc_ohm_2B","xc_ohm_2C" ,"xc_ohm_2D","xc_ohm_2E","xc_ohm_2F" ,
                          "ph_degrees_2A","ph_degrees_2B","ph_degrees_2C","ph_degrees_2D" ,"ph_degrees_2E", "ph_degrees_2F",
                          "mean_r_ohm_1", "r_ohm_1_sd","mean_xc_ohm_1" ,"xc_ohm_1_sd","mean_ph_degrees_1",
                          "ph_degrees_1_sd" ,"mean_r_ohm_2","r_ohm_2_sd","mean_xc_ohm_2","xc_ohm_2_sd" ,
                          "mean_ph_degrees_2" ,"ph_degrees_2_sd","r_ohm_1_CV", "xc_ohm_1_CV", "ph_1_CV", "r_ohm_2_CV", 
                          "xc_ohm_2_CV", "ph_2_CV") %>% as.matrix()
str(idata)


### calculate mean and sd for resistance, reactance and phase angle
list_vars<-list(colnames(idata[,c(1:6)]), colnames(idata[,c(7:12)]),
                colnames(idata[,c(13:18)]), colnames(idata[,c(19:24)]),
                colnames(idata[,c(25:30)]), colnames(idata[,c(31:36)]))
list_vars

iidata<-data.frame(1:nrow(idata))
for(i in 1:6){
  mean_vars<-rowMeans(idata[,list_vars[[i]]], na.rm=T)
  sd_vars<- rowSds(idata[,list_vars[[i]]], na.rm=T)
  iidata  <- cbind(iidata, mean_vars=mean_vars)
  iidata  <- cbind(iidata, sd_vars=sd_vars)
}

head(iidata)
colnames(iidata)<- c("index",paste0("mean_",list_vars[[1]][1]), paste0("sd_",list_vars[[1]][1]),
     paste0("mean_",list_vars[[2]][1]), paste0("sd_",list_vars[[2]][1]),
     paste0("mean_",list_vars[[3]][1]), paste0("sd_",list_vars[[3]][1]),
     paste0("mean_",list_vars[[4]][1]), paste0("sd_",list_vars[[4]][1]),
     paste0("mean_",list_vars[[5]][1]), paste0("sd_",list_vars[[5]][1]),
     paste0("mean_",list_vars[[6]][1]), paste0("sd_",list_vars[[6]][1]))
head(iidata)
     
## remove the [A]     
colnames(iidata)<-gsub("[A]","",colnames(iidata))
  
### rounding 
iidata <- iidata %>% mutate_at(vars(starts_with("mean_r_ohm")), funs(round(., 0))) %>%
 mutate_at(vars(starts_with("sd_r_ohm")), funs(round(., 1))) %>%
mutate_at(vars(starts_with("mean_xc_ohm")), funs(round(., 1))) %>%
mutate_at(vars(starts_with("sd_xc_ohm")), funs(round(., 2))) %>%
mutate_at(vars(starts_with("mean_ph_degrees")), funs(round(., 2))) %>%
mutate_at(vars(starts_with("sd_ph_degrees")), funs(round(., 3)))

iidata





#############################
####### Time point 2
colnames(idata)
r_ohm_2<-idata[,c(19:24)]
r_ohm_2

idata$mean_r_ohm_2<-round(rowMeans(r_ohm_2, na.rm=T), digit=0)
idata$mean_r_ohm_2

r_ohm_2_sd<-round(apply(r_ohm_2,1,sd,na.rm=T ),digit=1)

idata$r_ohm_2_sd<-r_ohm_2_sd
idata$r_ohm_2_sd

idata

######
colnames(idata)
xc_ohm_2<-idata[,c(25:30)]
xc_ohm_2

idata$mean_xc_ohm_2<-round(rowMeans(xc_ohm_2, na.rm=T), digit=1)
idata$mean_xc_ohm_2

xc_ohm_2_sd<-round(apply(xc_ohm_2,1,sd,na.rm=T ),digit=1)

idata$xc_ohm_2_sd<-xc_ohm_2_sd
idata$xc_ohm_2_sd

idata



###################
colnames(idata)
ph_degrees_2<-idata[,c(31:36)]
head(ph_degrees_2)
idata$mean_ph_degrees_2<-round(rowMeans(ph_degrees_2, na.rm=T), digit=1)
idata$mean_ph_degrees_2

ph_degrees_2_sd<-round(apply(ph_degrees_2,1,sd,na.rm=T ),digit=1)
idata$ph_degrees_2_sd<-ph_degrees_2_sd
idata$ph_degrees_2_sd

idata



############################
#####
colnames(data)

# now will calculate the R1/H and R2/H
idata$R.H_1<-idata$mean_r_ohm_1/data$height_m
idata$R.H_2<-idata$mean_r_ohm_2/data$height_m

idata$Xc.H_1<-idata$mean_xc_ohm_1/data$height_m
idata$Xc.H_2<-idata$mean_xc_ohm_2/data$height_m

# diff 
idata$dR.H.2.1.<-idata$R.H_2-idata$R.H_1
idata$dXc.H.2.1.<-idata$Xc.H_2-idata$Xc.H_1

idata$dR.H.2.1.

write.csv(idata, file="All.BIA.data_temp.csv")







## calculate mean CV per measure
sapply(idata[ ,c("r_ohm_1_CV", "xc_ohm_1_CV", "r_ohm_2_CV", 
                "xc_ohm_2_CV")], mean, na.rm=T)

sapply(idata[ ,c("r_ohm_1_CV", "xc_ohm_1_CV",  "r_ohm_2_CV", 
                 "xc_ohm_2_CV")], range, na.rm=T)


CV_data<- idata %>% select("r_ohm_1_CV", "xc_ohm_1_CV", "ph_1_CV", "r_ohm_2_CV", 
                           "xc_ohm_2_CV", "ph_2_CV")



c(round(dim(CV_data[which(CV_data$r_ohm_1_CV > 5),])[1],0), round(100*(dim(CV_data[which(CV_data$r_ohm_1_CV > 5),])[1])/(dim(CV_data[!is.na(CV_data$r_ohm_1_CV),])[1]),1),
dim(CV_data[which(CV_data$xc_ohm_1_CV > 5),])[1], round((100*dim(CV_data[which(CV_data$xc_ohm_1_CV > 5),])[1]) /(dim(CV_data[!is.na(CV_data$xc_ohm_1_CV),])[1]),1),
dim(CV_data[which(CV_data$r_ohm_2_CV > 5),])[1], round((100*dim(CV_data[which(CV_data$r_ohm_2_CV > 5),])[1]) /(dim(CV_data[!is.na(CV_data$r_ohm_2_CV),])[1]),1),
dim(CV_data[which(CV_data$xc_ohm_2_CV > 5),])[1], round(100*(dim(CV_data[which(CV_data$xc_ohm_2_CV > 5),])[1]) /(dim(CV_data[!is.na(CV_data$xc_ohm_2_CV),])[1]),1) )



# this is to clean the data based on standard deviation of up to 5 measurements
par(mfrow=c(1,2))
hist(idata$r_ohm_1_sd)
hist(idata$xc_ohm_1_sd)
hist(idata$ph_degrees_1_sd)

hist(idata$r_ohm_2_sd)
hist(idata$xc_ohm_2_sd)
hist(idata$ph_degrees_2_sd)



####
par(mfrow=c(1,3))

all.r_ohm<-c(idata$r_ohm_1_sd,idata$r_ohm_2_sd)
boxplot(all.r_ohm)
mean(all.r_ohm,na.rm=T)
sd(all.r_ohm,na.rm=T)
###calculate the standard deviation cut off for resistance
mean(all.r_ohm,na.rm=T)+3*(sd(all.r_ohm,na.rm=T))

####
all.xc_ohm<-c(idata$xc_ohm_1_sd,idata$xc_ohm_2_sd)
boxplot(all.xc_ohm)

mean(all.xc_ohm,na.rm=T)
sd(all.xc_ohm,na.rm=T)

###calculate the standard deviation cut off for reactance
mean(all.xc_ohm,na.rm=T)+3*(sd(all.xc_ohm,na.rm=T))


####
all.ph_degrees<-c(idata$ph_degrees_1_sd,idata$ph_degrees_2_sd)
boxplot(all.ph_degrees)
mean(all.ph_degrees,na.rm=T)
sd(all.ph_degrees,na.rm=T)
###calculate the standard deviation cut off for phase angle
mean(all.ph_degrees,na.rm=T)+3*(sd(all.ph_degrees,na.rm=T))

colnames(idata)



