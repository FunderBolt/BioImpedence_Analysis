# BIA data calculate and add the extra variables
#i.e mean and SD for variables
library(dplyr)
library(here)
library(matrixStats)
library(stringr)
library(goeveg)

### read in dataset
dataset <- read.csv(here("3-Data","2020-05-20_All.BIA.data_v19.csv"), row.names = 1)
colnames(dataset)

dput(colnames(dataset))
idata<-dataset %>% select("r_ohm_1A","r_ohm_1B","r_ohm_1C","r_ohm_1D","r_ohm_1E","r_ohm_1F",
                          "xc_ohm_1A","xc_ohm_1B","xc_ohm_1C","xc_ohm_1D","xc_ohm_1E","xc_ohm_1F",
                          "ph_degrees_1A", "ph_degrees_1B", "ph_degrees_1C","ph_degrees_1D",
                          "ph_degrees_1E","ph_degrees_1F",
                          "r_ohm_2A","r_ohm_2B","r_ohm_2C","r_ohm_2D","r_ohm_2E","r_ohm_2F",
                          "xc_ohm_2A", "xc_ohm_2B","xc_ohm_2C" ,"xc_ohm_2D","xc_ohm_2E","xc_ohm_2F" ,
                          "ph_degrees_2A","ph_degrees_2B","ph_degrees_2C","ph_degrees_2D" ,
                          "ph_degrees_2E", "ph_degrees_2F") %>% as.matrix()
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
## adding changing columns names
colnames(iidata)<- c("index",paste0("mean_",list_vars[[1]][1]), paste0("sd_",list_vars[[1]][1]),
     paste0("mean_",list_vars[[2]][1]), paste0("sd_",list_vars[[2]][1]),
     paste0("mean_",list_vars[[3]][1]), paste0("sd_",list_vars[[3]][1]),
     paste0("mean_",list_vars[[4]][1]), paste0("sd_",list_vars[[4]][1]),
     paste0("mean_",list_vars[[5]][1]), paste0("sd_",list_vars[[5]][1]),
     paste0("mean_",list_vars[[6]][1]), paste0("sd_",list_vars[[6]][1]))
head(iidata)
     
colnames(iidata)<- gsub("[A]","", colnames(iidata))

### rounding 
iidata <- iidata %>% mutate_at(vars(starts_with("mean_r_ohm")), funs(round(., 0))) %>%
 mutate_at(vars(starts_with("sd_r_ohm")), funs(round(., 1))) %>%
mutate_at(vars(starts_with("mean_xc_ohm")), funs(round(., 1))) %>%
mutate_at(vars(starts_with("sd_xc_ohm")), funs(round(., 2))) %>%
mutate_at(vars(starts_with("mean_ph_degrees")), funs(round(., 2))) %>%
mutate_at(vars(starts_with("sd_ph_degrees")), funs(round(., 3)))

## check behavior
iidata

#append to dataset
dataset<-cbind(dataset, iidata[,-1])



############################
#####  Calculating the indexes
colnames(dataset)

# now will calculate the R1/H and R2/H
dataset$R.H_1<-dataset$mean_r_ohm_1/dataset$height_m
dataset$R.H_2<-dataset$mean_r_ohm_2/dataset$height_m

dataset$Xc.H_1<-dataset$mean_xc_ohm_1/dataset$height_m
dataset$Xc.H_2<-dataset$mean_xc_ohm_2/dataset$height_m

# diff 
dataset$dR.H.2.1.<-dataset$R.H_2-dataset$R.H_1
dataset$dXc.H.2.1.<-dataset$Xc.H_2-dataset$Xc.H_1
#dataset$dR.H.2.1.


### adding CVs
colnames(dataset)
idata<-dataset[,152:163]
head(idata)
### calculate mean and sd for resistance, reactance and phase angle
list_vars<-list(c("mean_r_ohm_1","sd_r_ohm_1"), c("mean_xc_ohm_1","sd_xc_ohm_1"),
                c("mean_ph_degrees_1","sd_ph_degrees_1"), c("mean_r_ohm_2","sd_r_ohm_2"),
                c("mean_xc_ohm_2","sd_xc_ohm_2"), c("mean_ph_degrees_2","sd_ph_degrees_2"))
list_vars

iidata<-data.frame(1:nrow(idata))
for(i in 1:6){
  cv_vars<- 100 * (idata[,list_vars[[i]]][2] /  idata[,list_vars[[i]]][1])
  iidata  <- cbind(iidata, cv_vars=cv_vars)
}
iidata


## adding changing columns names
colnames(iidata)<- gsub("sd","CV", colnames(iidata))
iidata<-round(iidata,digits=1)

#append to dataset
dataset<-cbind(dataset, iidata[,-1])


##########################
write.csv(dataset, file=paste0(Sys.Date(),"_All.BIA.data_v19.csv"))


