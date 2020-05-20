# BIA data
# Calculate WHO growth standard deviations and etc. for children from 6 to 60 months

library(dplyr)
library(here)
library(anthro)

### read in dataset
dataset <- read.csv(here("3-Data","2020-05-19_All.BIA.data_v18.csv"), row.names = 1)
colnames(dataset)


# Compute WHO Child Growth Standards (z-scores) ---------------------------
anthro_data <- with(

  dataset, 
  
  anthro_zscores(
    sex = sex,           # gender information (should be 1 = male, 2 = female)
    age = AgeAdm_days_cal,       # age in days (if you want in months, is_age_in_month = FALSE as argument)
    is_age_in_month = FALSE, # T/F
    weight = weight,     # weight in Kilogram
    lenhei = height_cm,  # length or height in cm
    measure= hmeasure.adm,  
    armc = muac        # MUAC in cm
  )
)

dataset <- bind_cols(dataset, anthro_data[,c("zlen","zwei","zwfl")]) 

rm(anthro_data)
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

##### at test 2 (i.e., follow up 1)
# Compute WHO Child Growth Standards (z-scores) ---------------------------
colnames(dataset)

anthro_data <- with(
  
  dataset,
  
  anthro_zscores(
    sex = sex,           # gender information (should be 1 = male, 2 = female)
    age = AgeAdm_days_cal_test2,       # age in days (if you want in months, is_age_in_month = FALSE as argument)
    is_age_in_month = FALSE, # T/F
    weight = weight1_FollowUp,     # weight in Kilogram
    lenhei = height_cm,  # length or height in cm
    measure= hmeasure.adm,  
    armc = muac1_FollowUp        # MUAC in cm
  )
)

dataset <- bind_cols(dataset, anthro_data[,c("zlen","zwei","zwfl")]) %>% 
  rename(zlen_T2 = zlen, zwei_T2 = zwei, zwfl_T2 = zwfl) # rename variables

rm(anthro_data)
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# Export extracted dataset for use within project with Rmarkdown
write.csv(dataset, file=here("3. Data", paste0(Sys.Date(),"_All.BIA.data_v18.csv")), row.names = F)




# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Compute WHO Child over than 5 years of age Growth Standards (z-scores) ---------------------------
#########
#### Children of 5 years old to 19  !
#choose.files()

wfawho2007<-read.table("C:\\Program Files\\R\\R-4.0.0\\library\\who2007_R\\wfawho2007.txt",header=T,sep="",skip=0)
hfawho2007<-read.table("C:\\Program Files\\R\\R-4.0.0\\library\\who2007_R\\hfawho2007.txt",header=T,sep="",skip=0)
bfawho2007<-read.table("C:\\Program Files\\R\\R-4.0.0\\library\\who2007_R\\bfawho2007.txt",header=T,sep="",skip=0) 

source("C:\\Program Files\\R\\R-4.0.0\\library\\who2007_R\\who2007.r")

#colnames(dataset)
idata<-dataset[,c("sex", "AgeAdm_months_cal_test2","weight1_FollowUp","height_cm")]
head(idata)

idata$sex
idata$sex<-ifelse(idata$sex=="M",1,2)

idata$measure<-rep("H",nrow(idata))
idata<-idata[which(idata$AgeAdm_months_cal_test2>60),]

#save file
write.csv(idata, file="D:\\Dropbox\\Bandsma.Lab\\1.Projects\\99-Archived_Projects\\2016_BIA\\3-Data\\BIA_for_Anthro.csv")
idata<-read.csv(file="D:\\Dropbox\\Bandsma.Lab\\1.Projects\\99-Archived_Projects\\2016_BIA\\3-Data\\BIA_for_Anthro.csv", row.names = 1,na.strings="NA")


who2007(FileLab="BIA_for_Anthro",
        FilePath="D:\\Dropbox\\Bandsma.Lab\\1.Projects\\99-Archived_Projects\\2016_BIA\\3-Data",
        mydf=idata, sex=sex, age=AgeAdm_months_cal_test2,measure=measure,
        weight=weight1_FollowUp,
        height=height_cm) 


