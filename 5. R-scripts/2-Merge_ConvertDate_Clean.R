# BIA data
# Read in clinical data, clean and merge

library(dplyr)
library(here)
library(lubridate)
library(mgsub)


#read data
dataset <- read.csv(here("3. Data","All.BIA.data_v16.csv"), row.names = 1)
colnames(dataset)


#### read in clinical data for controls
clin_cp <- read.csv(here("3. Data","Raw_data","CP_Clinical_Data till 2018.csv"))

dput(colnames(clin_cp))
### select data of interest for community BIA
clin_cp<-clin_cp[ , c("subjid", "redcap_event_name", "adm_date", 
                    "adm_sex",
                    "adm_dob", "adm_agemons", "adm_agedays", 
                    "adm_weight", "adm_muac1", "adm_muac2", 
                    "adm_height1", "adm_height2", "adm_oedema",
                    "adm_curr_bfeeding","adm_hivrdt_now")]
head(clin_cp)

str(clin_cp)
## calculate variables
clin_cp$muac<-rowMeans(cbind(clin_cp$adm_muac1,clin_cp$adm_muac2))
clin_cp$height<-rowMeans(cbind(clin_cp$adm_height1,clin_cp$adm_height2))

## select final variables
dput(colnames(clin_cp))
clin_cp<-clin_cp[ , c("subjid", "redcap_event_name", "adm_date", 
                      "adm_sex",
                      "adm_dob", "adm_agemons","adm_agedays",
                      "adm_weight",  
                       "adm_oedema",
                      "adm_curr_bfeeding","adm_hivrdt_now","muac","height")]


### rename columns
colnames(clin_cp)<- c("subjid", "Participant", "date_Admission", 
                      "sex",
                      "dob", "AgeMonths_Cal","AgeAdm_days_cal",
                      "weight",  
                       "oedema",
                      "bfeeding","hiv_results","muac","height_cm")

### convert sex variables to match
dataset$sex
clin_cp$sex
clin_cp$sex<-ifelse(clin_cp$sex == 1, "M", "F")

### convert edema variables to match
dataset$oedema
clin_cp$oedema
clin_cp$oedema<-ifelse(clin_cp$oedema == 0, "None", NA)

### convert BF variables to match
dataset$bfeeding
clin_cp$bfeeding
clin_cp$bfeeding<-ifelse(clin_cp$bfeeding == 0, "No", "Yes")

### convert BF variables to match
dataset$Participant
clin_cp$Participant
clin_cp$Participant<-ifelse(clin_cp$Participant == "participant_enrolm_arm_1", "community_particip_arm_1", NA)


dataset$subjid<-as.character(dataset$subjid)
clin_cp$subjid<-as.character(clin_cp$subjid)

unique(clin_cp$subjid)
clin_merge<-full_join(dataset, clin_cp, by=c("subjid", "Participant", "date_Admission", 
                                             "sex",
                                             "dob", "AgeMonths_Cal","AgeAdm_days_cal",
                                             "weight",  
                                             "oedema",
                                             "bfeeding","hiv_results","muac","height_cm"))
head(clin_merge)
colnames(clin_merge)



#### read in BIA data
BIA_cp <- read.csv(here("3. Data","Raw_data","community participants bia data till 2018.csv"))
dput(colnames(BIA_cp))

### add missing columns
colnames(clin_merge)
clin_merge$Participant

### select data of interest for community BIA
BIA_cp<-BIA_cp[ , c("subjid", "redcap_event_name", "bia_tstdate", 
  "bia_tsttime",
  "bia_rohm1", "bia_xcohm1", "bia_pha_1", 
  "bia_rohm2", "bia_xcohm2", "bia_pha2", 
  "bia_rohm3", "bia_xcohm3", "bia_pha3"
)]
head(BIA_cp)

### rename columns
colnames(BIA_cp)<- c("subjid", "Participant", "date_of_test2", "time_of_test2",
                     "r_ohm_1A", "xc_ohm_1A", "ph_degrees_1A",
                     "r_ohm_1B", "xc_ohm_1B", "ph_degrees_1B", 
                     "r_ohm_1C", "xc_ohm_1C", "ph_degrees_1C")


BIA_cp$subjid<-as.character(BIA_cp$subjid)


BIA_merge<-full_join(clin_merge, BIA_cp, by=c("subjid","Participant","date_of_test2", "time_of_test2",
                                       "r_ohm_1A", "xc_ohm_1A", "ph_degrees_1A",
                                       "r_ohm_1B", "xc_ohm_1B", "ph_degrees_1B", 
                                       "r_ohm_1C", "xc_ohm_1C", "ph_degrees_1C"))
BIA_merge


length(unique(BIA_merge$subjid))

write.csv(BIA_merge, file=paste0(Sys.Date(),"_All.BIA.data_v17.csv"))

### this file was manually finalised due to some mismatched columns




##########################
### Converting dates
BIA_merge<-read.csv(file=here("3. Data","2020-05-18_All.BIA.data_v17.csv"), row.names = 1)

head(BIA_merge)
## make list of dates to convert
list_dates<-c("date_Admission","dob","date_enroll","date_stool","date_plasma","day3_date_FollowUp","date_stable_FollowUp","date_stable_FollowUp",
              "last_date_FollowUp","date_of_test2")

BIA_merge<-mutate_at(BIA_merge,list_dates, ymd)


## calculating age
BIA_merge$age.days<-difftime(BIA_merge$date_enroll,BIA_merge$dob, units="days")
BIA_merge$age.weeks<-difftime(BIA_merge$date_enroll,BIA_merge$dob, units="weeks")
BIA_merge$age.months.cal<-BIA_merge$age.days/30.417

## cleaning HIV 
BIA_merge$hiv_results
BIA_merge$hiv_results<-mgsub(BIA_merge$hiv_results, c("1","0","9"),c("yes","no",NA))
BIA_merge$hiv_results

BIA_merge$kwash
BIA_merge$kwash<-mgsub(BIA_merge$kwash, c("1","0"),c("yes","no"))
BIA_merge$kwash


########   Add variable death before of after test2
BIA_merge$Death.before.after.test2<-ifelse(!is.na(BIA_merge$R.H_2) & BIA_merge$Alive == "no", "after", "before")
BIA_merge$Death.before.after.test2<-as.factor(ifelse(BIA_merge$Alive == "yes", NA, BIA_merge$Death.before.after.test2))
BIA_merge$Death.before.after.test2
summary(BIA_merge$Death.before.after.test2)


#### calculate age for each time point
colnames(BIA_merge)
colnames(BIA_merge)[grep("date",colnames(BIA_merge))]

##age at test-2
BIA_merge$AgeAdm_days_cal_test2<-difftime(as.Date(BIA_merge$date_of_test2),as.Date(BIA_merge$dob),"days")
BIA_merge$AgeAdm_months_cal_test2<-round(BIA_merge$AgeAdm_days_cal_test2/30.438, digits=1)

#### add edema status at Test2
BIA_merge$oedemaTest2_Y_N<-ifelse(BIA_merge$oedema1_FollowUp=="None",0,1)

### discharge, because I am not sure if last_date_FollowUp is discharge, i will leave it


##########################
write.csv(BIA_merge, file=paste0(Sys.Date(),"_All.BIA.data_v17.csv"))


