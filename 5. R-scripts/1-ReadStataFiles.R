# Read new Stata files in R and convert to .csv

#install.packages("readstata13")
library(readstata13)
library(here)

#choose.files()
dat <- read.dta13(here("3. Data","Raw_data","Malawi.F75_STATA", "f75_baseline_Malawi.dta"))
head(dat)
write.csv(dat, file=paste0(Sys.Date(),"_F75_baseline_Malawi.csv"))


dat <- read.dta13(here("3. Data","Raw_data","Malawi.F75_STATA","f75_baseline_zscores_Malawi.dta"))
head(dat)
write.csv(dat, file=paste0(Sys.Date(),"_F75_baseline_zscores_Malawi.csv"))


dat <- read.dta13(here("3. Data","Raw_data","Malawi.F75_STATA", "f75_daily_record_Malawi.dta"))
head(dat)
write.csv(dat, file=paste0(Sys.Date(),"_F75_daily_record_Malawi.csv"))


dat <- read.dta13(here("3. Data","Raw_data","Malawi.F75_STATA", "f75_discharge_zscores_Malawi.dta"))
head(dat)
write.csv(dat, file=paste0(Sys.Date(),"_F75_discharge_zscores_Malawi.csv"))


dat <- read.dta13(here("3. Data","Raw_data","Malawi.F75_STATA", "f75_followup_Malawi.dta"))
head(dat)
write.csv(dat, file=paste0(Sys.Date(),"_F75_followup_Malawi.csv"))


dat <- read.dta13(here("3. Data","Raw_data","Malawi.F75_STATA", "f75_random_Malawi.dta"))
head(dat)
write.csv(dat, paste0(Sys.Date(),"_F75_random_Malawi.csv"))


dat <- read.dta13(here("3. Data","Raw_data","Malawi.F75_STATA", "f75_sae_Malawi.dta"))
head(dat)
write.csv(dat, paste0(Sys.Date(),"_F75_sae_Malawi.csv"))

