# Read new Stata files in R and convert to .csv

#install.packages("readstata13")
library(readstata13)
library(here)

#choose.files()
dat <- read.dta13(here("3. Data","Raw_data","Malawi.F75_STATA", "f75_baseline_Malawi.dta"))
head(dat)
write.csv(dat, file="f75_baseline_Malawi.csv")

choose.files()
dat <- read.dta13(here("3. Data","Raw_data","Malawi.F75_STATA","f75_baseline_zscores_Malawi.dta"))
head(dat)
write.csv(dat, file="f75_baseline_zscores_Malawi.csv")

choose.files()
dat <- read.dta13(here("3. Data","Raw_data","Malawi.F75_STATA", "f75_daily_record_Malawi.dta"))
head(dat)
write.csv(dat, file="f75_daily_record_Malawi.csv")

choose.files()
dat <- read.dta13(here("3. Data","Raw_data","Malawi.F75_STATA", "f75_discharge_zscores_Malawi.dta"))
head(dat)
write.csv(dat, file="f75_discharge_zscores_Malawi.csv")

choose.files()
dat <- read.dta13(here("3. Data","Raw_data","Malawi.F75_STATA", "f75_followup_Malawi.dta"))
head(dat)
write.csv(dat, file="f75_followup_Malawi.csv")

choose.files()
dat <- read.dta13(here("3. Data","Raw_data","Malawi.F75_STATA", "f75_random_Malawi.dta"))
head(dat)
#write.csv(dat, file="f75_random_Malawi.csv")

choose.files()
dat <- read.dta13(here("3. Data","Raw_data","Malawi.F75_STATA", "f75_sae_Malawi.dta"))
head(dat)
write.csv(dat, file="f75_sae_Malawi.csv")
