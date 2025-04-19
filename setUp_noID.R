
#--------------------------------------------------------------------------#
# Acute air pollution and DFNC                                             #
# Author: Monica Lopez                                                     #
# Date: 30/9/2024                                                          #
# Description: setup script                                                #
# Changes (programmer/date):                                               #
#--------------------------------------------------------------------------#


#---- INDEX ----------------------------------------------------------------                      
# 1) Merge datasets
# 2) Recategorize variables
# 3) Select samples
# 4) Transformation outcomes
#---------------------------------------------------------------------------

#R version 4.3.1

#cd
setwd("V:/medewerkers/529028 Lopez Vicente, M/AP")

#load packages
library(foreign)
library(ggplot2)
library(gridExtra)
library(bestNormalize)
library(splitstackshape)
library(openxlsx)

#load functions
source("analyses/final_acute/functions.R")

#load data

#air pollution childhood
airpollution.data<-read.spss("data/20230306_Monica_2.1.genr_ap_nobackextrap_MRIvisits_birth_13y.sav",  use.value.labels=T, max.value.labels=Inf,to.data.frame=TRUE)
airpollution.data<-airpollution.data[(airpollution.data$MRIvisit=="birth_MRI9 "),]
airpollution.data$nday_complete_ap<-airpollution.data$nday_complete
table(airpollution.data$nday_complete_ap<0.5)

#remove data from participants with less than 50% of days with exposure data
for (i in colnames(airpollution.data[6:26])){
airpollution.data[,i][airpollution.data$nday_complete_ap<0.5]<-NA
  }



#air pollution pregnancy
airpollutionpreg.data<-read.spss("data/20210831_Michelle-Airpollution_pregpost_birth_9y.sav",  use.value.labels=T, max.value.labels=Inf,to.data.frame=TRUE)
airpollutionpreg.data<-airpollutionpreg.data[c("IDC", "PM10_preg", "PM25_preg", "PM25coarse_preg","PM25abs_preg","NO2_preg", "NOX_preg","OP_dtt_preg",
                                               "OP_esr_preg","OC_preg","PAH_preg","PM25Cu_preg","PM25Fe_preg","PM25Si_preg","PM25Zn_preg")]


#fMRI data
load("V:/medewerkers/529028 Lopez Vicente, M/frontiers/dfnc_metrics_subjects_clean.RData")

#Covariates MRI
load("../sample selection fmri/mri_sample_all_new.RData")
selected_sample<-df[c("idc","agechildbrainmrif9", "agemri_f13","selectedf9","selectedf13", "ethninfv2" , "gender", "educm5", "income5", "mri_consent", "exclude_incidental", "has_rsfmri_nii_f9", "braces_mri_f9", "usable_fs_f9_final", "mean_rms9","rms_vols9","rms_vols_bin9","usemrif13",
"mean_rms13","rms_vols13","rms_vols_bin13", "exclude_braces", "if_exclude", "nvolsf9", "nvolsf13", "tsvdataf9", "tsvdataf13", "exclude_reg_f9", "exclude_reg_f13")]

#General covariates
whole.data <- read.spss("data/CHILD-ALLGENERALDATA_20042022.sav",  use.value.labels=T, max.value.labels=Inf,to.data.frame=TRUE)
whole.data<-whole.data[c("IDM","IDC", "OUTCOMECHILD","TWIN", "MULTIPLE","GENDER","BMI_0", "BMI_P", "AGE_M_v2",
                         "AGE_BF_v2","PARITY", "EDUCM", "EDUCP", "MARDICH", "ETHNMv2",
                         "ETHNFv2", "INCOME")]

#exclude IDs on request by participant
ids_to_exclude<-c(x,x,x)
whole.data<-whole.data[which(!whole.data$IDC%in%ids_to_exclude),]

#other covariates
birth.data<-read.spss("data/MONTHYEARBIRTHCHILD_19122018_string.sav",  use.value.labels=T, max.value.labels=Inf,to.data.frame=TRUE)

smoking.data<-read.spss("data/MATERNALSMOKING_22112016.sav",  use.value.labels=T, max.value.labels=Inf,to.data.frame=TRUE)
smoking.data<-smoking.data[c("idm", "SMOKE_ALL")]

alcohol.data<-read.spss("data/GEDRAGSGROEP_MaternalDrinking_22112016.sav",  use.value.labels=T, max.value.labels=Inf,to.data.frame=TRUE)
alcohol.data<-alcohol.data[c("IDM", "mdrink_updated")]

folic.data<-read.spss("data/MATERNALFOLICACID_23062010.sav",  use.value.labels=T, max.value.labels=Inf,to.data.frame=TRUE)

maternaliq.data<-read.spss("data/COGNITIONPARENT_28052013_APM_IBF_VDR_outliersexcluded.sav",  use.value.labels=T, max.value.labels=Inf,to.data.frame=TRUE)
maternaliq.data<-maternaliq.data[c("IDC", "APM_IQ")]

greenspace.data<-read.spss("data/GENRnaturalspaces_25112019.sav",  use.value.labels=T, max.value.labels=Inf,to.data.frame=TRUE)
greenspace.data<-greenspace.data[c("IDC","ndvi300_preg")]

status.data<-read.spss("data/Statusscores_voorMichelle.sav",  use.value.labels=T, max.value.labels=Inf,to.data.frame=TRUE)
status.data<-status.data[c("IDC","STATUSSCORE")]

maternalbsi.data<-read.spss("data/GR1003-BSI D1_22112016.sav",  use.value.labels=T, max.value.labels=Inf,to.data.frame=TRUE)
maternalbsi.data<-maternalbsi.data[c("idm", "gsi")]


#merge
temp1<-merge(whole.data, selected_sample, by.x = "IDC", by.y = "idc", all.x = T, sort = T)
temp2<-merge(temp1, airpollution.data, by = "IDC", all.x = T)
temp4<-merge(temp2, smoking.data, by.x = "IDM",by.y = "idm", all.x = T)
temp5<-merge(temp4, alcohol.data, by = "IDM", all.x = T)
temp6<-merge(temp5, folic.data, by = "IDM", all.x = T)
temp7<-merge(temp6, maternaliq.data, by = "IDC", all.x = T)
temp8<-merge(temp7, greenspace.data, by = "IDC", all.x = T)
temp9<-merge(temp8, status.data, by = "IDC", all.x = T)
temp10<-merge(temp9, birth.data, by = "IDC", all.x = T)
temp11<-merge(temp10, airpollutionpreg.data, by = "IDC", all.x = T)
dataset_all<-merge(temp11, maternalbsi.data, by.x = "IDM",by.y = "idm",  all.x = T)

#recategorize

dataset_all$educm_3cat <- NA
dataset_all$educm_3cat[as.numeric(dataset_all$EDUCM) < 3] <- 3
dataset_all$educm_3cat[as.numeric(dataset_all$EDUCM) >= 3 & as.numeric(dataset_all$EDUCM) < 5] <- 2
dataset_all$educm_3cat[as.numeric(dataset_all$EDUCM) >= 5] <- 1

dataset_all$educp_3cat <- NA
dataset_all$educp_3cat[as.numeric(dataset_all$EDUCP) < 3] <- 3
dataset_all$educp_3cat[as.numeric(dataset_all$EDUCP) >= 3 & as.numeric(dataset_all$EDUCP) < 5] <- 2
dataset_all$educp_3cat[as.numeric(dataset_all$EDUCP) >= 5] <- 1

dataset_all$educm_3cat <- factor(dataset_all$educm_3cat, levels=c(1, 2, 3), labels=c('higher', 'secondary', 'none or primary'))
dataset_all$educp_3cat <- factor(dataset_all$educp_3cat, levels=c(1, 2, 3), labels=c('higher', 'secondary', 'none or primary'))

dataset_all$ethnmv2_6cat <- NA
dataset_all$ethnmv2_6cat[as.numeric(dataset_all$ETHNMv2) ==1] <- 1
dataset_all$ethnmv2_6cat[as.numeric(dataset_all$ETHNMv2) ==4] <- 2
dataset_all$ethnmv2_6cat[as.numeric(dataset_all$ETHNMv2) ==6] <- 3
dataset_all$ethnmv2_6cat[as.numeric(dataset_all$ETHNMv2) ==7] <- 4
dataset_all$ethnmv2_6cat[as.numeric(dataset_all$ETHNMv2) ==13] <- 5
dataset_all$ethnmv2_6cat[as.numeric(dataset_all$ETHNMv2) ==2 | as.numeric(dataset_all$ETHNMv2) ==3 | as.numeric(dataset_all$ETHNMv2) ==5 | as.numeric(dataset_all$ETHNMv2) ==8 |
                           as.numeric(dataset_all$ETHNMv2) ==9 |as.numeric(dataset_all$ETHNMv2) ==10 | as.numeric(dataset_all$ETHNMv2) ==11 | as.numeric(dataset_all$ETHNMv2) ==12 |
                           as.numeric(dataset_all$ETHNMv2) ==14] <- 6

dataset_all$ethnmv2_6cat <- factor(dataset_all$ethnmv2_6cat, levels=c(1, 2, 3, 4, 5, 6), labels=c('Dutch', 'Moroccan', 'Surinamese', 'Turkish', 'other European', 'other non-European'))

dataset_all$ethnfv2_6cat <- NA
dataset_all$ethnfv2_6cat[as.numeric(dataset_all$ETHNFv2) ==1] <- 1
dataset_all$ethnfv2_6cat[as.numeric(dataset_all$ETHNFv2) ==4] <- 2
dataset_all$ethnfv2_6cat[as.numeric(dataset_all$ETHNFv2) ==6] <- 3
dataset_all$ethnfv2_6cat[as.numeric(dataset_all$ETHNFv2) ==7] <- 4
dataset_all$ethnfv2_6cat[as.numeric(dataset_all$ETHNFv2) ==13] <- 5
dataset_all$ethnfv2_6cat[as.numeric(dataset_all$ETHNFv2) ==2 | as.numeric(dataset_all$ETHNFv2) ==3 | as.numeric(dataset_all$ETHNFv2) ==5 | as.numeric(dataset_all$ETHNFv2) ==8 |
                           as.numeric(dataset_all$ETHNFv2) ==9 |as.numeric(dataset_all$ETHNFv2) ==10 | as.numeric(dataset_all$ETHNFv2) ==11 | as.numeric(dataset_all$ETHNFv2) ==12 |
                           as.numeric(dataset_all$ETHNFv2) ==14] <- 6

dataset_all$ethnfv2_6cat <- factor(dataset_all$ethnfv2_6cat, levels=c(1, 2, 3, 4, 5, 6), labels=c('Dutch', 'Moroccan', 'Surinamese', 'Turkish', 'other European', 'other non-European'))


dataset_all$income_4cat <- NA
dataset_all$income_4cat[as.numeric(dataset_all$INCOME) ==12] <- 1
dataset_all$income_4cat[as.numeric(dataset_all$INCOME) ==9 |as.numeric(dataset_all$INCOME) ==10 | as.numeric(dataset_all$INCOME) ==11 ] <- 2
dataset_all$income_4cat[as.numeric(dataset_all$INCOME) ==6 |as.numeric(dataset_all$INCOME) ==7 | as.numeric(dataset_all$INCOME) ==8 ] <- 3
dataset_all$income_4cat[as.numeric(dataset_all$INCOME) ==1 |as.numeric(dataset_all$INCOME) ==2 | as.numeric(dataset_all$INCOME) ==3 |
                          as.numeric(dataset_all$INCOME) ==4 |as.numeric(dataset_all$INCOME) ==5  ] <- 4

dataset_all$income_4cat <- factor(dataset_all$income_4cat, levels=c(1, 2, 3, 4), labels=c('> 2200 euro', '1600-2200 euro', '900-1600 euro', '<900 euro'))

dataset_all$parity_3cat <- NA
dataset_all$parity_3cat[dataset_all$PARITY==0] <- 1
dataset_all$parity_3cat[dataset_all$PARITY==1] <- 2
dataset_all$parity_3cat[dataset_all$PARITY>1] <- 3

dataset_all$parity_3cat <- factor(dataset_all$parity_3cat, levels=c(1, 2, 3), labels=c('nulliparous', '1 child', '2+ children'))

#birth 
table(dataset_all$mybirthChild)
dataset_all<-cSplit(dataset_all, "mybirthChild", sep=" ")
dataset_all$month<-as.factor(dataset_all$mybirthChild_1)
dataset_all<-as.data.frame(dataset_all)

dataset_all$season <- NA
dataset_all$season[dataset_all$month=="DEC" | dataset_all$month=="JAN" |dataset_all$month=="FEB" ] <- 1
dataset_all$season[dataset_all$month=="MAR" | dataset_all$month=="APR" |dataset_all$month=="MAY" ] <- 2
dataset_all$season[dataset_all$month=="JUN" | dataset_all$month=="JUL" |dataset_all$month=="AUG" ] <- 3
dataset_all$season[dataset_all$month=="SEP" | dataset_all$month=="OCT" |dataset_all$month=="NOV" ] <- 4
dataset_all$season <- factor(dataset_all$season, levels=c(1, 2, 3, 4), labels=c('winter', 'spring', 'summer', 'autumn'))


#agemri: convert to NA for excluded subjects 
dataset_all$agechildbrainmrif9[is.na(dataset_all$selectedf9)] <- NA
dataset_all$agemri_f13[is.na(dataset_all$selectedf13)] <- NA


#load lag data
airpollutionlagsdatum.data<-read.spss("data/20230306_Monica_2.2.genr_ap_backextrap_MRIlags_birth_13y.sav",  use.value.labels=T, max.value.labels=Inf,to.data.frame=TRUE)
airpollutionlags.data<-readRDS("data/20240124_Monica_genr_ap_backextrap_MRIlags_birth_13y.rds")
airpollutionlagsschools.data<-readRDS("data/20240722_Monica_genr_school_ap_LUR_MRI_7dday_lag.rds")

pss2date <- function(x) as.Date(x/86400, origin = "1582-10-14")
airpollutionlagsdatum.data$data.F09<-pss2date(airpollutionlagsdatum.data$datum_MRI9_1)
airpollutionlagsdatum.data$data.F13<-pss2date(airpollutionlagsdatum.data$datumMRI13)

airpollutionlagsdatum.data<-airpollutionlagsdatum.data[(airpollutionlagsdatum.data$MRIlag=="MRI9_Lag0 " | airpollutionlagsdatum.data$MRIlag=="MRI13_Lag0"),]
airpollutionlagsdatum.data<-airpollutionlagsdatum.data[c("IDC", "data.F09", "data.F13", "MRIlag")]
colnames(airpollutionlagsdatum.data)[which(colnames(airpollutionlagsdatum.data) %in% c("IDC", "data.F09", "data.F13","MRIlag") )] <- c("idc","data.F09", "data.F13","ses")

airpollutionlagsdatum.data$ses[airpollutionlagsdatum.data$ses=="MRI9_Lag0 "]<-"F09"
airpollutionlagsdatum.data$ses[airpollutionlagsdatum.data$ses=="MRI13_Lag0"]<-"F13"


#I have to order the dataset first, otherwise the cbind doesn't match well
airpollutionlags.data<-airpollutionlags.data[order(airpollutionlags.data$MRIlag),]


airpollutionlags0.data<-airpollutionlags.data[(airpollutionlags.data$MRIlag=="MRI9_lag0" | airpollutionlags.data$MRIlag=="MRI13_lag0"),]
airpollutionlags0.data<-airpollutionlags0.data[c("IDC", "no2_be","nox_be","pm25_be","pm25abs_be","pm10_be","pmcoarse_be","MRIlag")]
colnames(airpollutionlags0.data)[which(colnames(airpollutionlags0.data) %in% c("no2_be","nox_be","pm25_be","pm25abs_be","pm10_be","pmcoarse_be") )] <- c("no2_Lag0","nox_Lag0","pm25_Lag0","pm25abs_Lag0","pm10_Lag0","pmcoarse_Lag0")

airpollutionlags1.data<-airpollutionlags.data[(airpollutionlags.data$MRIlag=="MRI9_lag1" | airpollutionlags.data$MRIlag=="MRI13_lag1"),]
airpollutionlags1.data<-airpollutionlags1.data[c("no2_be","nox_be","pm25_be","pm25abs_be","pm10_be","pmcoarse_be")]
colnames(airpollutionlags1.data)[which(colnames(airpollutionlags1.data) %in% c("no2_be","nox_be","pm25_be","pm25abs_be","pm10_be","pmcoarse_be") )] <- c("no2_Lag1","nox_Lag1","pm25_Lag1","pm25abs_Lag1","pm10_Lag1","pmcoarse_Lag1")

airpollutionlags2.data<-airpollutionlags.data[(airpollutionlags.data$MRIlag=="MRI9_lag2" | airpollutionlags.data$MRIlag=="MRI13_lag2"),]
airpollutionlags2.data<-airpollutionlags2.data[c("no2_be","nox_be","pm25_be","pm25abs_be","pm10_be","pmcoarse_be")]
colnames(airpollutionlags2.data)[which(colnames(airpollutionlags2.data) %in% c("no2_be","nox_be","pm25_be","pm25abs_be","pm10_be","pmcoarse_be") )] <- c("no2_Lag2","nox_Lag2","pm25_Lag2","pm25abs_Lag2","pm10_Lag2","pmcoarse_Lag2")

airpollutionlags3.data<-airpollutionlags.data[(airpollutionlags.data$MRIlag=="MRI9_lag3" | airpollutionlags.data$MRIlag=="MRI13_lag3"),]
airpollutionlags3.data<-airpollutionlags3.data[c("no2_be","nox_be","pm25_be","pm25abs_be","pm10_be","pmcoarse_be")]
colnames(airpollutionlags3.data)[which(colnames(airpollutionlags3.data) %in% c("no2_be","nox_be","pm25_be","pm25abs_be","pm10_be","pmcoarse_be") )] <- c("no2_Lag3","nox_Lag3","pm25_Lag3","pm25abs_Lag3","pm10_Lag3","pmcoarse_Lag3")

airpollutionlags4.data<-airpollutionlags.data[(airpollutionlags.data$MRIlag=="MRI9_lag4" | airpollutionlags.data$MRIlag=="MRI13_lag4"),]
airpollutionlags4.data<-airpollutionlags4.data[c("no2_be","nox_be","pm25_be","pm25abs_be","pm10_be","pmcoarse_be")]
colnames(airpollutionlags4.data)[which(colnames(airpollutionlags4.data) %in% c("no2_be","nox_be","pm25_be","pm25abs_be","pm10_be","pmcoarse_be") )] <- c("no2_Lag4","nox_Lag4","pm25_Lag4","pm25abs_Lag4","pm10_Lag4","pmcoarse_Lag4")

airpollutionlags5.data<-airpollutionlags.data[(airpollutionlags.data$MRIlag=="MRI9_lag5" | airpollutionlags.data$MRIlag=="MRI13_lag5"),]
airpollutionlags5.data<-airpollutionlags5.data[c("no2_be","nox_be","pm25_be","pm25abs_be","pm10_be","pmcoarse_be")]
colnames(airpollutionlags5.data)[which(colnames(airpollutionlags5.data) %in% c("no2_be","nox_be","pm25_be","pm25abs_be","pm10_be","pmcoarse_be") )] <- c("no2_Lag5","nox_Lag5","pm25_Lag5","pm25abs_Lag5","pm10_Lag5","pmcoarse_Lag5")

airpollutionlags6.data<-airpollutionlags.data[(airpollutionlags.data$MRIlag=="MRI9_lag6" | airpollutionlags.data$MRIlag=="MRI13_lag6"),]
airpollutionlags6.data<-airpollutionlags6.data[c("no2_be","nox_be","pm25_be","pm25abs_be","pm10_be","pmcoarse_be")]
colnames(airpollutionlags6.data)[which(colnames(airpollutionlags6.data) %in% c("no2_be","nox_be","pm25_be","pm25abs_be","pm10_be","pmcoarse_be") )] <- c("no2_Lag6","nox_Lag6","pm25_Lag6","pm25abs_Lag6","pm10_Lag6","pmcoarse_Lag6")

airpollutionlags7.data<-airpollutionlags.data[(airpollutionlags.data$MRIlag=="MRI9_lag7" | airpollutionlags.data$MRIlag=="MRI13_lag7"),]
airpollutionlags7.data<-airpollutionlags7.data[c("no2_be","nox_be","pm25_be","pm25abs_be","pm10_be","pmcoarse_be")]
colnames(airpollutionlags7.data)[which(colnames(airpollutionlags7.data) %in% c("no2_be","nox_be","pm25_be","pm25abs_be","pm10_be","pmcoarse_be") )] <- c("no2_Lag7","nox_Lag7","pm25_Lag7","pm25abs_Lag7","pm10_Lag7","pmcoarse_Lag7")

airpollutionlagsall.data<-cbind(airpollutionlags0.data,airpollutionlags1.data,airpollutionlags2.data,airpollutionlags3.data,airpollutionlags4.data,airpollutionlags5.data,airpollutionlags6.data,airpollutionlags7.data)

colnames(airpollutionlagsall.data)[which(colnames(airpollutionlagsall.data) %in% c("IDC","MRIlag") )] <- c("idc","ses")


airpollutionlagsall.data$ses[airpollutionlagsall.data$ses=="MRI9_lag0"]<-"F09"
airpollutionlagsall.data$ses[airpollutionlagsall.data$ses=="MRI13_lag0"]<-"F13"

#calculate mean all lags
airpollutionlagsall.data$no2_week<-rowMeans(airpollutionlagsall.data[c("no2_Lag0", "no2_Lag1","no2_Lag2", "no2_Lag3","no2_Lag4", "no2_Lag5","no2_Lag6", "no2_Lag7")], na.rm=T)
airpollutionlagsall.data$nox_week<-rowMeans(airpollutionlagsall.data[c("nox_Lag0", "nox_Lag1","nox_Lag2", "nox_Lag3","nox_Lag4", "nox_Lag5","nox_Lag6", "nox_Lag7")], na.rm=T)
airpollutionlagsall.data$pm25_week<-rowMeans(airpollutionlagsall.data[c("pm25_Lag0", "pm25_Lag1","pm25_Lag2", "pm25_Lag3","pm25_Lag4", "pm25_Lag5","pm25_Lag6", "pm25_Lag7")], na.rm=T)
airpollutionlagsall.data$pm25abs_week<-rowMeans(airpollutionlagsall.data[c("pm25abs_Lag0", "pm25abs_Lag1","pm25abs_Lag2", "pm25abs_Lag3","pm25abs_Lag4", "pm25abs_Lag5","pm25abs_Lag6", "pm25abs_Lag7")], na.rm=T)
airpollutionlagsall.data$pm10_week<-rowMeans(airpollutionlagsall.data[c("pm10_Lag0", "pm10_Lag1","pm10_Lag2", "pm10_Lag3","pm10_Lag4", "pm10_Lag5","pm10_Lag6", "pm10_Lag7")], na.rm=T)
airpollutionlagsall.data$pmcoarse_week<-rowMeans(airpollutionlagsall.data[c("pmcoarse_Lag0", "pmcoarse_Lag1","pmcoarse_Lag2", "pmcoarse_Lag3","pmcoarse_Lag4", "pmcoarse_Lag5","pmcoarse_Lag6", "pmcoarse_Lag7")], na.rm=T)

##SCHOOLS

#I have to order the dataset first, otherwise the cbind doesn't match well
airpollutionlagsschools.data<-airpollutionlagsschools.data[order(airpollutionlagsschools.data$Lag),]
airpollutionlagsschools.data<-airpollutionlagsschools.data[order(airpollutionlagsschools.data$visit_date_label),]

airpollutionlagsschools0.data<-airpollutionlagsschools.data[((airpollutionlagsschools.data$visit_date_label=="datum_MRI9_1" & airpollutionlagsschools.data$Lag=="0 days") | (airpollutionlagsschools.data$visit_date_label=="datumMRI13" & airpollutionlagsschools.data$Lag=="0 days")),]
airpollutionlagsschools0.data<-airpollutionlagsschools0.data[c("IDC", "be_no2","be_nox","be_pm25","be_pm10","be_pmcoarse","visit_date_label")]
colnames(airpollutionlagsschools0.data)[which(colnames(airpollutionlagsschools0.data) %in% c("be_no2","be_nox","be_pm25","be_pm10","be_pmcoarse") )] <- c("no2_Lag0","nox_Lag0","pm25_Lag0","pm10_Lag0","pmcoarse_Lag0")

airpollutionlagsschools1.data<-airpollutionlagsschools.data[((airpollutionlagsschools.data$visit_date_label=="datum_MRI9_1" & airpollutionlagsschools.data$Lag=="1 days") | (airpollutionlagsschools.data$visit_date_label=="datumMRI13" & airpollutionlagsschools.data$Lag=="1 days")),]
airpollutionlagsschools1.data<-airpollutionlagsschools1.data[c("IDC", "be_no2","be_nox","be_pm25","be_pm10","be_pmcoarse","visit_date_label")]
colnames(airpollutionlagsschools1.data)[which(colnames(airpollutionlagsschools1.data) %in% c("be_no2","be_nox","be_pm25","be_pm10","be_pmcoarse") )] <- c("no2_Lag1","nox_Lag1","pm25_Lag1","pm10_Lag1","pmcoarse_Lag1")

airpollutionlagsschools2.data<-airpollutionlagsschools.data[((airpollutionlagsschools.data$visit_date_label=="datum_MRI9_1" & airpollutionlagsschools.data$Lag=="2 days") | (airpollutionlagsschools.data$visit_date_label=="datumMRI13" & airpollutionlagsschools.data$Lag=="2 days")),]
airpollutionlagsschools2.data<-airpollutionlagsschools2.data[c("IDC", "be_no2","be_nox","be_pm25","be_pm10","be_pmcoarse","visit_date_label")]
colnames(airpollutionlagsschools2.data)[which(colnames(airpollutionlagsschools2.data) %in% c("be_no2","be_nox","be_pm25","be_pm10","be_pmcoarse") )] <- c("no2_Lag2","nox_Lag2","pm25_Lag2","pm10_Lag2","pmcoarse_Lag2")

airpollutionlagsschools3.data<-airpollutionlagsschools.data[((airpollutionlagsschools.data$visit_date_label=="datum_MRI9_1" & airpollutionlagsschools.data$Lag=="3 days") | (airpollutionlagsschools.data$visit_date_label=="datumMRI13" & airpollutionlagsschools.data$Lag=="3 days")),]
airpollutionlagsschools3.data<-airpollutionlagsschools3.data[c("IDC", "be_no2","be_nox","be_pm25","be_pm10","be_pmcoarse","visit_date_label")]
colnames(airpollutionlagsschools3.data)[which(colnames(airpollutionlagsschools3.data) %in% c("be_no2","be_nox","be_pm25","be_pm10","be_pmcoarse") )] <- c("no2_Lag3","nox_Lag3","pm25_Lag3","pm10_Lag3","pmcoarse_Lag3")

airpollutionlagsschools4.data<-airpollutionlagsschools.data[((airpollutionlagsschools.data$visit_date_label=="datum_MRI9_1" & airpollutionlagsschools.data$Lag=="4 days") | (airpollutionlagsschools.data$visit_date_label=="datumMRI13" & airpollutionlagsschools.data$Lag=="4 days")),]
airpollutionlagsschools4.data<-airpollutionlagsschools4.data[c("IDC", "be_no2","be_nox","be_pm25","be_pm10","be_pmcoarse","visit_date_label")]
colnames(airpollutionlagsschools4.data)[which(colnames(airpollutionlagsschools4.data) %in% c("be_no2","be_nox","be_pm25","be_pm10","be_pmcoarse") )] <- c("no2_Lag4","nox_Lag4","pm25_Lag4","pm10_Lag4","pmcoarse_Lag4")

airpollutionlagsschools5.data<-airpollutionlagsschools.data[((airpollutionlagsschools.data$visit_date_label=="datum_MRI9_1" & airpollutionlagsschools.data$Lag=="5 days") | (airpollutionlagsschools.data$visit_date_label=="datumMRI13" & airpollutionlagsschools.data$Lag=="5 days")),]
airpollutionlagsschools5.data<-airpollutionlagsschools5.data[c("IDC", "be_no2","be_nox","be_pm25","be_pm10","be_pmcoarse","visit_date_label")]
colnames(airpollutionlagsschools5.data)[which(colnames(airpollutionlagsschools5.data) %in% c("be_no2","be_nox","be_pm25","be_pm10","be_pmcoarse") )] <- c("no2_Lag5","nox_Lag5","pm25_Lag5","pm10_Lag5","pmcoarse_Lag5")

airpollutionlagsschools6.data<-airpollutionlagsschools.data[((airpollutionlagsschools.data$visit_date_label=="datum_MRI9_1" & airpollutionlagsschools.data$Lag=="6 days") | (airpollutionlagsschools.data$visit_date_label=="datumMRI13" & airpollutionlagsschools.data$Lag=="6 days")),]
airpollutionlagsschools6.data<-airpollutionlagsschools6.data[c("IDC", "be_no2","be_nox","be_pm25","be_pm10","be_pmcoarse","visit_date_label")]
colnames(airpollutionlagsschools6.data)[which(colnames(airpollutionlagsschools6.data) %in% c("be_no2","be_nox","be_pm25","be_pm10","be_pmcoarse") )] <- c("no2_Lag6","nox_Lag6","pm25_Lag6","pm10_Lag6","pmcoarse_Lag6")

airpollutionlagsschools7.data<-airpollutionlagsschools.data[((airpollutionlagsschools.data$visit_date_label=="datum_MRI9_1" & airpollutionlagsschools.data$Lag=="7 days") | (airpollutionlagsschools.data$visit_date_label=="datumMRI13" & airpollutionlagsschools.data$Lag=="7 days")),]
airpollutionlagsschools7.data<-airpollutionlagsschools7.data[c("IDC", "be_no2","be_nox","be_pm25","be_pm10","be_pmcoarse","visit_date_label")]
colnames(airpollutionlagsschools7.data)[which(colnames(airpollutionlagsschools7.data) %in% c("be_no2","be_nox","be_pm25","be_pm10","be_pmcoarse") )] <- c("no2_Lag7","nox_Lag7","pm25_Lag7","pm10_Lag7","pmcoarse_Lag7")

airpollutionlagsschoolsall1.data<-merge(airpollutionlagsschools0.data,airpollutionlagsschools1.data, by=c("IDC","visit_date_label"), all.x = T, all.y = T)
airpollutionlagsschoolsall2.data<-merge(airpollutionlagsschoolsall1.data,airpollutionlagsschools2.data, by=c("IDC","visit_date_label"), all.x = T, all.y = T)
airpollutionlagsschoolsall3.data<-merge(airpollutionlagsschoolsall2.data,airpollutionlagsschools3.data, by=c("IDC","visit_date_label"), all.x = T, all.y = T)
airpollutionlagsschoolsall4.data<-merge(airpollutionlagsschoolsall3.data,airpollutionlagsschools4.data, by=c("IDC","visit_date_label"), all.x = T, all.y = T)
airpollutionlagsschoolsall5.data<-merge(airpollutionlagsschoolsall4.data,airpollutionlagsschools5.data, by=c("IDC","visit_date_label"), all.x = T, all.y = T)
airpollutionlagsschoolsall6.data<-merge(airpollutionlagsschoolsall5.data,airpollutionlagsschools6.data, by=c("IDC","visit_date_label"), all.x = T, all.y = T)
airpollutionlagsschoolsall.data<-merge(airpollutionlagsschoolsall6.data,airpollutionlagsschools7.data, by=c("IDC","visit_date_label"), all.x = T, all.y = T)


colnames(airpollutionlagsschoolsall.data)[which(colnames(airpollutionlagsschoolsall.data) %in% c("IDC","visit_date_label") )] <- c("idc","ses")


airpollutionlagsschoolsall.data$ses[airpollutionlagsschoolsall.data$ses=="datum_MRI9_1"]<-"F09"
airpollutionlagsschoolsall.data$ses[airpollutionlagsschoolsall.data$ses=="datumMRI13"]<-"F13"

#calculate mean all lags 
airpollutionlagsschoolsall.data$no2_week_schools<-rowMeans(airpollutionlagsschoolsall.data[c("no2_Lag0", "no2_Lag1","no2_Lag2", "no2_Lag3","no2_Lag4", "no2_Lag5","no2_Lag6", "no2_Lag7")], na.rm=T)
airpollutionlagsschoolsall.data$nox_week_schools<-rowMeans(airpollutionlagsschoolsall.data[c("nox_Lag0", "nox_Lag1","nox_Lag2", "nox_Lag3","nox_Lag4", "nox_Lag5","nox_Lag6", "nox_Lag7")], na.rm=T)
airpollutionlagsschoolsall.data$pm25_week_schools<-rowMeans(airpollutionlagsschoolsall.data[c("pm25_Lag0", "pm25_Lag1","pm25_Lag2", "pm25_Lag3","pm25_Lag4", "pm25_Lag5","pm25_Lag6", "pm25_Lag7")], na.rm=T)
airpollutionlagsschoolsall.data$pm10_week_schools<-rowMeans(airpollutionlagsschoolsall.data[c("pm10_Lag0", "pm10_Lag1","pm10_Lag2", "pm10_Lag3","pm10_Lag4", "pm10_Lag5","pm10_Lag6", "pm10_Lag7")], na.rm=T)
airpollutionlagsschoolsall.data$pmcoarse_week_schools<-rowMeans(airpollutionlagsschoolsall.data[c("pmcoarse_Lag0", "pmcoarse_Lag1","pmcoarse_Lag2", "pmcoarse_Lag3","pmcoarse_Lag4", "pmcoarse_Lag5","pmcoarse_Lag6", "pmcoarse_Lag7")], na.rm=T)

#keep only week variables
airpollutionlagsschoolsall.data<-airpollutionlagsschoolsall.data[c("idc","ses","no2_week_schools","nox_week_schools", "pm25_week_schools", "pm10_week_schools", "pmcoarse_week_schools")]

#remove those that are duplicated (different values for single lags due to problem on school data: overlap different schools)

nodup9<-airpollutionlagsschoolsall.data[airpollutionlagsschoolsall.data$ses=="F09" ,]
table(duplicated(nodup9$idc))
ids_dup_9<-nodup9$idc[duplicated(nodup9$idc)]
length(unique(ids_dup_9))

nodup9<-nodup9[which(!nodup9$idc%in%ids_dup_9),]


nodup13<-airpollutionlagsschoolsall.data[airpollutionlagsschoolsall.data$ses=="F13" ,]
table(duplicated(nodup13$idc))
ids_dup_13<-nodup13$idc[duplicated(nodup13$idc)]
length(unique(ids_dup_13))

nodup13<-nodup13[which(!nodup13$idc%in%ids_dup_13),]

airpollutionlagsschoolsall_clean.data<-rbind(nodup9,nodup13)


#load time of the day when the scan was conducted
mritimeF9.data<-read.spss("data/MRItime/20160817_UseDataMRIF9_MRItime.sav",  use.value.labels=T, max.value.labels=Inf,to.data.frame=TRUE)
mritimeF13.data<-read.spss("data/MRItime/20210225_UseMRIDataF13_v2_MRItime.sav",  use.value.labels=T, max.value.labels=Inf,to.data.frame=TRUE)

table(!is.na(mritimeF9.data$tijd_MRI9.1) & !is.na(mritimeF9.data$tijd_MRI9.2))
ids_2times<-mritimeF9.data$IDC[!is.na(mritimeF9.data$tijd_MRI9.1) & !is.na(mritimeF9.data$tijd_MRI9.2)]

mritimeF9.data$time.F09<-mritimeF9.data$tijd_MRI9.1/3600
mritimeF13.data$time.F13<-mritimeF13.data$tijdMRI/3600

mritimeF9.data<-mritimeF9.data[c("IDC", "time.F09")]
mritimeF13.data<-mritimeF13.data[c("IDC", "time.F13")]
mritime.data<-merge(mritimeF9.data, mritimeF13.data, by="IDC")

varying = c("time.F09","time.F13") 
idVar = 'IDC'
timeVar = 'ses'

mritime.data_long <- reshape(mritime.data, varying=varying, idvar=idVar, timevar=timeVar, direction='long')
colnames(mritime.data_long)[which(colnames(mritime.data_long) %in% c("IDC") )] <- c("idc")
mritime.data_long$time<-round(mritime.data_long$time, 0)
mritime.data_long$time_factor<-as.factor(mritime.data_long$time)


#load season and temperature data 
season.data<-readRDS("data/20230428_Monica_genr_temperature_MRIlags_post.rds")
season.data<-season.data[(season.data$MRIlag=="MRI9_Lag0" | season.data$MRIlag=="MRI13_Lag0"),]
season.data<-season.data[c("IDC", "season", "MRIlag")]
colnames(season.data)[which(colnames(season.data) %in% c("IDC", "season","MRIlag") )] <- c("idc","season_Lag0","ses")

season.data$ses[season.data$ses=="MRI9_Lag0"]<-"F09"
season.data$ses[season.data$ses=="MRI13_Lag0"]<-"F13"


#temperaturelags.data<-readRDS("data/20230717_Monica_temperature_MRIlags_post.rds")
temperaturelags.data<-readRDS("data/20240124_Monica_temperature_MRIlags_post.rds")

#I have to order the dataset first, otherwise the cbind doesn't match well
temperaturelags.data<-temperaturelags.data[order(temperaturelags.data$MRIlag),]

temperaturelags0.data<-temperaturelags.data[(temperaturelags.data$MRIlag=="MRI9_lag0" | temperaturelags.data$MRIlag=="MRI13_lag0"),]
temperaturelags0.data<-temperaturelags0.data[c("IDC", "TG","HU","MRIlag")]
colnames(temperaturelags0.data)[which(colnames(temperaturelags0.data) %in% c("TG","HU") )] <- c("TG_Lag0","HU_Lag0")

temperaturelags1.data<-temperaturelags.data[(temperaturelags.data$MRIlag=="MRI9_lag1" | temperaturelags.data$MRIlag=="MRI13_lag1"),]
temperaturelags1.data<-temperaturelags1.data[c( "TG","HU")]
colnames(temperaturelags1.data)[which(colnames(temperaturelags1.data) %in% c("TG","HU") )] <- c("TG_Lag1","HU_Lag1")

temperaturelags2.data<-temperaturelags.data[(temperaturelags.data$MRIlag=="MRI9_lag2" | temperaturelags.data$MRIlag=="MRI13_lag2"),]
temperaturelags2.data<-temperaturelags2.data[c("TG","HU")]
colnames(temperaturelags2.data)[which(colnames(temperaturelags2.data) %in% c("TG","HU") )] <- c("TG_Lag2","HU_Lag2")

temperaturelags3.data<-temperaturelags.data[(temperaturelags.data$MRIlag=="MRI9_lag3" | temperaturelags.data$MRIlag=="MRI13_lag3"),]
temperaturelags3.data<-temperaturelags3.data[c("TG","HU")]
colnames(temperaturelags3.data)[which(colnames(temperaturelags3.data) %in% c("TG","HU") )] <- c("TG_Lag3","HU_Lag3")

temperaturelags4.data<-temperaturelags.data[(temperaturelags.data$MRIlag=="MRI9_lag4" | temperaturelags.data$MRIlag=="MRI13_lag4"),]
temperaturelags4.data<-temperaturelags4.data[c( "TG","HU")]
colnames(temperaturelags4.data)[which(colnames(temperaturelags4.data) %in% c("TG","HU") )] <- c("TG_Lag4","HU_Lag4")

temperaturelags5.data<-temperaturelags.data[(temperaturelags.data$MRIlag=="MRI9_lag5" | temperaturelags.data$MRIlag=="MRI13_lag5"),]
temperaturelags5.data<-temperaturelags5.data[c("TG","HU")]
colnames(temperaturelags5.data)[which(colnames(temperaturelags5.data) %in% c("TG","HU") )] <- c("TG_Lag5","HU_Lag5")

temperaturelags6.data<-temperaturelags.data[(temperaturelags.data$MRIlag=="MRI9_lag6" | temperaturelags.data$MRIlag=="MRI13_lag6"),]
temperaturelags6.data<-temperaturelags6.data[c( "TG","HU")]
colnames(temperaturelags6.data)[which(colnames(temperaturelags6.data) %in% c("TG","HU") )] <- c("TG_Lag6","HU_Lag6")

temperaturelags7.data<-temperaturelags.data[(temperaturelags.data$MRIlag=="MRI9_lag7" | temperaturelags.data$MRIlag=="MRI13_lag7"),]
temperaturelags7.data<-temperaturelags7.data[c("TG","HU")]
colnames(temperaturelags7.data)[which(colnames(temperaturelags7.data) %in% c("TG","HU") )] <- c("TG_Lag7","HU_Lag7")

temperaturelagsall.data<-cbind(temperaturelags0.data,temperaturelags1.data,temperaturelags2.data,temperaturelags3.data,temperaturelags4.data,temperaturelags5.data,temperaturelags6.data,temperaturelags7.data)

colnames(temperaturelagsall.data)[which(colnames(temperaturelagsall.data) %in% c("IDC","MRIlag") )] <- c("idc","ses")


temperaturelagsall.data$ses[temperaturelagsall.data$ses=="MRI9_lag0"]<-"F09"
temperaturelagsall.data$ses[temperaturelagsall.data$ses=="MRI13_lag0"]<-"F13"

# calculate mean all lags

temperaturelagsall.data$TG_week<-rowMeans(temperaturelagsall.data[c("TG_Lag0", "TG_Lag1","TG_Lag2", "TG_Lag3","TG_Lag4", "TG_Lag5","TG_Lag6", "TG_Lag7")], na.rm=T)
temperaturelagsall.data$HU_week<-rowMeans(temperaturelagsall.data[c("HU_Lag0", "HU_Lag1","HU_Lag2", "HU_Lag3","HU_Lag4", "HU_Lag5","HU_Lag6", "HU_Lag7")], na.rm=T)


#merge
temp13<-merge(airpollutionlagsall.data, mritime.data_long, by = c("idc", "ses"), all.x = T, sort = T)
temp14<-merge(temp13, season.data, by = c("idc", "ses"), all.x = T, sort = T)
temp15<-merge(temp14, airpollutionlagsdatum.data, by = c("idc", "ses"), all.x = T, sort = T)
temp16<-merge(temp15, airpollutionlagsschoolsall_clean.data, by = c("idc", "ses"), all.x = T, sort = T)
dataset_lags<-merge(temp16,temperaturelagsall.data,  by = c("idc", "ses"), all.x = T, sort = T)

#check the merge is ok
dataset_lags$no2_Lag0[dataset_lags$idc=="1002" & dataset_lags$ses=="F09"]
dataset_lags$no2_Lag0[dataset_lags$idc=="1002" & dataset_lags$ses=="F13"]
airpollutionlags.data$no2[airpollutionlags.data$IDC=="1002" & airpollutionlags.data$MRIlag=="MRI9_lag0"]
airpollutionlags.data$no2[airpollutionlags.data$IDC=="1002" & airpollutionlags.data$MRIlag=="MRI13_lag0"]

dataset_lags$no2_Lag2[dataset_lags$idc=="1002" & dataset_lags$ses=="F09"]
dataset_lags$no2_Lag2[dataset_lags$idc=="1002" & dataset_lags$ses=="F13"]
airpollutionlags.data$no2[airpollutionlags.data$IDC=="1002" & airpollutionlags.data$MRIlag=="MRI9_lag2"]
airpollutionlags.data$no2[airpollutionlags.data$IDC=="1002" & airpollutionlags.data$MRIlag=="MRI13_lag2"]

dataset_lags$time_factor[dataset_lags$idc=="1002"& dataset_lags$ses=="F09"]
dataset_lags$time_factor[dataset_lags$idc=="1002"& dataset_lags$ses=="F13"]
mritimeF9.data$time.F09[mritimeF9.data$IDC=="1002"]
mritimeF13.data$time.F13[mritimeF13.data$IDC=="1002"]

dataset_lags$season_Lag0[dataset_lags$idc=="1002" & dataset_lags$ses=="F09"]
season.data$season_Lag0[season.data$idc=="1002" & season.data$ses=="F09"]

dataset_lags$TG_Lag0[dataset_lags$idc=="1002" & dataset_lags$ses=="F09"]
dataset_lags$TG_Lag0[dataset_lags$idc=="1002" & dataset_lags$ses=="F13"]
dataset_lags$TG_Lag2[dataset_lags$idc=="1002" & dataset_lags$ses=="F09"]
dataset_lags$TG_Lag2[dataset_lags$idc=="1002" & dataset_lags$ses=="F13"]

temperaturelags.data$TG[temperaturelags.data$IDC=="1002" & temperaturelags.data$MRIlag=="MRI9_lag0"]
temperaturelags.data$TG[temperaturelags.data$IDC=="1002" & temperaturelags.data$MRIlag=="MRI13_lag0"]
temperaturelags.data$TG[temperaturelags.data$IDC=="1002" & temperaturelags.data$MRIlag=="MRI9_lag2"]
temperaturelags.data$TG[temperaturelags.data$IDC=="1002" & temperaturelags.data$MRIlag=="MRI13_lag2"]

#check home vs school data
table(is.na(dataset_lags$no2_week))
table(is.na(dataset_lags$no2_week_schools))
dataset_lags$idc[is.na(dataset_lags$no2_week_schools) & !is.na(dataset_lags$no2_week) & dataset_lags$ses=="F09"]
dataset_lags$idc[is.na(dataset_lags$no2_week_schools) & !is.na(dataset_lags$no2_week) & dataset_lags$ses=="F13"]

airpollutionlagsschools.data$be_no2[airpollutionlagsschools.data$IDC=="7"]
airpollutionlagsschoolsall_clean.data$no2_week_schools[airpollutionlagsschoolsall_clean.data$idc=="7"]
dataset_lags$no2_week_schools[dataset_lags$idc=="7"]
dataset_lags$no2_week[dataset_lags$idc=="7"]


#calculate day of the week


dataset_lags$weekdayF09<-lubridate::wday(dataset_lags$data.F09, label=TRUE, abbr=FALSE)
dataset_lags$weekdayF13<-lubridate::wday(dataset_lags$data.F13, label=TRUE, abbr=FALSE)
dataset_lags$weekday<-as.factor(ifelse(dataset_lags$ses=="F09", dataset_lags$weekdayF09, dataset_lags$weekdayF13))
table(is.na(dataset_lags$weekdayF09))
table(is.na(dataset_lags$weekdayF13))
table(is.na(dataset_lags$weekday)[dataset_lags$ses=="F09"])
table(is.na(dataset_lags$weekday)[dataset_lags$ses=="F13"])

#season of visit to factor
dataset_lags$season_Lag0<-as.factor(dataset_lags$season_Lag0)


#fix dataset
# remove those with potential wrong AP data


ids_2time_remove<-c(x, x, x, x)



vars<-c("no2_Lag0","nox_Lag0","pm25_Lag0","pm25abs_Lag0","pm10_Lag0","pmcoarse_Lag0", "no2_Lag1","nox_Lag1","pm25_Lag1","pm25abs_Lag1","pm10_Lag1","pmcoarse_Lag1",
        "no2_Lag2","nox_Lag2","pm25_Lag2","pm25abs_Lag2","pm10_Lag2","pmcoarse_Lag2", "no2_Lag3","nox_Lag3","pm25_Lag3","pm25abs_Lag3","pm10_Lag3","pmcoarse_Lag3",
        "no2_Lag4","nox_Lag4","pm25_Lag4","pm25abs_Lag4","pm10_Lag4","pmcoarse_Lag4",
        "no2_Lag5","nox_Lag5","pm25_Lag5","pm25abs_Lag5","pm10_Lag5","pmcoarse_Lag5",
        "no2_Lag6","nox_Lag6","pm25_Lag6","pm25abs_Lag6","pm10_Lag6","pmcoarse_Lag6",
        "no2_Lag7","nox_Lag7","pm25_Lag7","pm25abs_Lag7","pm10_Lag7","pmcoarse_Lag7",
        "no2_week","nox_week","pm25_week","pm25abs_week","pm10_week","pmcoarse_week",
        "no2_week_schools","nox_week_schools","pm25_week_schools","pm10_week_schools","pmcoarse_week_schools",
        "weekday", "time_factor","TG_Lag0","HU_Lag0","season_Lag0",
        "TG_Lag1","HU_Lag1","TG_Lag2","HU_Lag2","TG_Lag3","HU_Lag3",
        "TG_Lag4","HU_Lag4","TG_Lag5","HU_Lag5","TG_Lag6","HU_Lag6","TG_Lag7","HU_Lag7", "TG_week", "HU_week")

for (v in vars){
  dataset_lags[,v][dataset_lags$idc%in%ids_2time_remove & dataset_lags$ses=="F09"]<-NA
}

head(dataset_lags[dataset_lags$idc=="9158",])
head(dataset_lags[dataset_lags$idc=="8942",])

# merge dataset_all with no2_week for each visit (wide format) to be able to make the exclusions



dataset_lag_week<-dataset_lags[c("idc", "no2_week", "no2_week_schools", "ses")]
dataset_lagw_wide<-reshape(dataset_lag_week, idvar = "idc", timevar = "ses", direction = "wide")
dataset_all_lag<-merge(dataset_all, dataset_lagw_wide, by.x = "IDC", by.y = "idc", all.x = T, sort = T)

#select sample 
#exclude twins
dataset_notwins<-dataset_all_lag[which(dataset_all_lag$TWIN=="No"),]

# create inclusion var

dataset_notwins$incl_var<-0
dataset_notwins$complete9[dataset_notwins$selectedf9==1 & !is.na(dataset_notwins$no2_week.F09)]<-1
dataset_notwins$complete13[dataset_notwins$selectedf13==1 & !is.na(dataset_notwins$no2_week.F13)]<-1
dataset_notwins$incl_var[(dataset_notwins$complete9==1 | dataset_notwins$complete13==1)]<-1
dataset_notwins$incl_var<-as.factor(dataset_notwins$incl_var)

dataset_notwins$incl_var_school<-0
dataset_notwins$complete9_sch[dataset_notwins$selectedf9==1 & !is.na(dataset_notwins$no2_week_schools.F09)]<-1
dataset_notwins$complete13_sch[dataset_notwins$selectedf13==1 & !is.na(dataset_notwins$no2_week_schools.F13)]<-1
dataset_notwins$incl_var_school[(dataset_notwins$complete9_sch==1 | dataset_notwins$complete13_sch==1)]<-1
dataset_notwins$incl_var_school<-as.factor(dataset_notwins$incl_var_school)


#air pollution + mri data F9 and/or F13
dataset_ap<-dataset_notwins[which(dataset_notwins$incl_var==1),]
dataset_ap_not_included<-dataset_notwins[which(dataset_notwins$incl_var==0),]

#air pollution at school + mri data F9 and/or F13
dataset_ap_sch<-dataset_notwins[which(dataset_notwins$incl_var_school==1),]
dataset_ap_sch_not_included<-dataset_notwins[which(dataset_notwins$incl_var_school==0),]


#air pollution at home and/or school + mri data F9 and/or F13
dataset<-dataset_notwins[which(dataset_notwins$incl_var==1 | dataset_notwins$incl_var_school==1),]
dataset_not_included<-dataset_notwins[which(dataset_notwins$incl_var_school==0 & dataset_notwins$incl_var_school==0),]


#dataset long format
colnames(dataset)[which(colnames(dataset) %in% c("IDC","agechildbrainmrif9","agemri_f13") )] <- c("idc","agemri.F09","agemri.F13")

varying = c("agemri.F09","agemri.F13") 
idVar = 'idc'
timeVar = 'ses'

dataset_long <- reshape(dataset, varying=varying, idvar=idVar, timevar=timeVar, direction='long')

#merge dataset and dataset_lags

dataset_lags<-merge(dataset_long, dataset_lags, by = c("idc", "ses"), all.x = T, sort = T)

#remove acute info when a participant is not selected (mri) in a visit 
#not supposed to have a date
head(dataset_lags[is.na(dataset_lags$selectedf9),])
head(dataset_lags[is.na(dataset_lags$selectedf13),])

for (v in vars){
  dataset_lags[,v][is.na(dataset_lags$selectedf9) & dataset_lags$ses=="F09"]<-NA
  dataset_lags[,v][is.na(dataset_lags$selectedf13) & dataset_lags$ses=="F13"]<-NA
  
}

#check home vs school data 
table(is.na(dataset_lags$no2_week))
table(is.na(dataset_lags$no2_week_schools))

table(is.na(dataset_lags$no2_week_schools)[dataset_lags$complete9==1 & dataset_lags$ses=="F09"])
table(is.na(dataset_lags$no2_week_schools)[dataset_lags$complete13==1 & dataset_lags$ses=="F13"])

dataset_lags$idc[dataset_lags$complete9==1 & dataset_lags$ses=="F09" & is.na(dataset_lags$no2_week_schools)]

##CHECK IF ALL HAVE SEASON DATA
table(is.na(dataset_lags$season_Lag0))
table(is.na(dataset_lags$season_Lag0)[dataset_lags$complete9==1 & dataset_lags$ses=="F09"]) 
table(is.na(dataset_lags$season_Lag0)[dataset_lags$complete13==1 & dataset_lags$ses=="F13"])

##CHECK IF ALL HAVE temperature DATA
table(is.na(dataset_lags$TG_Lag1))
table(is.na(dataset_lags$TG_Lag1)[dataset_lags$complete9==1 & dataset_lags$ses=="F09"])
table(is.na(dataset_lags$TG_Lag1)[dataset_lags$complete13==1 & dataset_lags$ses=="F13"])

id1<-dataset_lags$idc[which(is.na(dataset_lags$TG_Lag1) & dataset_lags$complete9==1 & dataset_lags$ses=="F09" )]
id2<-dataset_lags$idc[which(is.na(dataset_lags$TG_Lag1) & dataset_lags$complete13==1 & dataset_lags$ses=="F13" )]


##CHECK IF ALL HAVE humidity DATA (if not, impute)
table(is.na(dataset_lags$HU_Lag1))
table(is.na(dataset_lags$HU_Lag1)[dataset_lags$complete9==1 & dataset_lags$ses=="F09"])
#9 missings
table(is.na(dataset_lags$HU_Lag1)[dataset_lags$complete13==1 & dataset_lags$ses=="F13"])
#19 missings



#merge dataset and dfnc metrics
dataset_metrics<-merge(dataset_lags, df_id2, by = c("idc", "ses"), all.x = T, sort = T)

#demean age 
dataset_metrics$agemri_dm<- dataset_metrics$agemri-mean(dataset_metrics$agemri[dataset_metrics$ses=='F09'], na.rm=TRUE)

#transformation MDT

#first we need to get rid of the 0 because boxcox does not allow them
dataset_metrics$mdt1_p<-as.numeric(dataset_metrics$mdt1) + 1
dataset_metrics$mdt2_p<-as.numeric(dataset_metrics$mdt2) + 1
dataset_metrics$mdt3_p<-as.numeric(dataset_metrics$mdt3) + 1
dataset_metrics$mdt4_p<-as.numeric(dataset_metrics$mdt4) + 1
dataset_metrics$mdt5_p<-as.numeric(dataset_metrics$mdt5) + 1


# use box-cox transformation from package bestNormalize:
mdt1_tr <- bestNormalize::boxcox(dataset_metrics$mdt1_p)
dataset_metrics$mdt1_trafo <- mdt1_tr$x.t
#hist(dataset_metrics$mdt1_p, breaks = 200)
#hist(dataset_metrics$mdt1_trafo, breaks = 200)

mdt2_tr <- bestNormalize::boxcox(dataset_metrics$mdt2_p)
dataset_metrics$mdt2_trafo <- mdt2_tr$x.t
#hist(dataset_metrics$mdt2_p, breaks = 200)
#hist(dataset_metrics$mdt2_trafo, breaks = 200)

mdt3_tr <- bestNormalize::boxcox(dataset_metrics$mdt3_p)
dataset_metrics$mdt3_trafo <- mdt3_tr$x.t
#hist(dataset_metrics$mdt3_p, breaks = 200)
#hist(dataset_metrics$mdt3_trafo, breaks = 200)

mdt4_tr <- bestNormalize::boxcox(dataset_metrics$mdt4_p)
dataset_metrics$mdt4_trafo <- mdt4_tr$x.t
#hist(dataset_metrics$mdt4_p, breaks = 200)
#hist(dataset_metrics$mdt4_trafo, breaks = 200)

mdt5_tr <- bestNormalize::boxcox(dataset_metrics$mdt5_p)
dataset_metrics$mdt5_trafo <- mdt5_tr$x.t
#hist(dataset_metrics$mdt5_p, breaks = 200)
#hist(dataset_metrics$mdt5_trafo, breaks = 200)


rm(df,df_id2,temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,temp10, temp11,temp12, temp13,temp14,temp15,temp16,
   airpollution.data, airpollutionpreg.data, 
   airpollutionlags.data, airpollutionlags0.data, airpollutionlags1.data, airpollutionlags2.data, 
   airpollutionlags3.data, airpollutionlags4.data, airpollutionlags5.data,airpollutionlags6.data, 
   airpollutionlags7.data,airpollutionlagsdatum.data,airpollutionlagsall.data,
   alcohol.data, folic.data, greenspace.data, maternalbsi.data, maternaliq.data,
   noise.data, noisepreg.data, selected_sample, smoking.data, status.data, whole.data,birth.data,
   dataset_all,dataset_all_lag, dataset_lag1, dataset_lag1_wide, dataset_long,dataset_lags,
   mritime.data, mritime.data_long, mritimeF13.data, mritimeF9.data,
   temperaturelags.data, temperaturelags0.data, temperaturelags1.data, temperaturelags2.data,
   temperaturelags3.data, temperaturelags4.data, temperaturelags5.data, temperaturelags6.data, temperaturelags7.data,temperaturelagsall.data, season.data,
   nodup9, nodup13, airpollutionlagsschools.data,airpollutionlagsschools0.data,airpollutionlagsschools1.data,airpollutionlagsschools2.data,
   airpollutionlagsschools3.data,airpollutionlagsschools4.data,airpollutionlagsschools5.data,airpollutionlagsschools6.data,airpollutionlagsschools7.data,
   airpollutionlagsschoolsall.data,airpollutionlagsschoolsall_clean.data,airpollutionlagsschoolsall1.data,airpollutionlagsschoolsall2.data,airpollutionlagsschoolsall3.data,airpollutionlagsschoolsall4.data,
   airpollutionlagsschoolsall5.data,airpollutionlagsschoolsall6.data,airpollutionlagsschoolsall7.data)

#save(dataset_metrics, file='results/dataset_metrics_long_noimp.Rdata')
#save(dataset_notwins, file='results/dataset_notwins_original.Rdata')
