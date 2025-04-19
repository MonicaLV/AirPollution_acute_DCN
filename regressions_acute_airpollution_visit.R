setwd("V:/medewerkers/529028 Lopez Vicente, M/AP")

source("analyses/final_acute/setUp.R")

# Load packages
library(gam)
library(lme4)
library(mice)
library(miceadds)
library(broom.mixed)
library(poolr)
library(sandwich)

#merge weights, imputed covariates, outcomes and exposures: 

#load data
load("results/weights_home9.RData")
load("results/weights_home13.RData")
load("results/weights_school9.RData")
load("results/weights_school13.RData")
ipw9$weights_avg.F09<-ipw9$weights_avg9
ipw13$weights_avg.F13<-ipw13$weights_avg13
ipw9sch$weights_avg.F09<-ipw9sch$weights_avg9sch
ipw13sch$weights_avg.F13<-ipw13sch$weights_avg13sch


load("results/imputedmids_analysis_home.RData")
imp_full_home<-imp_full
load("results/imputedmids_analysis_school.RData")
imp_full_school<-imp_full


#transform exposures variables increase units 
dataset_metrics$pm10_week_incr <- dataset_metrics$pm10_week/10
dataset_metrics$pm25_week_incr <- dataset_metrics$pm25_week/5
dataset_metrics$pmcoarse_week_incr <- dataset_metrics$pmcoarse_week/5
dataset_metrics$pm25abs_week_incr <- dataset_metrics$pm25abs_week
dataset_metrics$no2_week_incr <- dataset_metrics$no2_week/10
dataset_metrics$nox_week_incr <- dataset_metrics$nox_week/20

dataset_metrics$pm10_week_schools_incr <- dataset_metrics$pm10_week_schools/10
dataset_metrics$pm25_week_schools_incr <- dataset_metrics$pm25_week_schools/5
dataset_metrics$pmcoarse_week_schools_incr <- dataset_metrics$pmcoarse_week_schools/5
dataset_metrics$no2_week_schools_incr <- dataset_metrics$no2_week_schools/10
dataset_metrics$nox_week_schools_incr <- dataset_metrics$nox_week_schools/20

#reshape dataset_metrics to wide

vnames = c("mdt1_trafo", "mdt2_trafo", "mdt3_trafo", "mdt4_trafo", "mdt5_trafo", "agemri_dm" ,
           "no2_week","nox_week","pm25_week","pm25abs_week","pm10_week","pmcoarse_week",
           "no2_week_schools","nox_week_schools","pm25_week_schools","pm10_week_schools","pmcoarse_week_schools",
           "no2_week_incr","nox_week_incr","pm25_week_incr","pm25abs_week_incr","pm10_week_incr","pmcoarse_week_incr",
           "no2_week_schools_incr","nox_week_schools_incr","pm25_week_schools_incr","pm10_week_schools_incr","pmcoarse_week_schools_incr")
idVar = 'idc'
timeVar = 'ses'

dataset_wide <- reshape(dataset_metrics, idvar=idVar, v.names = vnames ,timevar=timeVar, direction='wide')

#merge (loop for each imputed dataset) - use dataset_wide

#values to delete (imputed covariates that have been imputed when the visit is not available)
vars9<-c("season_Lag0.F09", "time_factor.F09","weekday.F09","TG_week.F09","HU_week.F09")
vars13<-c("season_Lag0.F13", "time_factor.F13","weekday.F13","TG_week.F13","HU_week.F13")

for (i in 1:25) {
  imp_ana <- mice::complete(imp_full_home,i) 
  imp_ana <- merge(imp_ana,ipw9, by.x = "idc", by.y = "IDC")
  imp_ana <- merge(imp_ana,ipw13, by.x = "idc", by.y = "IDC")
  imp_ana <-merge(imp_ana[c("idc", "weights_avg.F09","weights_avg.F13", "ethnmv2_6cat","ethnfv2_6cat","GENDER","MARDICH","season","educm_3cat",
                            "mdrink_updated","income_4cat","SMOKE_ALL","FOLIUM_VALIDATED","parity_3cat","AGE_M_v2","AGE_BF_v2",
                            "BMI_0","BMI_P","APM_IQ","ndvi300_preg","STATUSSCORE","no2","nox","pm25","pm25abs","pm10","pmcoarse",
                            "season_Lag0.F09","season_Lag0.F13","time_factor.F09","time_factor.F13",
                            "weekday.F09","weekday.F13","TG_week.F09","HU_week.F09","TG_week.F13","HU_week.F13")],
                  
                  dataset_wide[c("idc","agemri_dm.F09","agemri_dm.F13","selectedf9","selectedf13",
                                 "no2_week.F09","nox_week.F09","pm25_week.F09","pm25abs_week.F09","pm10_week.F09","pmcoarse_week.F09",
                                 "mdt1_trafo.F09","mdt2_trafo.F09","mdt3_trafo.F09","mdt4_trafo.F09","mdt5_trafo.F09",
                                 "no2_week.F13","nox_week.F13","pm25_week.F13","pm25abs_week.F13","pm10_week.F13","pmcoarse_week.F13",
                                 "mdt1_trafo.F13","mdt2_trafo.F13","mdt3_trafo.F13","mdt4_trafo.F13","mdt5_trafo.F13",
                                 "no2_week_incr.F09","nox_week_incr.F09","pm25_week_incr.F09","pm25abs_week_incr.F09","pm10_week_incr.F09","pmcoarse_week_incr.F09",
                                 "no2_week_incr.F13","nox_week_incr.F13","pm25_week_incr.F13","pm25abs_week_incr.F13","pm10_week_incr.F13","pmcoarse_week_incr.F13")],
                  by = "idc")
  for (v in vars9){
    imp_ana[,v][is.na(imp_ana$selectedf9)]<-NA  
  }
  for (v in vars13){
    imp_ana[,v][is.na(imp_ana$selectedf13)]<-NA
  }
  
  imp_ana_long <- reshape(imp_ana, varying=c("weights_avg.F09","weights_avg.F13","season_Lag0.F09","season_Lag0.F13","time_factor.F09","time_factor.F13",
                                             "weekday.F09","weekday.F13","TG_week.F09","HU_week.F09","TG_week.F13","HU_week.F13", "agemri_dm.F09","agemri_dm.F13",
                                             "no2_week.F09","nox_week.F09","pm25_week.F09","pm25abs_week.F09","pm10_week.F09","pmcoarse_week.F09",
                                             "mdt1_trafo.F09","mdt2_trafo.F09","mdt3_trafo.F09","mdt4_trafo.F09","mdt5_trafo.F09",
                                             "no2_week.F13","nox_week.F13","pm25_week.F13","pm25abs_week.F13","pm10_week.F13","pmcoarse_week.F13",
                                             "mdt1_trafo.F13","mdt2_trafo.F13","mdt3_trafo.F13","mdt4_trafo.F13","mdt5_trafo.F13",
                                             "no2_week_incr.F09","nox_week_incr.F09","pm25_week_incr.F09","pm25abs_week_incr.F09","pm10_week_incr.F09","pmcoarse_week_incr.F09",
                                             "no2_week_incr.F13","nox_week_incr.F13","pm25_week_incr.F13","pm25abs_week_incr.F13","pm10_week_incr.F13","pmcoarse_week_incr.F13"), idvar='idc', timevar='ses', direction='long')
  
  imp_ana_long_F09 <- imp_ana_long[imp_ana_long$ses=="F09",]
  imp_ana_long_F13 <- imp_ana_long[imp_ana_long$ses=="F13",]
  
  assign(paste0("final_F09_",i), imp_ana_long_F09)  
  assign(paste0("final_F13_",i), imp_ana_long_F13)

}

# turn back into mids object 
# Create a list
final_list_F09 <- list(final_F09_1,final_F09_2,final_F09_3,final_F09_4,final_F09_5,final_F09_6,final_F09_7,final_F09_8,final_F09_9,final_F09_10,
                   final_F09_11,final_F09_12,final_F09_13,final_F09_14,final_F09_15,final_F09_16,final_F09_17,final_F09_18,final_F09_19,
                   final_F09_20,final_F09_21,final_F09_22,final_F09_23,final_F09_24,final_F09_25)

final_list_F13 <- list(final_F13_1,final_F13_2,final_F13_3,final_F13_4,final_F13_5,final_F13_6,final_F13_7,final_F13_8,final_F13_9,final_F13_10,
                       final_F13_11,final_F13_12,final_F13_13,final_F13_14,final_F13_15,final_F13_16,final_F13_17,final_F13_18,final_F13_19,
                       final_F13_20,final_F13_21,final_F13_22,final_F13_23,final_F13_24,final_F13_25)

# Combine list back to MIDS object to use the pool function of R again
final_impset_home_F09  <- datalist2mids(final_list_F09)
final_impset_home_F13  <- datalist2mids(final_list_F13)

#same for SCHOOL dataset


for (i in 1:25) {
  imp_ana <- mice::complete(imp_full_school,i) 
  imp_ana <- merge(imp_ana,ipw9sch, by.x = "idc", by.y = "IDC")
  imp_ana <- merge(imp_ana,ipw13sch, by.x = "idc", by.y = "IDC")
  
  imp_ana <-merge(imp_ana[c("idc", "weights_avg.F09","weights_avg.F13","ethnmv2_6cat","ethnfv2_6cat","GENDER","MARDICH","season","educm_3cat",
                            "mdrink_updated","income_4cat","SMOKE_ALL","FOLIUM_VALIDATED","parity_3cat","AGE_M_v2","AGE_BF_v2",
                            "BMI_0","BMI_P","APM_IQ","ndvi300_preg","STATUSSCORE","no2","nox","pm25","pm25abs","pm10","pmcoarse",
                            "season_Lag0.F09","season_Lag0.F13","time_factor.F09","time_factor.F13",
                            "weekday.F09","weekday.F13","TG_week.F09","HU_week.F09","TG_week.F13","HU_week.F13")],
                  
                  dataset_wide[c("idc","agemri_dm.F09","agemri_dm.F13","selectedf9","selectedf13",
                                 "no2_week_schools.F09","nox_week_schools.F09","pm25_week_schools.F09","pm10_week_schools.F09","pmcoarse_week_schools.F09",
                                 "mdt1_trafo.F09","mdt2_trafo.F09","mdt3_trafo.F09","mdt4_trafo.F09","mdt5_trafo.F09",
                                 "no2_week_schools.F13","nox_week_schools.F13","pm25_week_schools.F13","pm10_week_schools.F13","pmcoarse_week_schools.F13",
                                 "mdt1_trafo.F13","mdt2_trafo.F13","mdt3_trafo.F13","mdt4_trafo.F13","mdt5_trafo.F13",
                                 "no2_week_schools_incr.F09","nox_week_schools_incr.F09","pm25_week_schools_incr.F09","pm10_week_schools_incr.F09","pmcoarse_week_schools_incr.F09",
                                 "no2_week_schools_incr.F13","nox_week_schools_incr.F13","pm25_week_schools_incr.F13","pm10_week_schools_incr.F13","pmcoarse_week_schools_incr.F13")],
                  by = "idc")
  for (v in vars9){
    imp_ana[,v][is.na(imp_ana$selectedf9)]<-NA  
  }
  for (v in vars13){
    imp_ana[,v][is.na(imp_ana$selectedf13)]<-NA
  }
  
  imp_ana_long <- reshape(imp_ana, varying=c("weights_avg.F09","weights_avg.F13","season_Lag0.F09","season_Lag0.F13","time_factor.F09","time_factor.F13",
                                             "weekday.F09","weekday.F13","TG_week.F09","HU_week.F09","TG_week.F13","HU_week.F13", "agemri_dm.F09","agemri_dm.F13",
                                             "no2_week_schools.F09","nox_week_schools.F09","pm25_week_schools.F09","pm10_week_schools.F09","pmcoarse_week_schools.F09",
                                             "mdt1_trafo.F09","mdt2_trafo.F09","mdt3_trafo.F09","mdt4_trafo.F09","mdt5_trafo.F09",
                                             "no2_week_schools.F13","nox_week_schools.F13","pm25_week_schools.F13","pm10_week_schools.F13","pmcoarse_week_schools.F13",
                                             "mdt1_trafo.F13","mdt2_trafo.F13","mdt3_trafo.F13","mdt4_trafo.F13","mdt5_trafo.F13",
                                             "no2_week_schools_incr.F09","nox_week_schools_incr.F09","pm25_week_schools_incr.F09","pm10_week_schools_incr.F09","pmcoarse_week_schools_incr.F09",
                                             "no2_week_schools_incr.F13","nox_week_schools_incr.F13","pm25_week_schools_incr.F13","pm10_week_schools_incr.F13","pmcoarse_week_schools_incr.F13"), idvar='idc', timevar='ses', direction='long')
  imp_ana_long_F09 <- imp_ana_long[imp_ana_long$ses=="F09",]
  imp_ana_long_F13 <- imp_ana_long[imp_ana_long$ses=="F13",]
  
  assign(paste0("final_F09_",i), imp_ana_long_F09)  
  assign(paste0("final_F13_",i), imp_ana_long_F13)
  
}

# turn back into mids object 
# Create a list
final_list_F09 <- list(final_F09_1,final_F09_2,final_F09_3,final_F09_4,final_F09_5,final_F09_6,final_F09_7,final_F09_8,final_F09_9,final_F09_10,
                       final_F09_11,final_F09_12,final_F09_13,final_F09_14,final_F09_15,final_F09_16,final_F09_17,final_F09_18,final_F09_19,
                       final_F09_20,final_F09_21,final_F09_22,final_F09_23,final_F09_24,final_F09_25)

final_list_F13 <- list(final_F13_1,final_F13_2,final_F13_3,final_F13_4,final_F13_5,final_F13_6,final_F13_7,final_F13_8,final_F13_9,final_F13_10,
                       final_F13_11,final_F13_12,final_F13_13,final_F13_14,final_F13_15,final_F13_16,final_F13_17,final_F13_18,final_F13_19,
                       final_F13_20,final_F13_21,final_F13_22,final_F13_23,final_F13_24,final_F13_25)

# Combine list back to MIDS object to use the pool function of R again
final_impset_school_F09  <- datalist2mids(final_list_F09)
final_impset_school_F13  <- datalist2mids(final_list_F13)


#regression models home

xvars_week <-c( "no2_week_incr","nox_week_incr","pm25_week_incr","pm25abs_week_incr","pm10_week_incr","pmcoarse_week_incr")

covars_week <-c( "time_factor", "weekday", "season_Lag0","TG_week", "HU_week",
                 "AGE_M_v2", "AGE_BF_v2", "educm_3cat", "ethnmv2_6cat", "MARDICH", "income_4cat", "SMOKE_ALL", "mdrink_updated", "FOLIUM_VALIDATED",
                 "season",  "parity_3cat", "BMI_0", "APM_IQ", "ndvi300_preg", "STATUSSCORE" )

x2 <- 'agemri_dm'

xvars_child<- c("no2","nox","pm25","pm25abs","pm10","pmcoarse")

f_base <- paste("~",  paste0(xvars_week, "+", x2, "+"), paste0(covars_week, collapse = " + "))
f <- paste(y,f_base[i], "+",xvars_child[i])
fmFull<- with(final_impset_home_F09, lm(as.formula(f), weights = weights_avg))
fmFullSummary <- data.frame(summary(pool(fmFull))) 

table_mdt1_acute_week_F09<-linear_regression(final_impset_home_F09,  y='mdt1_trafo', xvars_week)
table_mdt2_acute_week_F09<-linear_regression(final_impset_home_F09,  y='mdt2_trafo', xvars_week)
table_mdt3_acute_week_F09<-linear_regression(final_impset_home_F09,  y='mdt3_trafo', xvars_week)
table_mdt4_acute_week_F09<-linear_regression(final_impset_home_F09,  y='mdt4_trafo', xvars_week)
table_mdt5_acute_week_F09<-linear_regression(final_impset_home_F09,  y='mdt5_trafo', xvars_week)


table_mdt1_acute_week_F13<-linear_regression(final_impset_home_F13,  y='mdt1_trafo', xvars_week)
table_mdt2_acute_week_F13<-linear_regression(final_impset_home_F13,  y='mdt2_trafo', xvars_week)
table_mdt3_acute_week_F13<-linear_regression(final_impset_home_F13,  y='mdt3_trafo', xvars_week)
table_mdt4_acute_week_F13<-linear_regression(final_impset_home_F13,  y='mdt4_trafo', xvars_week)
table_mdt5_acute_week_F13<-linear_regression(final_impset_home_F13,  y='mdt5_trafo', xvars_week)

tables_acute_week_F09<-list(table_mdt1_acute_week_F09,table_mdt2_acute_week_F09,table_mdt3_acute_week_F09,table_mdt4_acute_week_F09,table_mdt5_acute_week_F09)
tables_acute_week_F13<-list(table_mdt1_acute_week_F13,table_mdt2_acute_week_F13,table_mdt3_acute_week_F13,table_mdt4_acute_week_F13,table_mdt5_acute_week_F13)

write.xlsx(tables_acute_week_F09, file =  'results/tables_acute_week_F09.xlsx')
write.xlsx(tables_acute_week_F13, file =  'results/tables_acute_week_F13.xlsx')

#without chronic effect
table_mdt1_acute_week_nc_F09<-linear_regression_nocr(final_impset_home_F09,  y='mdt1_trafo', xvars_week)
table_mdt2_acute_week_nc_F09<-linear_regression_nocr(final_impset_home_F09,  y='mdt2_trafo', xvars_week)
table_mdt3_acute_week_nc_F09<-linear_regression_nocr(final_impset_home_F09,  y='mdt3_trafo', xvars_week)
table_mdt4_acute_week_nc_F09<-linear_regression_nocr(final_impset_home_F09,  y='mdt4_trafo', xvars_week)
table_mdt5_acute_week_nc_F09<-linear_regression_nocr(final_impset_home_F09,  y='mdt5_trafo', xvars_week)


table_mdt1_acute_week_nc_F13<-linear_regression_nocr(final_impset_home_F13,  y='mdt1_trafo', xvars_week)
table_mdt2_acute_week_nc_F13<-linear_regression_nocr(final_impset_home_F13,  y='mdt2_trafo', xvars_week)
table_mdt3_acute_week_nc_F13<-linear_regression_nocr(final_impset_home_F13,  y='mdt3_trafo', xvars_week)
table_mdt4_acute_week_nc_F13<-linear_regression_nocr(final_impset_home_F13,  y='mdt4_trafo', xvars_week)
table_mdt5_acute_week_nc_F13<-linear_regression_nocr(final_impset_home_F13,  y='mdt5_trafo', xvars_week)

tables_acute_week_nc_F09<-list(table_mdt1_acute_week_nc_F09,table_mdt2_acute_week_nc_F09,table_mdt3_acute_week_nc_F09,table_mdt4_acute_week_nc_F09,table_mdt5_acute_week_nc_F09)
tables_acute_week_nc_F13<-list(table_mdt1_acute_week_nc_F13,table_mdt2_acute_week_nc_F13,table_mdt3_acute_week_nc_F13,table_mdt4_acute_week_nc_F13,table_mdt5_acute_week_nc_F13)

write.xlsx(tables_acute_week_nc_F09, file =  'results/tables_acute_week_nocr_F09.xlsx')
write.xlsx(tables_acute_week_nc_F13, file =  'results/tables_acute_week_nocr_F13.xlsx')


#regression models school

xvars_week<-c( "no2_week_schools_incr","nox_week_schools_incr","pm25_week_schools_incr","pm10_week_schools_incr","pmcoarse_week_schools_incr")
xvars_child<- c("no2","nox","pm25","pm10","pmcoarse")
f_base <- paste("~",  paste0(xvars_week, "+", x2, "+"), paste0(covars_week, collapse = " + "))

table_mdt1_acute_week_school_F09<-linear_regression(final_impset_school_F09,  y='mdt1_trafo', xvars_week)
table_mdt2_acute_week_school_F09<-linear_regression(final_impset_school_F09,  y='mdt2_trafo', xvars_week)
table_mdt3_acute_week_school_F09<-linear_regression(final_impset_school_F09,  y='mdt3_trafo', xvars_week)
table_mdt4_acute_week_school_F09<-linear_regression(final_impset_school_F09,  y='mdt4_trafo', xvars_week)
table_mdt5_acute_week_school_F09<-linear_regression(final_impset_school_F09,  y='mdt5_trafo', xvars_week)


table_mdt1_acute_week_school_F13<-linear_regression(final_impset_school_F13,  y='mdt1_trafo', xvars_week)
table_mdt2_acute_week_school_F13<-linear_regression(final_impset_school_F13,  y='mdt2_trafo', xvars_week)
table_mdt3_acute_week_school_F13<-linear_regression(final_impset_school_F13,  y='mdt3_trafo', xvars_week)
table_mdt4_acute_week_school_F13<-linear_regression(final_impset_school_F13,  y='mdt4_trafo', xvars_week)
table_mdt5_acute_week_school_F13<-linear_regression(final_impset_school_F13,  y='mdt5_trafo', xvars_week)


tables_acute_week_school_F09<-list(table_mdt1_acute_week_school_F09,table_mdt2_acute_week_school_F09,
                                   table_mdt3_acute_week_school_F09,table_mdt4_acute_week_school_F09,
                                   table_mdt5_acute_week_school_F09)
tables_acute_week_school_F13<-list(table_mdt1_acute_week_school_F13,table_mdt2_acute_week_school_F13,
                               table_mdt3_acute_week_school_F13,table_mdt4_acute_week_school_F13,
                               table_mdt5_acute_week_school_F13)

write.xlsx(tables_acute_week_school_F09, file =  'results/tables_acute_week_school_F09.xlsx')
write.xlsx(tables_acute_week_school_F13, file =  'results/tables_acute_week_school_F13.xlsx')

#without chronic effect
table_mdt1_acute_week_school_nc_F09<-linear_regression_nocr(final_impset_school_F09,  y='mdt1_trafo', xvars_week)
table_mdt2_acute_week_school_nc_F09<-linear_regression_nocr(final_impset_school_F09,  y='mdt2_trafo', xvars_week)
table_mdt3_acute_week_school_nc_F09<-linear_regression_nocr(final_impset_school_F09,  y='mdt3_trafo', xvars_week)
table_mdt4_acute_week_school_nc_F09<-linear_regression_nocr(final_impset_school_F09,  y='mdt4_trafo', xvars_week)
table_mdt5_acute_week_school_nc_F09<-linear_regression_nocr(final_impset_school_F09,  y='mdt5_trafo', xvars_week)

table_mdt1_acute_week_school_nc_F13<-linear_regression_nocr(final_impset_school_F13,  y='mdt1_trafo', xvars_week)
table_mdt2_acute_week_school_nc_F13<-linear_regression_nocr(final_impset_school_F13,  y='mdt2_trafo', xvars_week)
table_mdt3_acute_week_school_nc_F13<-linear_regression_nocr(final_impset_school_F13,  y='mdt3_trafo', xvars_week)
table_mdt4_acute_week_school_nc_F13<-linear_regression_nocr(final_impset_school_F13,  y='mdt4_trafo', xvars_week)
table_mdt5_acute_week_school_nc_F13<-linear_regression_nocr(final_impset_school_F13,  y='mdt5_trafo', xvars_week)

tables_acute_week_school_nc_F09<-list(table_mdt1_acute_week_school_nc_F09,table_mdt2_acute_week_school_nc_F09,
                                  table_mdt3_acute_week_school_nc_F09,table_mdt4_acute_week_school_nc_F09,
                                  table_mdt5_acute_week_school_nc_F09)
tables_acute_week_school_nc_F13<-list(table_mdt1_acute_week_school_nc_F13,table_mdt2_acute_week_school_nc_F13,
                                      table_mdt3_acute_week_school_nc_F13,table_mdt4_acute_week_school_nc_F13,
                                      table_mdt5_acute_week_school_nc_F13)

write.xlsx(tables_acute_week_school_nc_F09, file =  'results/tables_acute_week_school_nochronic_F09.xlsx')
write.xlsx(tables_acute_week_school_nc_F13, file =  'results/tables_acute_week_school_nochronic_F13.xlsx')


#summary tables
labels<-c("exposures", "MDT1", "MDT2", "MDT3", "MDT4", "MDT5")

acuteweek_table <- data.frame(table_mdt1_acute_week_F09$xs, table_mdt1_acute_week_F09$cis, table_mdt2_acute_week_F09$cis, 
                              table_mdt3_acute_week_F09$cis, table_mdt4_acute_week_F09$cis, table_mdt5_acute_week_F09$cis)
colnames(acuteweek_table)<-c(labels)

acuteweek_school_table <- data.frame(table_mdt1_acute_week_school_F09$xs, table_mdt1_acute_week_school_F09$cis, table_mdt2_acute_week_school_F09$cis, 
                                     table_mdt3_acute_week_school_F09$cis, table_mdt4_acute_week_school_F09$cis, table_mdt5_acute_week_school_F09$cis)
colnames(acuteweek_school_table)<-c(labels)

week_table_final_F09 = rbind(acuteweek_table[1,],acuteweek_school_table[1,], acuteweek_table[2,],acuteweek_school_table[2,],
                         acuteweek_table[3,],acuteweek_school_table[3,],acuteweek_table[5,],acuteweek_school_table[4,],
                         acuteweek_table[6,],acuteweek_school_table[5,],acuteweek_table[4,])

write.xlsx(week_table_final_F09, file =  'results/summary_table_acute_week_home_school_F09.xlsx')


acuteweek_table <- data.frame(table_mdt1_acute_week_F13$xs, table_mdt1_acute_week_F13$cis, table_mdt2_acute_week_F13$cis, 
                              table_mdt3_acute_week_F13$cis, table_mdt4_acute_week_F13$cis, table_mdt5_acute_week_F13$cis)
colnames(acuteweek_table)<-c(labels)

acuteweek_school_table <- data.frame(table_mdt1_acute_week_school_F13$xs, table_mdt1_acute_week_school_F13$cis, table_mdt2_acute_week_school_F13$cis, 
                                     table_mdt3_acute_week_school_F13$cis, table_mdt4_acute_week_school_F13$cis, table_mdt5_acute_week_school_F13$cis)
colnames(acuteweek_school_table)<-c(labels)

week_table_final_F13 = rbind(acuteweek_table[1,],acuteweek_school_table[1,], acuteweek_table[2,],acuteweek_school_table[2,],
                             acuteweek_table[3,],acuteweek_school_table[3,],acuteweek_table[5,],acuteweek_school_table[4,],
                             acuteweek_table[6,],acuteweek_school_table[5,],acuteweek_table[4,])

write.xlsx(week_table_final_F13, file =  'results/summary_table_acute_week_home_school_F13.xlsx')




acuteweek_table <- data.frame(table_mdt1_acute_week_nc_F09$xs, table_mdt1_acute_week_nc_F09$cis, table_mdt2_acute_week_nc_F09$cis, 
                              table_mdt3_acute_week_nc_F09$cis, table_mdt4_acute_week_nc_F09$cis, table_mdt5_acute_week_nc_F09$cis)
colnames(acuteweek_table)<-c(labels)

acuteweek_school_table <- data.frame(table_mdt1_acute_week_school_nc_F09$xs, table_mdt1_acute_week_school_nc_F09$cis, table_mdt2_acute_week_school_nc_F09$cis, 
                                     table_mdt3_acute_week_school_nc_F09$cis, table_mdt4_acute_week_school_nc_F09$cis, table_mdt5_acute_week_school_nc_F09$cis)
colnames(acuteweek_school_table)<-c(labels)

week_table_final_nc_F09 = rbind(acuteweek_table[1,],acuteweek_school_table[1,], acuteweek_table[2,],acuteweek_school_table[2,],
                             acuteweek_table[3,],acuteweek_school_table[3,],acuteweek_table[5,],acuteweek_school_table[4,],
                             acuteweek_table[6,],acuteweek_school_table[5,],acuteweek_table[4,])

write.xlsx(week_table_final_nc_F09, file =  'results/summary_table_acute_week_home_school_nocr_F09.xlsx')


acuteweek_table <- data.frame(table_mdt1_acute_week_nc_F13$xs, table_mdt1_acute_week_nc_F13$cis, table_mdt2_acute_week_nc_F13$cis, 
                              table_mdt3_acute_week_nc_F13$cis, table_mdt4_acute_week_nc_F13$cis, table_mdt5_acute_week_nc_F13$cis)
colnames(acuteweek_table)<-c(labels)

acuteweek_school_table <- data.frame(table_mdt1_acute_week_school_nc_F13$xs, table_mdt1_acute_week_school_nc_F13$cis, table_mdt2_acute_week_school_nc_F13$cis, 
                                     table_mdt3_acute_week_school_nc_F13$cis, table_mdt4_acute_week_school_nc_F13$cis, table_mdt5_acute_week_school_nc_F13$cis)
colnames(acuteweek_school_table)<-c(labels)

week_table_final_nc_F13 = rbind(acuteweek_table[1,],acuteweek_school_table[1,], acuteweek_table[2,],acuteweek_school_table[2,],
                             acuteweek_table[3,],acuteweek_school_table[3,],acuteweek_table[5,],acuteweek_school_table[4,],
                             acuteweek_table[6,],acuteweek_school_table[5,],acuteweek_table[4,])

write.xlsx(week_table_final_nc_F13, file =  'results/summary_table_acute_week_home_school_nocr_F13.xlsx')
