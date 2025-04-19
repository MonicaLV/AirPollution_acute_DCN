#--------------------------------------------------------------------------#
# Acute air pollution and DFNC                                             #
# Author: Monica Lopez                                                     #
# Date: 27/9/2024                                                          #
# Description: GAM & mixed models  Air pollution home & school vs MDT      #
# Changes (programmer/date):                                               #
#--------------------------------------------------------------------------#

#R version 4.3.1

setwd("V:/medewerkers/529028 Lopez Vicente, M/AP")

source("analyses/final_acute/setUp.R")

# Load packages
library(gam)
library(lme4)
library(mice)
library(miceadds)
library(broom.mixed)
library(poolr)

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
  
  assign(paste0("final_",i), imp_ana_long)
}

# turn back into mids object 
# Create a list
final_list <- list(final_1,final_2,final_3,final_4,final_5,final_6,final_7,final_8,final_9,final_10,
                   final_11,final_12,final_13,final_14,final_15,final_16,final_17,final_18,final_19,
                   final_20,final_21,final_22,final_23,final_24,final_25)

# Combine list back to MIDS object to use the pool function of R again
final_impset_home  <- datalist2mids(final_list)


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
  
  assign(paste0("final_",i), imp_ana_long)
}

# turn back into mids object 
# Create a list
final_list <- list(final_1,final_2,final_3,final_4,final_5,final_6,final_7,final_8,final_9,final_10,
                   final_11,final_12,final_13,final_14,final_15,final_16,final_17,final_18,final_19,
                   final_20,final_21,final_22,final_23,final_24,final_25)

# Combine list back to MIDS object to use the pool function of R again
final_impset_school  <- datalist2mids(final_list)




#GAM models

final25home<-complete(final_impset_home, 25)

final25home_F09<-final25home[final25home$ses=="F09",]
final25home_F13<-final25home[final25home$ses=="F13",]


xvars_week<-c( "no2_week","nox_week","pm25_week","pm25abs_week","pm10_week","pmcoarse_week")

covars_week<-c("agemri_dm",  "time_factor", "weekday", "season_Lag0","TG_week", "HU_week", "educm_3cat","ethnmv2_6cat", "ethnfv2_6cat", "MARDICH", 
               'income_4cat', "SMOKE_ALL", "mdrink_updated", "FOLIUM_VALIDATED", "parity_3cat", 
               'AGE_M_v2',"AGE_BF_v2",'BMI_0',  "APM_IQ", "ndvi300_preg", "STATUSSCORE", "season" )

xvars_child<- c("no2","nox","pm25","pm25abs","pm10","pmcoarse")

xvars_temp_week <- c("TG_week","HU_week")

yvars <- c('mdt1_trafo', 'mdt2_trafo', 'mdt3_trafo', 'mdt4_trafo', 'mdt5_trafo')


f_base <- paste("~",  paste0("ns(", xvars_week, ", df=2) +"), paste0(covars_week, collapse = " + ")) # make the right-hand side of the formula

#f <- paste('mdt1_trafo', f_base[4], "+", xvars_child[4])

pdf(file = 'results/gamF09_ap_week_acute.pdf')

gam_f09_ap_week_acute<-gam_function(final25home_F09, yvars, xvars_week)

dev.off()

pdf(file = 'results/gamF13_ap_week_acute.pdf')

gam_f13_ap_week_acute<-gam_function(final25home_F13, yvars, xvars_week)

dev.off()


# GAM schools

final25school<-complete(final_impset_school, 25)

final25school_F09<-final25school[final25school$ses=="F09",]
final25school_F13<-final25school[final25school$ses=="F13",]


xvars_week<-c( "no2_week_schools","nox_week_schools","pm25_week_schools","pm10_week_schools","pmcoarse_week_schools")


xvars_child<- c("no2","nox","pm25","pm10","pmcoarse")


f_base <- paste("~",  paste0("ns(", xvars_week, ", df=2) +"), paste0(covars_week, collapse = " + ")) # make the right-hand side of the formula

pdf(file = 'results/gamF09_ap_week_acute_school.pdf')

gam_f09_ap_week_acute_school<-gam_function(final25school_F09, yvars, xvars_week)

dev.off()

pdf(file = 'results/gamF13_ap_week_acute_school.pdf')

gam_f13_ap_week_acute_school<-gam_function(final25school_F13, yvars, xvars_week)

dev.off()

#temperature by outcome
f_base <- paste("~",  paste0("ns(", xvars_temp_week, ", df=2)")) # make the right-hand side of the formula

for (i in seq_along(f_base)){ 
  for (y in yvars){
    f <- paste(y, f_base[i])
    fmFull <- gam(as.formula(f), data=final25home_F09, weights = final25home_F09$weights_avg)
    par(mfrow = c(1,1))
    plot.Gam(fmFull, residuals=F, se=TRUE, col="red", cex=1, terms= paste0("ns(", xvars_temp_week[i], ", df = 2)"), main = paste0(xvars_temp_week[i], 'vs', y))
  }
}


for (i in seq_along(f_base)){ 
  for (y in yvars){
    f <- paste(y, f_base[i])
    fmFull <- gam(as.formula(f), data=final25home_F13, weights = final25home_F13$weights_avg)
    par(mfrow = c(1,1))
    plot.Gam(fmFull, residuals=F, se=TRUE, col="red", cex=1, terms= paste0("ns(", xvars_temp_week[i], ", df = 2)"), main = paste0(xvars_temp_week[i], 'vs', y))
  }
}

#regression models home

xvars_week <-c( "no2_week_incr","nox_week_incr","pm25_week_incr","pm25abs_week_incr","pm10_week_incr","pmcoarse_week_incr")

covars_week <-c( "time_factor", "weekday", "season_Lag0","TG_week", "HU_week",
                 "AGE_M_v2", "AGE_BF_v2", "educm_3cat", "ethnmv2_6cat", "MARDICH", "income_4cat", "SMOKE_ALL", "mdrink_updated", "FOLIUM_VALIDATED",
                 "season",  "parity_3cat", "BMI_0", "APM_IQ", "ndvi300_preg", "STATUSSCORE" )

x2 <- 'agemri_dm'

xvars_child<- c("no2","nox","pm25","pm25abs","pm10","pmcoarse")

f_base <- paste("~",  paste0(xvars_week, "+", x2, "+"), paste0(covars_week, collapse = " + "))

# f<-paste("mdt5_trafo",f_base[2], "+",xvars_child[2],"+" ,"(1 |idc)")
# test_model <-with(final_impset_home, lmer(as.formula(f), weights = weights_avg))
# test_model2 <- data.frame(summary(pool(test_model)))
# test_model2$LCI <- test_model2$estimate-1.96*test_model2$std.error
# test_model2$UCI <- test_model2$estimate+1.96*test_model2$std.error

# f<-paste("mdt4_trafo",f_base[2], "+",xvars_child[2],"+" ,"(1 |idc)")
# test_model <-with(final_impset_home, lmer(as.formula(f), weights = weights_avg))
# test_model2 <- data.frame(summary(pool(test_model)))
# test_model2$LCI <- test_model2$estimate-1.96*test_model2$std.error
# test_model2$UCI <- test_model2$estimate+1.96*test_model2$std.error

table_mdt1_acute_week<-get_lme4_info_all_iterx_acute(final_impset_home,  y='mdt1_trafo', xvars_week)
table_mdt2_acute_week<-get_lme4_info_all_iterx_acute(final_impset_home,  y='mdt2_trafo', xvars_week)
table_mdt3_acute_week<-get_lme4_info_all_iterx_acute(final_impset_home,  y='mdt3_trafo', xvars_week)
table_mdt4_acute_week<-get_lme4_info_all_iterx_acute(final_impset_home,  y='mdt4_trafo', xvars_week)
table_mdt5_acute_week<-get_lme4_info_all_iterx_acute(final_impset_home,  y='mdt5_trafo', xvars_week)

tables_acute_week<-list(table_mdt1_acute_week,table_mdt2_acute_week,table_mdt3_acute_week,table_mdt4_acute_week,table_mdt5_acute_week)

write.xlsx(tables_acute_week, file =  'results/tables_acute_week.xlsx')


table_mdt1_acute_week_nc<-get_lme4_info_all_iterx_acute_nochr(final_impset_home,  y='mdt1_trafo', xvars_week)
table_mdt2_acute_week_nc<-get_lme4_info_all_iterx_acute_nochr(final_impset_home,  y='mdt2_trafo', xvars_week)
table_mdt3_acute_week_nc<-get_lme4_info_all_iterx_acute_nochr(final_impset_home,  y='mdt3_trafo', xvars_week)
table_mdt4_acute_week_nc<-get_lme4_info_all_iterx_acute_nochr(final_impset_home,  y='mdt4_trafo', xvars_week)
table_mdt5_acute_week_nc<-get_lme4_info_all_iterx_acute_nochr(final_impset_home,  y='mdt5_trafo', xvars_week)

tables_acute_week_nc<-list(table_mdt1_acute_week_nc,table_mdt2_acute_week_nc,table_mdt3_acute_week_nc,table_mdt4_acute_week_nc,table_mdt5_acute_week_nc)

write.xlsx(tables_acute_week_nc, file =  'results/tables_acute_week_nochronic.xlsx')

#minimally adjusted
covars_week_min <-c( "time_factor", "weekday", "season_Lag0","TG_week", "HU_week")
f_base <- paste("~",  paste0(xvars_week, "+", x2, "+"), paste0(covars_week_min, collapse = " + "))

table_mdt1_acute_week_min<-get_lme4_info_all_iterx_acute_nochr(final_impset_home,  y='mdt1_trafo', xvars_week)
table_mdt2_acute_week_min<-get_lme4_info_all_iterx_acute_nochr(final_impset_home,  y='mdt2_trafo', xvars_week)
table_mdt3_acute_week_min<-get_lme4_info_all_iterx_acute_nochr(final_impset_home,  y='mdt3_trafo', xvars_week)
table_mdt4_acute_week_min<-get_lme4_info_all_iterx_acute_nochr(final_impset_home,  y='mdt4_trafo', xvars_week)
table_mdt5_acute_week_min<-get_lme4_info_all_iterx_acute_nochr(final_impset_home,  y='mdt5_trafo', xvars_week)

tables_acute_week_min<-list(table_mdt1_acute_week_min,table_mdt2_acute_week_min,
                            table_mdt3_acute_week_min,table_mdt4_acute_week_min,table_mdt5_acute_week_min)

write.xlsx(tables_acute_week_min, file =  'results/tables_acute_week_minimallyAdj.xlsx')



#check residuals and vif (one imputed dataset)
library(car)

ys=c('mdt1_trafo','mdt2_trafo','mdt3_trafo','mdt4_trafo','mdt5_trafo')

for (y in ys){
  for (i in seq_along(f_base)){ 
  f <- paste(y,f_base[i], "+",xvars_child[i],"+" ,"(1 |idc)")
  fmFull <- lmer(as.formula(f), final25home, weights = final25home$weights_avg)
  qqnorm(resid(fmFull), main = xvars_week[i])
  print(vif(fmFull))
  }
}

for (y in ys){
  for (i in seq_along(f_base)){ 
    f <- paste(y,f_base[i], "+","(1 |idc)")
    fmFull <- lmer(as.formula(f), final25home, weights = final25home$weights_avg)
    qqnorm(resid(fmFull), main = xvars_week[i])
    print(vif(fmFull))
  }
}

#regression models school

xvars_week<-c( "no2_week_schools_incr","nox_week_schools_incr","pm25_week_schools_incr","pm10_week_schools_incr","pmcoarse_week_schools_incr")
xvars_child<- c("no2","nox","pm25","pm10","pmcoarse")
f_base <- paste("~",  paste0(xvars_week, "+", x2, "+"), paste0(covars_week, collapse = " + "))

# f<-paste("mdt1_trafo",f_base[3], "+",xvars_child[3],"+" ,"(1 |idc)")
# test_model <-with(final_impset_school, lmer(as.formula(f), weights = weights_avg))
# test_model2 <- data.frame(summary(pool(test_model))) 

table_mdt1_acute_week_school<-get_lme4_info_all_iterx_acute(final_impset_school,  y='mdt1_trafo', xvars_week)
table_mdt2_acute_week_school<-get_lme4_info_all_iterx_acute(final_impset_school,  y='mdt2_trafo', xvars_week)
table_mdt3_acute_week_school<-get_lme4_info_all_iterx_acute(final_impset_school,  y='mdt3_trafo', xvars_week)
table_mdt4_acute_week_school<-get_lme4_info_all_iterx_acute(final_impset_school,  y='mdt4_trafo', xvars_week)
table_mdt5_acute_week_school<-get_lme4_info_all_iterx_acute(final_impset_school,  y='mdt5_trafo', xvars_week)

tables_acute_week_school<-list(table_mdt1_acute_week_school,table_mdt2_acute_week_school,
                               table_mdt3_acute_week_school,table_mdt4_acute_week_school,
                               table_mdt5_acute_week_school)

write.xlsx(tables_acute_week_school, file =  'results/tables_acute_week_school.xlsx')

table_mdt1_acute_week_school_nc<-get_lme4_info_all_iterx_acute_nochr(final_impset_school,  y='mdt1_trafo', xvars_week)
table_mdt2_acute_week_school_nc<-get_lme4_info_all_iterx_acute_nochr(final_impset_school,  y='mdt2_trafo', xvars_week)
table_mdt3_acute_week_school_nc<-get_lme4_info_all_iterx_acute_nochr(final_impset_school,  y='mdt3_trafo', xvars_week)
table_mdt4_acute_week_school_nc<-get_lme4_info_all_iterx_acute_nochr(final_impset_school,  y='mdt4_trafo', xvars_week)
table_mdt5_acute_week_school_nc<-get_lme4_info_all_iterx_acute_nochr(final_impset_school,  y='mdt5_trafo', xvars_week)

tables_acute_week_school_nc<-list(table_mdt1_acute_week_school_nc,table_mdt2_acute_week_school_nc,
                               table_mdt3_acute_week_school_nc,table_mdt4_acute_week_school_nc,
                               table_mdt5_acute_week_school_nc)

write.xlsx(tables_acute_week_school_nc, file =  'results/tables_acute_week_school_nochronic.xlsx')

#minimally adjusted
f_base <- paste("~",  paste0(xvars_week, "+", x2, "+"), paste0(covars_week_min, collapse = " + "))

table_mdt1_acute_week_school_min<-get_lme4_info_all_iterx_acute_nochr(final_impset_school,  y='mdt1_trafo', xvars_week)
table_mdt2_acute_week_school_min<-get_lme4_info_all_iterx_acute_nochr(final_impset_school,  y='mdt2_trafo', xvars_week)
table_mdt3_acute_week_school_min<-get_lme4_info_all_iterx_acute_nochr(final_impset_school,  y='mdt3_trafo', xvars_week)
table_mdt4_acute_week_school_min<-get_lme4_info_all_iterx_acute_nochr(final_impset_school,  y='mdt4_trafo', xvars_week)
table_mdt5_acute_week_school_min<-get_lme4_info_all_iterx_acute_nochr(final_impset_school,  y='mdt5_trafo', xvars_week)

tables_acute_week_school_min<-list(table_mdt1_acute_week_school_min,table_mdt2_acute_week_school_min,
                                  table_mdt3_acute_week_school_min,table_mdt4_acute_week_school_min,
                                  table_mdt5_acute_week_school_min)

write.xlsx(tables_acute_week_school_min, file =  'results/tables_acute_week_school_minimallyAdj.xlsx')


#residuals and vif

for (y in ys){
  for (i in seq_along(f_base)){ 
    f <- paste(y,f_base[i], "+",xvars_child[i],"+" ,"(1 |idc)")
    fmFull <- lmer(as.formula(f), final25school, weights = final25school$weights_avg)
    qqnorm(resid(fmFull), main = xvars_week[i])
    print(vif(fmFull))
  }
}

for (y in ys){
  for (i in seq_along(f_base)){ 
    f <- paste(y,f_base[i], "+","(1 |idc)")
    fmFull <- lmer(as.formula(f), final25school, weights = final25school$weights_avg)
    qqnorm(resid(fmFull), main = xvars_week[i])
    print(vif(fmFull))
  }
}

#summary tables
labels<-c("exposures", "MDT1", "MDT2", "MDT3", "MDT4", "MDT5")

acuteweek_table <- data.frame(table_mdt1_acute_week$xs1, table_mdt1_acute_week$cis, table_mdt2_acute_week$cis, 
                              table_mdt3_acute_week$cis, table_mdt4_acute_week$cis, table_mdt5_acute_week$cis)
colnames(acuteweek_table)<-c(labels)

acuteweek_school_table <- data.frame(table_mdt1_acute_week_school$xs1, table_mdt1_acute_week_school$cis, table_mdt2_acute_week_school$cis, 
                              table_mdt3_acute_week_school$cis, table_mdt4_acute_week_school$cis, table_mdt5_acute_week_school$cis)
colnames(acuteweek_school_table)<-c(labels)

week_table_final = rbind(acuteweek_table[1,],acuteweek_school_table[1,], acuteweek_table[2,],acuteweek_school_table[2,],
                         acuteweek_table[3,],acuteweek_school_table[3,],acuteweek_table[5,],acuteweek_school_table[4,],
                         acuteweek_table[6,],acuteweek_school_table[5,],acuteweek_table[4,])

write.xlsx(week_table_final, file =  'results/summary_table_acute_week_home_school.xlsx')


acuteweek_table_nochr <- data.frame(table_mdt1_acute_week_nc$xs1, table_mdt1_acute_week_nc$cis, table_mdt2_acute_week_nc$cis, 
                              table_mdt3_acute_week_nc$cis, table_mdt4_acute_week_nc$cis, table_mdt5_acute_week_nc$cis)
colnames(acuteweek_table_nochr)<-c(labels)

acuteweek_school_table_nochr <- data.frame(table_mdt1_acute_week_school_nc$xs1, table_mdt1_acute_week_school_nc$cis, table_mdt2_acute_week_school_nc$cis, 
                                     table_mdt3_acute_week_school_nc$cis, table_mdt4_acute_week_school_nc$cis, table_mdt5_acute_week_school_nc$cis)
colnames(acuteweek_school_table_nochr)<-c(labels)

week_table_final_nochr = rbind(acuteweek_table_nochr[1,],acuteweek_school_table_nochr[1,], acuteweek_table_nochr[2,],acuteweek_school_table_nochr[2,],
                         acuteweek_table_nochr[3,],acuteweek_school_table_nochr[3,],acuteweek_table_nochr[5,],acuteweek_school_table_nochr[4,],
                         acuteweek_table_nochr[6,],acuteweek_school_table_nochr[5,],acuteweek_table_nochr[4,])

write.xlsx(week_table_final_nochr, file =  'results/summary_table_acute_week_home_school_nochr.xlsx')



acuteweek_table_min <- data.frame(table_mdt1_acute_week_min$xs1, table_mdt1_acute_week_min$cis, table_mdt2_acute_week_min$cis, 
                                    table_mdt3_acute_week_min$cis, table_mdt4_acute_week_min$cis, table_mdt5_acute_week_min$cis)
colnames(acuteweek_table_min)<-c(labels)

acuteweek_school_table_min <- data.frame(table_mdt1_acute_week_school_min$xs1, table_mdt1_acute_week_school_min$cis, table_mdt2_acute_week_school_min$cis, 
                                           table_mdt3_acute_week_school_min$cis, table_mdt4_acute_week_school_min$cis, table_mdt5_acute_week_school_min$cis)
colnames(acuteweek_school_table_min)<-c(labels)

week_table_final_min = rbind(acuteweek_table_min[1,],acuteweek_school_table_min[1,], acuteweek_table_min[2,],acuteweek_school_table_min[2,],
                               acuteweek_table_min[3,],acuteweek_school_table_min[3,],acuteweek_table_min[5,],acuteweek_school_table_min[4,],
                               acuteweek_table_min[6,],acuteweek_school_table_min[5,],acuteweek_table_min[4,])

write.xlsx(week_table_final_min, file =  'results/summary_table_acute_week_home_school_minimallyAdj.xlsx')


#Determine new p values for outcome multiple testing correction 

#Calculate correlations on outcomes
mydata.cor = stats::cor(dataset_metrics[, c("mdt1_trafo","mdt2_trafo","mdt3_trafo","mdt4_trafo","mdt5_trafo")],  method ="spearman",  use = "pairwise.complete.obs")

# effective number of tests
alpha = 0.05
evals = eigen(mydata.cor)$values
#formula from Galway 2009 http://www.ncbi.nlm.nih.gov/pubmed/19217024
Meff = ((sum(sqrt(evals)))^2) / sum(evals)
#provide the new pvalue to achieve alpha 0.05
sidak = 1 - ((1 - alpha)^(1/Meff))

#adjusted p value
#0.011

for(i in 1:5)mydata.cor[i,i]<-1
meff(mydata.cor, method = "galwey")
sidak1 = 1 - ((1 - alpha)^(1/4))
#adjusted p value
#0.013




