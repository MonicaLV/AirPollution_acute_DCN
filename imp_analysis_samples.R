#--------------------------------------------------------------------------#
# Acute air pollution and DFNC                                             #
# Author: Monica Lopez                                                     #
# Date: 23/9/2024                                                          #
# Description: Multiple Imputation analysis Cohorts                        #
# Changes (programmer/date):                                               #
#--------------------------------------------------------------------------#

#R version 4.3.1

setwd("V:/medewerkers/529028 Lopez Vicente, M/AP")

source("analyses/final_acute/setUp.R")

#reshape dataset_metrics to wide

vnames = c("mdt1_trafo", "mdt2_trafo", "mdt3_trafo", "mdt4_trafo", "mdt5_trafo", "agemri", "agemri_dm" , "num",
           "no2_week","nox_week","pm25_week","pm25abs_week","pm10_week","pmcoarse_week",
           "no2_week_schools","nox_week_schools","pm25_week_schools","pm10_week_schools","pmcoarse_week_schools",
           "time_factor","season_Lag0","TG_week","HU_week", "weekday")
idVar = 'idc'
timeVar = 'ses'

dataset_wide <- reshape(dataset_metrics, idvar=idVar, v.names = vnames ,timevar=timeVar, direction='wide')


#calculate mean repeated measures 
#if only one measure, keep it 


dataset_wide$mdt1_trafo<-(dataset_wide$mdt1_trafo.F09+dataset_wide$mdt1_trafo.F13)/2
dataset_wide$mdt1_trafo<- ifelse(is.na(dataset_wide$mdt1_trafo.F09), dataset_wide$mdt1_trafo.F13, dataset_wide$mdt1_trafo)
dataset_wide$mdt1_trafo<- ifelse(is.na(dataset_wide$mdt1_trafo.F13), dataset_wide$mdt1_trafo.F09, dataset_wide$mdt1_trafo)

dataset_wide$mdt2_trafo<-(dataset_wide$mdt2_trafo.F09+dataset_wide$mdt2_trafo.F13)/2
dataset_wide$mdt2_trafo<- ifelse(is.na(dataset_wide$mdt2_trafo.F09), dataset_wide$mdt2_trafo.F13, dataset_wide$mdt2_trafo)
dataset_wide$mdt2_trafo<- ifelse(is.na(dataset_wide$mdt2_trafo.F13), dataset_wide$mdt2_trafo.F09, dataset_wide$mdt2_trafo)

dataset_wide$mdt3_trafo<-(dataset_wide$mdt3_trafo.F09+dataset_wide$mdt3_trafo.F13)/2
dataset_wide$mdt3_trafo<- ifelse(is.na(dataset_wide$mdt3_trafo.F09), dataset_wide$mdt3_trafo.F13, dataset_wide$mdt3_trafo)
dataset_wide$mdt3_trafo<- ifelse(is.na(dataset_wide$mdt3_trafo.F13), dataset_wide$mdt3_trafo.F09, dataset_wide$mdt3_trafo)

dataset_wide$mdt4_trafo<-(dataset_wide$mdt4_trafo.F09+dataset_wide$mdt4_trafo.F13)/2
dataset_wide$mdt4_trafo<- ifelse(is.na(dataset_wide$mdt4_trafo.F09), dataset_wide$mdt4_trafo.F13, dataset_wide$mdt4_trafo)
dataset_wide$mdt4_trafo<- ifelse(is.na(dataset_wide$mdt4_trafo.F13), dataset_wide$mdt4_trafo.F09, dataset_wide$mdt4_trafo)

dataset_wide$mdt5_trafo<-(dataset_wide$mdt5_trafo.F09+dataset_wide$mdt5_trafo.F13)/2
dataset_wide$mdt5_trafo<- ifelse(is.na(dataset_wide$mdt5_trafo.F09), dataset_wide$mdt5_trafo.F13, dataset_wide$mdt5_trafo)
dataset_wide$mdt5_trafo<- ifelse(is.na(dataset_wide$mdt5_trafo.F13), dataset_wide$mdt5_trafo.F09, dataset_wide$mdt5_trafo)


#make 2 samples: home and school analyses

dataset_wide_home<-dataset_wide[which(dataset_wide$incl_var==1),]

dataset_wide_school<-dataset_wide[which(dataset_wide$incl_var_school==1),]

#HOME

# select variables

impVar <- c("idc", 
            #outcomes
            "mdt1_trafo",  "mdt2_trafo", "mdt3_trafo", "mdt4_trafo", "mdt5_trafo",
            #stable covariates
            "ethnmv2_6cat", "ethnfv2_6cat", "GENDER", "MARDICH", "season",
            "educm_3cat","educp_3cat", "mdrink_updated", 'income_4cat',  "SMOKE_ALL", "FOLIUM_VALIDATED", "parity_3cat",
            'AGE_M_v2',"AGE_BF_v2",'BMI_0', 'BMI_P',  "APM_IQ", "ndvi300_preg", "STATUSSCORE", 
            #exposures childhood
            "no2","nox","pm25", "pm25abs" , "pm10","pmcoarse",
            #changing covariates
            "agemri.F09","agemri.F13",  "season_Lag0.F09","season_Lag0.F13", "time_factor.F09","time_factor.F13", 
            "weekday.F09", "weekday.F13","TG_week.F09","HU_week.F09","TG_week.F13","HU_week.F13",
            #acute exposures
            "no2_week.F09","nox_week.F09","pm25_week.F09","pm25abs_week.F09","pm10_week.F09","pmcoarse_week.F09",
            "no2_week.F13","nox_week.F13","pm25_week.F13","pm25abs_week.F13","pm10_week.F13","pmcoarse_week.F13")


cov <- dataset_wide_home[impVar]


# Missing data evaluation ------------------------------------------------------
summary(cov)
nrow(cov[complete.cases(cov),])              # 370 subjects with no missing information
sum(is.na(cov))                              # 39866 total missing values
sort(sapply(cov, function(x) sum(is.na(x)))) # Most missing in paternal variables (educp)

cov_NA         <- data.frame(matrix(round(sapply(cov, function(x) sum(is.na(x)))), dimnames = list(names(cov), "Absolute frequency")))
cov_NA$percent <- round(sapply(cov, function(x) sum(is.na(x))) / nrow(cov) * 100, 1)
cov_NA$MCAR    <- ifelse(cov_NA[, 2] < 5, "Yes", "No") # Yes = Missings at random because <5% 
cov_NA$Exclude <- ifelse(cov_NA[, 2] >35, "Yes", "No") # We exclude educp in the imputation due to > 35% missing
cov_NA

# Exclude the above variables with > 35% missing
cov$educp_3cat <- NULL

# --- Dry imputation ------------------------------------------------------
# Dry imputation to obtain predictor matrix
# We use the quickpred function to define the predictor matrix. 
# minpuc = minimum proportion of usable cases. It should be between 0.3-0.4, we decide considering the missing evaluation previously done
# mincor = minimum correlation to be used as predictors (default is 0.1)
# exc = variables excluded as predictors (id, filter)

data_full <- cov
ini       <- mice(data_full, m = 1, maxit = 0, print = T,seed = 9304,
                  pred=quickpred(data_full, mincor = 0.06, minpuc = 0.3, exc = c("idc"))) 

# --- Actual imputation ---------------------------------------------------
# First identify the method mat"rix to be able to edit it
# You want to exclude any ID variables or those with no missing values, usually mice does this automatically
meth = ini$meth             
# Check
meth

# We check the number of predictors. Ideally, we should have between 5 and 25 predictors per variable. 
# Values for mincor and minpuc can be tuned to have a number of predictors between 5 and 25 
pred = ini$pred
table(apply(pred,1,sum))
pred


# --- Run the imputation -------------------------------------------------
imp_full      <- mice(data_full, m = 25, meth = meth, pred = pred, max = 30, seed = 9304)

save(imp_full,      file = "results/imputedmids_analysis_home.RData")


imp_full$loggedEvents #some variables (out) were excluded in the imputation due to collinearity or other reasons

imp_full_long <- complete(imp_full, "long", include = T)

# --- Check --------------------------------------------------------------
# Missing (won't be 0 because we've included our original .imp 0 dataset)
sapply(colnames(imp_full_long), function(x)length(which(is.na(imp_full_long[,x])))) %>% max
nrow(imp_full_long[complete.cases(imp_full_long)&imp_full_long$.imp==1,c(4:50)]) 

imp1_full<-imp_full_long[which(imp_full_long$.imp==1),]
imp0_full<-imp_full_long[which(imp_full_long$.imp==0),]
summary(imp1_full)
summary(imp0_full)

# Convergence
pdf("results/convergence_analysis_home.pdf")
plot(imp_full)
dev.off()

# Density plot for the continuous variables
pdf("results/density_analysis_home.pdf")
densityplot(imp_full, layout = c(2,4))
dev.off()

# --- Finalize ----------------------------------------------------------

colnames(imp_full_long)



#SCHOOL

# select variables

impVar <- c("idc", 
            #outcomes
            "mdt1_trafo",  "mdt2_trafo", "mdt3_trafo", "mdt4_trafo", "mdt5_trafo",
            #stable covariates
            "ethnmv2_6cat", "ethnfv2_6cat", "GENDER", "MARDICH", "season",
            "educm_3cat","educp_3cat", "mdrink_updated", 'income_4cat',  "SMOKE_ALL", "FOLIUM_VALIDATED", "parity_3cat",
            'AGE_M_v2',"AGE_BF_v2",'BMI_0', 'BMI_P',  "APM_IQ", "ndvi300_preg", "STATUSSCORE", 
            #exposures childhood
            "no2","nox","pm25", "pm25abs" , "pm10","pmcoarse",
            #changing covariates
            "agemri.F09","agemri.F13",  "season_Lag0.F09","season_Lag0.F13", "time_factor.F09","time_factor.F13", 
            "weekday.F09", "weekday.F13","TG_week.F09","HU_week.F09","TG_week.F13","HU_week.F13",
            #acute exposures
            "no2_week_schools.F09","nox_week_schools.F09","pm25_week_schools.F09","pm10_week_schools.F09","pmcoarse_week_schools.F09",
            "no2_week_schools.F13","nox_week_schools.F13","pm25_week_schools.F13","pm10_week_schools.F13","pmcoarse_week_schools.F13")


cov <- dataset_wide_school[impVar]

# Missing data evaluation ------------------------------------------------------
summary(cov)
nrow(cov[complete.cases(cov),])              # 188 subjects with no missing information
sum(is.na(cov))                              # 23782 total missing values
sort(sapply(cov, function(x) sum(is.na(x)))) # Most missing in paternal variables (educp)

cov_NA         <- data.frame(matrix(round(sapply(cov, function(x) sum(is.na(x)))), dimnames = list(names(cov), "Absolute frequency")))
cov_NA$percent <- round(sapply(cov, function(x) sum(is.na(x))) / nrow(cov) * 100, 1)
cov_NA$MCAR    <- ifelse(cov_NA[, 2] < 5, "Yes", "No") # Yes = Missings at random because <5% 
cov_NA$Exclude <- ifelse(cov_NA[, 2] >35, "Yes", "No") # We exclude educp in the imputation due to > 35% missing
cov_NA

# Exclude the above variables with > 35% missing
cov$educp_3cat <- NULL

# --- Dry imputation ------------------------------------------------------
# Dry imputation to obtain predictor matrix
# We use the quickpred function to define the predictor matrix. 
# minpuc = minimum proportion of usable cases. It should be between 0.3-0.4, we decide considering the missing evaluation previously done
# mincor = minimum correlation to be used as predictors (default is 0.1)
# exc = variables excluded as predictors (id, filter)

data_full <- cov
ini       <- mice(data_full, m = 1, maxit = 0, print = T,seed = 9304,
                  pred=quickpred(data_full, mincor = 0.06, minpuc = 0.3, exc = c("idc"))) 

# --- Actual imputation ---------------------------------------------------
# First identify the method mat"rix to be able to edit it
# You want to exclude any ID variables or those with no missing values, usually mice does this automatically
meth = ini$meth             
# Check
meth

# We check the number of predictors. Ideally, we should have between 5 and 25 predictors per variable. 
# Values for mincor and minpuc can be tuned to have a number of predictors between 5 and 25 
pred = ini$pred
table(apply(pred,1,sum))
pred


# --- Run the imputation -------------------------------------------------
imp_full      <- mice(data_full, m = 25, meth = meth, pred = pred, max = 30, seed = 9304)

save(imp_full,      file = "results/imputedmids_analysis_school.RData")


imp_full$loggedEvents #some variables (out) were excluded in the imputation due to collinearity or other reasons

imp_full_long <- complete(imp_full, "long", include = T)

# --- Check --------------------------------------------------------------
# Missing (won't be 0 because we've included our original .imp 0 dataset)
sapply(colnames(imp_full_long), function(x)length(which(is.na(imp_full_long[,x])))) %>% max
nrow(imp_full_long[complete.cases(imp_full_long)&imp_full_long$.imp==1,c(4:50)]) 

imp1_full<-imp_full_long[which(imp_full_long$.imp==1),]
imp0_full<-imp_full_long[which(imp_full_long$.imp==0),]
summary(imp1_full)
summary(imp0_full)

# Convergence
pdf("results/convergence_analysis_school.pdf")
plot(imp_full)
dev.off()

# Density plot for the continuous variables
pdf("results/density_analysis_school.pdf")
densityplot(imp_full, layout = c(2,4))
dev.off()

# --- Finalize ----------------------------------------------------------

colnames(imp_full_long)



