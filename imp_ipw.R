
#--------------------------------------------------------------------------#
# Acute air pollution and DFNC                                             #
# Author: Monica Lopez                                                     #
# Date: 19/9/2024                                                          #
# Description: Multiple Imputation Full Cohort, IPW                        #
# Changes (programmer/date):                                               #
#--------------------------------------------------------------------------#

#R version 4.3.1

# Load packages

# Load R libraries -------------------------------------------------------------------------------------------------------------
list.of.packages <- c("mice",      # needed for imputation
                      "miceadds",
                      "mitools",
                      "CBPS",      # for creating prediction of inclusion model CBPS
                      "dplyr",
                      "data.table",
                      "dplyr",
                      "ggplot2",
                      "xlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require,character.only = T) 


# Load data (full cohort)

setwd("V:/medewerkers/529028 Lopez Vicente, M/AP")

source("analyses/final_acute/setUp.R")

cov<-dataset_notwins[c("IDC", "incl_var","incl_var_school","GENDER","educm_3cat","educp_3cat","ethnmv2_6cat", "ethnfv2_6cat", "MARDICH", 'income_4cat', 
                       "SMOKE_ALL", "mdrink_updated", "FOLIUM_VALIDATED", "parity_3cat","season",
                       'AGE_M_v2',"AGE_BF_v2",'BMI_0', 'BMI_P',  "APM_IQ", "ndvi300_preg", "STATUSSCORE",  "NO2_preg", "PM25_preg")]

# -------------------------------------------------------------------------------
# FULL COHORT IMPUTATION 
# -------------------------------------------------------------------------------

# Missing data evaluation ------------------------------------------------------
summary(cov)
nrow(cov[complete.cases(cov),])              # 2103 subjects with no missing information
sum(is.na(cov))                              # 32585 total missing values
sort(sapply(cov, function(x) sum(is.na(x)))) # Most missing in paternal variables (educp)

cov_NA         <- data.frame(matrix(round(sapply(cov, function(x) sum(is.na(x)))), dimnames = list(names(cov), "Absolute frequency")))
cov_NA$percent <- round(sapply(cov, function(x) sum(is.na(x))) / nrow(cov) * 100, 1)
cov_NA$MCAR    <- ifelse(cov_NA[, 2] < 5, "Yes", "No") # Yes = Missings at random because <5% 
cov_NA$Exclude <- ifelse(cov_NA[, 2] >35, "Yes", "No") # We exclude educp in the imputation due to > 35% missing
cov_NA

cov<-cov[c("IDC", "incl_var","incl_var_school","GENDER","educm_3cat","ethnmv2_6cat", "ethnfv2_6cat", "MARDICH", 'income_4cat', 
                       "SMOKE_ALL", "mdrink_updated", "FOLIUM_VALIDATED", "parity_3cat","season",
                       'AGE_M_v2',"AGE_BF_v2",'BMI_0', 'BMI_P',  "APM_IQ", "ndvi300_preg", "STATUSSCORE",  "NO2_preg", "PM25_preg")]


# --- Dry imputation ------------------------------------------------------
# Dry imputation to obtain predictor matrix
# We use the quickpred function to define the predictor matrix. 
# minpuc = minimum proportion of usable cases. It should be between 0.3-0.4, we decide considering the missing evaluation previously done
# mincor = minimum correlation to be used as predictors (default is 0.1)
# exc = variables excluded as predictors (id, filter)

data_full <- cov
ini       <- mice(data_full, m = 1, maxit = 0, print = T,seed = 9304,
                  pred=quickpred(data_full, mincor = 0.02, minpuc = 0.3, exc = c("IDC", "incl_var", "incl_var_school"))) 

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

save(imp_full,      file = "results/imputedmids_fullcohort.RData")

imp_full_long <- complete(imp_full, "long", include = T)

# --- Check --------------------------------------------------------------
# Missing (won't be 0 because we've included our original .imp 0 dataset)
sapply(colnames(imp_full_long), function(x)length(which(is.na(imp_full_long[,x])))) %>% max
nrow(imp_full_long[complete.cases(imp_full_long)&imp_full_long$.imp==1,c(6:22)]) 

imp1_full<-imp_full_long[which(imp_full_long$.imp==1),]
imp0_full<-imp_full_long[which(imp_full_long$.imp==0),]
summary(imp1_full)
summary(imp0_full)

# Convergence
pdf("results/convergence_fullcohort.pdf")
plot(imp_full)
dev.off()

# Density plot for the continuous variables
pdf("results/density_fullcohort.pdf")
densityplot(imp_full, layout = c(2,4))
dev.off()

# --- Finalize ----------------------------------------------------------

colnames(imp_full_long)

# -------------------------------------------------------------------------------
# INVERSE PROBABILITY WEIGHTING - HOME
# -------------------------------------------------------------------------------
load("results/imputedmids_fullcohort.RData")
imp_full_long <- complete(imp_full, "long", include = T)

#calculate different weights for different visits
incl_vars<-dataset_notwins[c("IDC", "complete9","complete13","complete9_sch","complete13_sch")]

incl_vars$complete9[is.na(incl_vars$complete9)]<-0
incl_vars$complete13[is.na(incl_vars$complete13)]<-0
incl_vars$complete9_sch[is.na(incl_vars$complete9_sch)]<-0
incl_vars$complete13_sch[is.na(incl_vars$complete13_sch)]<-0

incl_vars$complete9<-as.factor(incl_vars$complete9)
incl_vars$complete13<-as.factor(incl_vars$complete13)
incl_vars$complete9_sch<-as.factor(incl_vars$complete9_sch)
incl_vars$complete13_sch<-as.factor(incl_vars$complete13_sch)



imp_full_long1<-merge(imp_full_long,incl_vars, by="IDC")

## Identify your inclusion variable ----
incl9 <- 'complete9'
incl13 <- 'complete13'
incl9sch <- 'complete9_sch'
incl13sch <- 'complete13_sch'

## Identify your predictors used for IPW (decided a priori) ----
pred <-c("GENDER","educm_3cat","ethnmv2_6cat", "ethnfv2_6cat", "MARDICH", 'income_4cat', 
         "SMOKE_ALL", "mdrink_updated", "FOLIUM_VALIDATED", "parity_3cat","season",
         'AGE_M_v2',"AGE_BF_v2",'BMI_0', 'BMI_P',  "APM_IQ", "ndvi300_preg", "STATUSSCORE",  "NO2_preg", "PM25_preg")


## Create a formula ----
fit_eq9 <- reformulate(termlabels = pred, response = incl9); fit_eq9
fit_eq13 <- reformulate(termlabels = pred, response = incl13); fit_eq13
fit_eq9sch <- reformulate(termlabels = pred, response = incl9sch); fit_eq9sch
fit_eq13sch <- reformulate(termlabels = pred, response = incl13sch); fit_eq13sch

## Create an empty list to store the propensity scores ----
ps9 <- list()
ps13 <- list()
ps9sch <- list()
ps13sch <- list()

##' Run a for-loop to run each CBPS model over each imputed set and get the propensity scores ----
##' Store in pdf the plots of absolute difference in standardized means before and after weighting
##' Recommended < 0.1 (after plot). Otherwise, we should check the variables included in the model.

pdf("results/CBPS_balance_2_home9.pdf")

for(imp in 1:25) {
  set.seed(9304)
  imp_full_long2 <- imp_full_long1[which(imp_full_long1$.imp == imp),]
  fit.imp       <- CBPS(fit_eq9, data = imp_full_long2)
  plot(fit.imp)
  ps9[[imp]]     <- fit.imp$fitted.values
}

dev.off()

pdf("results/CBPS_balance_2_home13.pdf")

for(imp in 1:25) {
  set.seed(9304)
  imp_full_long2 <- imp_full_long1[which(imp_full_long1$.imp == imp),]
  fit.imp       <- CBPS(fit_eq13, data = imp_full_long2)
  plot(fit.imp)
  ps13[[imp]]     <- fit.imp$fitted.values
}

dev.off()

pdf("results/CBPS_balance_2_school9.pdf")

for(imp in 1:25) {
  set.seed(9304)
  imp_full_long2 <- imp_full_long1[which(imp_full_long1$.imp == imp),]
  fit.imp       <- CBPS(fit_eq9sch, data = imp_full_long2)
  plot(fit.imp)
  ps9sch[[imp]]     <- fit.imp$fitted.values
}

dev.off()

pdf("results/CBPS_balance_2_school13.pdf")

for(imp in 1:25) {
  set.seed(9304)
  imp_full_long2 <- imp_full_long1[which(imp_full_long1$.imp == imp),]
  fit.imp       <- CBPS(fit_eq13sch, data = imp_full_long2)
  plot(fit.imp)
  ps13sch[[imp]]     <- fit.imp$fitted.values
}

dev.off()

## Use the long format to paste the propensity scores into the imputed datasets ----
long  <- imp_full_long1[which(imp_full_long1$.imp > 0),] 

## Get list of the PS as follows: ----
ps9 = unlist(ps9) %>% as.data.frame
colnames(ps9) = c("ps9")

drawhist(data     = ps9, 
         fname    = "results/hist_ps_home9.jpg", 
         xlabel   = ps9$ps9,
         xaxis    = "Propensity Scores",
         binwidth = 0.005,
         xlab     = pretty(ps9$ps9, n = 10))

## Get list of the PS as follows: ----
ps13 = unlist(ps13) %>% as.data.frame
colnames(ps13) = c("ps13")

drawhist(data     = ps13, 
         fname    = "results/hist_ps_home13.jpg", 
         xlabel   = ps13$ps13,
         xaxis    = "Propensity Scores",
         binwidth = 0.005,
         xlab     = pretty(ps13$ps13, n = 10))

ps9sch = unlist(ps9sch) %>% as.data.frame
colnames(ps9sch) = c("ps9sch")

drawhist(data     = ps9sch, 
         fname    = "results/hist_ps_school9.jpg", 
         xlabel   = ps9sch$ps9sch,
         xaxis    = "Propensity Scores",
         binwidth = 0.005,
         xlab     = pretty(ps9sch$ps9sch, n = 10))

## Get list of the PS as follows: ----
ps13sch = unlist(ps13sch) %>% as.data.frame
colnames(ps13sch) = c("ps13sch")

drawhist(data     = ps13sch, 
         fname    = "results/hist_ps_school13.jpg", 
         xlabel   = ps13sch$ps13sch,
         xaxis    = "Propensity Scores",
         binwidth = 0.005,
         xlab     = pretty(ps13sch$ps13sch, n = 10))

## Get list of the .imp as follows: ----
imp = rep(1:25, each = 9607) %>% as.data.frame # 9607 is the sample size
colnames(imp) = c(".imp")

## Get list of the idnum as follows: ----
one_set = long[which(long$.imp == 1),]
idnum_df = rep(one_set$IDC, 25) %>% as.data.frame
colnames(idnum_df) = c("IDC")

## Create the dataframe with the PS: ----
df = cbind(idnum_df, imp, ps9, ps13, ps9sch, ps13sch)

## Create the final dataframe with all variables: ----
final_imp_full <- merge(long, df, by = c("IDC", ".imp"))
final_imp_full<-final_imp_full[order(final_imp_full$IDC, final_imp_full$.imp), ]

table(as.factor(final_imp_full$.imp))

# 3. Calculate the weights from the propensity scores (taking the inverse of the probability)
final_imp_full <- mutate(final_imp_full, weights9 = ifelse(get(incl9) == 1, (1/ps9), (1 / (1-ps9))))
final_imp_full <- mutate(final_imp_full, weights13 = ifelse(get(incl13) == 1, (1/ps13), (1 / (1-ps13))))
final_imp_full <- mutate(final_imp_full, weights9sch = ifelse(get(incl9sch) == 1, (1/ps9sch), (1 / (1-ps9sch))))
final_imp_full <- mutate(final_imp_full, weights13sch = ifelse(get(incl13sch) == 1, (1/ps13sch), (1 / (1-ps13sch))))


# 4. Check weights for large values (ex: larger than 10) - winsorize if necessary
# Check the distribution of the propensity scores

# Check the distribution of the weights
drawhist(data     = final_imp_full, 
         fname    = "results/hist_weights_home9.png", 
         xlabel   = final_imp_full$weights9,
         xaxis    = "Weights9",
         binwidth = 0.05,
         xlab     = pretty(final_imp_full$weights9, n = 10))

summary(final_imp_full$weights9)
sum(final_imp_full$weights9 >= 10, na.rm = T)

drawhist(data     = final_imp_full, 
         fname    = "results/hist_weights_home13.png", 
         xlabel   = final_imp_full$weights13,
         xaxis    = "Weights13",
         binwidth = 0.05,
         xlab     = pretty(final_imp_full$weights13, n = 10))

summary(final_imp_full$weights13)
sum(final_imp_full$weights13 >= 10, na.rm = T)

drawhist(data     = final_imp_full, 
         fname    = "results/hist_weights_school9.png", 
         xlabel   = final_imp_full$weights9sch,
         xaxis    = "Weights9sch",
         binwidth = 0.05,
         xlab     = pretty(final_imp_full$weights9sch, n = 10))

summary(final_imp_full$weights9sch)
sum(final_imp_full$weights9sch >= 10, na.rm = T)

drawhist(data     = final_imp_full, 
         fname    = "results/hist_weights_school13.png", 
         xlabel   = final_imp_full$weights13sch,
         xaxis    = "Weights13sch",
         binwidth = 0.05,
         xlab     = pretty(final_imp_full$weights13sch, n = 10))

summary(final_imp_full$weights13sch)
sum(final_imp_full$weights13sch >= 10, na.rm = T)

# winsorize to 10
final_imp_full$weights9 <- ifelse(final_imp_full$weights9 > 10, 10, final_imp_full$weights9)
final_imp_full$weights13 <- ifelse(final_imp_full$weights13 > 10, 10, final_imp_full$weights13)
final_imp_full$weights9sch <- ifelse(final_imp_full$weights9sch > 10, 10, final_imp_full$weights9sch)
final_imp_full$weights13sch <- ifelse(final_imp_full$weights13sch > 10, 10, final_imp_full$weights13sch)



# Check the distribution of the winsorized weights
drawhist(data     = final_imp_full, 
         fname    = "results/hist_weights_winsorize_10_home9.png", 
         xlabel   = final_imp_full$weights9,
         xaxis    = "Weights9",
         binwidth = 0.05,
         xlab     = pretty(final_imp_full$weights9, n = 10))

drawhist(data     = final_imp_full, 
         fname    = "results/hist_weights_winsorize_10_home13.png", 
         xlabel   = final_imp_full$weights13,
         xaxis    = "Weights13",
         binwidth = 0.05,
         xlab     = pretty(final_imp_full$weights13, n = 10))

drawhist(data     = final_imp_full, 
         fname    = "results/hist_weights_winsorize_10_school9.png", 
         xlabel   = final_imp_full$weights9sch,
         xaxis    = "Weights9sch",
         binwidth = 0.05,
         xlab     = pretty(final_imp_full$weights9sch, n = 10))

drawhist(data     = final_imp_full, 
         fname    = "results/hist_weights_winsorize_10_school13.png", 
         xlabel   = final_imp_full$weights13sch,
         xaxis    = "Weights13sch",
         binwidth = 0.05,
         xlab     = pretty(final_imp_full$weights13sch, n = 10))

# Average the winsorized weights

summary(final_imp_full$weights9)
summary(final_imp_full$weights13)
summary(final_imp_full$weights9sch)
summary(final_imp_full$weights13sch)

dim(final_imp_full)[1]/9607


df                      <- final_imp_full[,c("IDC",".imp","weights9")]
df_wide                 <- reshape(df, idvar = "IDC", timevar = ".imp", direction = "wide")
df_wide$weights_avg9     <- rowMeans(df_wide[,2:26])


# Check the distribution of the averaged winsorized weights
drawhist(data     = df_wide, 
         fname    = "results/hist_weights_average_home9.png", 
         xlabel   = df_wide$weights_avg9,
         xaxis    = "Weights9",
         binwidth = 0.05,
         xlab     = pretty(df_wide$weights_avg9, n = 10))


ipw9 <- df_wide[,c(1,27)]



df                      <- final_imp_full[,c("IDC",".imp","weights13")]
df_wide                 <- reshape(df, idvar = "IDC", timevar = ".imp", direction = "wide")
df_wide$weights_avg13     <- rowMeans(df_wide[,2:26])

# Check the distribution of the averaged winsorized weights
drawhist(data     = df_wide, 
         fname    = "results/hist_weights_average_home13.png", 
         xlabel   = df_wide$weights_avg13,
         xaxis    = "Weights13",
         binwidth = 0.05,
         xlab     = pretty(df_wide$weights_avg13, n = 10))


ipw13 <- df_wide[,c(1,27)]

df                      <- final_imp_full[,c("IDC",".imp","weights9sch")]
df_wide                 <- reshape(df, idvar = "IDC", timevar = ".imp", direction = "wide")
df_wide$weights_avg9sch     <- rowMeans(df_wide[,2:26])


# Check the distribution of the averaged winsorized weights
drawhist(data     = df_wide, 
         fname    = "results/hist_weights_average_school9.png", 
         xlabel   = df_wide$weights_avg9sch,
         xaxis    = "Weights9sch",
         binwidth = 0.05,
         xlab     = pretty(df_wide$weights_avg9sch, n = 10))


ipw9sch <- df_wide[,c(1,27)]



df                      <- final_imp_full[,c("IDC",".imp","weights13sch")]
df_wide                 <- reshape(df, idvar = "IDC", timevar = ".imp", direction = "wide")
df_wide$weights_avg13sch     <- rowMeans(df_wide[,2:26])

# Check the distribution of the averaged winsorized weights
drawhist(data     = df_wide, 
         fname    = "results/hist_weights_average_school13.png", 
         xlabel   = df_wide$weights_avg13sch,
         xaxis    = "Weights13sch",
         binwidth = 0.05,
         xlab     = pretty(df_wide$weights_avg13sch, n = 10))


ipw13sch <- df_wide[,c(1,27)]

# Save IDs and weights
save(ipw9, file = "results/weights_home9.RData")
save(ipw13, file = "results/weights_home13.RData")
save(ipw9sch, file = "results/weights_school9.RData")
save(ipw13sch, file = "results/weights_school13.RData")

