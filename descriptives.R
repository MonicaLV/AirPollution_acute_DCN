
#--------------------------------------------------------------------------#
# Acute Air pollution and DFNC                                             #
# Author: Monica Lopez                                                     #
# Date: 25/9/2024                                                          #
# Description: Descriptive tables and figures                              #
# Changes (programmer/date):                                               #
#--------------------------------------------------------------------------#


#---- INDEX ----------------------------------------------------------------                      
# 1) Flowchart
# 2) Sociodemographic characteristics of sample
# 3) Exposures descriptive
# 4) correlations exposures
#---------------------------------------------------------------------------

#R version 4.3.1

#cd
setwd("V:/medewerkers/529028 Lopez Vicente, M/AP")

#load packages
library(dplyr)
library(corrplot)
library(mice)

#load dataset (setUp)
source("analyses/final_acute/setUp.R")


#Flowchart 


#F09
#consent
table(dataset_notwins$mri_consent==1)
#3888 

#fmri
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1)#have fmri data
#3357 
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==0)
#531

# data available & number of volumes
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 & 
        dataset_notwins$nvolsf9==200) #correct number of volumes
#3267
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 & 
        dataset_notwins$nvolsf9!=200) #not enough number of volumes
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 & 
        is.na(dataset_notwins$nvolsf9)) # missing number of volumes
#73+17

# check all have motion data
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 & 
        dataset_notwins$nvolsf9==200 & !is.na(dataset_notwins$mean_rms9))
#3267
# tsv file
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 & 
        dataset_notwins$nvolsf9==200 & dataset_notwins$tsvdataf9==1) #have tsv file
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 & 
        dataset_notwins$nvolsf9==200 & is.na(dataset_notwins$tsvdataf9)) #don't have tsv file
#1

#no braces
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 & 
        dataset_notwins$nvolsf9==200 & dataset_notwins$tsvdataf9==1 & dataset_notwins$braces_mri_f9==0) #no braces
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 & 
        dataset_notwins$nvolsf9==200 & dataset_notwins$tsvdataf9==1 & dataset_notwins$braces_mri_f9==1) #braces
#26

#to exclude
531+73+17+1+26


#incidental findings
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 & 
        dataset_notwins$nvolsf9==200 & dataset_notwins$tsvdataf9==1 & dataset_notwins$braces_mri_f9==0 & dataset_notwins$exclude_incidental==0) #no incidental finding
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 & 
        dataset_notwins$nvolsf9==200 & dataset_notwins$tsvdataf9==1 & dataset_notwins$braces_mri_f9==0 & dataset_notwins$exclude_incidental==1) #incidental finding

#rms
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 & 
        dataset_notwins$nvolsf9==200 & dataset_notwins$tsvdataf9==1 & dataset_notwins$braces_mri_f9==0 & dataset_notwins$exclude_incidental==0 & 
        dataset_notwins$mean_rms9<=0.25) #low motion
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 & 
        dataset_notwins$nvolsf9==200 & dataset_notwins$tsvdataf9==1 & dataset_notwins$braces_mri_f9==0 & dataset_notwins$exclude_incidental==0 & 
        dataset_notwins$mean_rms9>0.25) #high motion

table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 & 
        dataset_notwins$nvolsf9==200 & dataset_notwins$tsvdataf9==1 & dataset_notwins$braces_mri_f9==0 & dataset_notwins$exclude_incidental==0 & 
        dataset_notwins$mean_rms9<=0.25 & dataset_notwins$rms_vols_bin9==0) #less than 20% of the volumes with high motion
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 & 
        dataset_notwins$nvolsf9==200 & dataset_notwins$tsvdataf9==1 & dataset_notwins$braces_mri_f9==0 & dataset_notwins$exclude_incidental==0 & 
        dataset_notwins$mean_rms9<=0.25 & dataset_notwins$rms_vols_bin9==1) #more than 20% of the volumes with high motion

#to exclude
389+294

#registration 
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 & 
        dataset_notwins$nvolsf9==200 & dataset_notwins$tsvdataf9==1 & dataset_notwins$braces_mri_f9==0 & dataset_notwins$exclude_incidental==0 & 
        dataset_notwins$mean_rms9<=0.25 & dataset_notwins$rms_vols_bin9==0 & is.na(dataset_notwins$exclude_reg_f9)) #no registration problems
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 & 
        dataset_notwins$nvolsf9==200 & dataset_notwins$tsvdataf9==1 & dataset_notwins$braces_mri_f9==0 & dataset_notwins$exclude_incidental==0 & 
        dataset_notwins$mean_rms9<=0.25 & dataset_notwins$rms_vols_bin9==0 & dataset_notwins$exclude_reg_f9==1) #registration problem


#air pollution acute home
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 &
        dataset_notwins$nvolsf9==200 & dataset_notwins$tsvdataf9==1 & dataset_notwins$braces_mri_f9==0 & dataset_notwins$exclude_incidental==0 &
        dataset_notwins$mean_rms9<=0.25 & dataset_notwins$rms_vols_bin9==0 & is.na(dataset_notwins$exclude_reg_f9) & !is.na(dataset_notwins$no2_week.F09))
#2475 
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 &
        dataset_notwins$nvolsf9==200 & dataset_notwins$tsvdataf9==1 & dataset_notwins$braces_mri_f9==0 & dataset_notwins$exclude_incidental==0 &
        dataset_notwins$mean_rms9<=0.25 & dataset_notwins$rms_vols_bin9==0 & is.na(dataset_notwins$exclude_reg_f9) & is.na(dataset_notwins$no2_week.F09))
#41

#air pollution acute school
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 &
        dataset_notwins$nvolsf9==200 & dataset_notwins$tsvdataf9==1 & dataset_notwins$braces_mri_f9==0 & dataset_notwins$exclude_incidental==0 &
        dataset_notwins$mean_rms9<=0.25 & dataset_notwins$rms_vols_bin9==0 & is.na(dataset_notwins$exclude_reg_f9) & !is.na(dataset_notwins$no2_week_schools.F09))
#1498
table(dataset_notwins$mri_consent==1 & dataset_notwins$has_rsfmri_nii_f9==1 &
        dataset_notwins$nvolsf9==200 & dataset_notwins$tsvdataf9==1 & dataset_notwins$braces_mri_f9==0 & dataset_notwins$exclude_incidental==0 &
        dataset_notwins$mean_rms9<=0.25 & dataset_notwins$rms_vols_bin9==0 & is.na(dataset_notwins$exclude_reg_f9) & is.na(dataset_notwins$no2_week_schools.F09))
#1018 


#F13

##mri
table(dataset_notwins$usemrif13==1)

# fmri data available & number of volumes
table(dataset_notwins$usemrif13==1 & dataset_notwins$nvolsf13==200) #correct number of volumes
table(dataset_notwins$usemrif13==1 & dataset_notwins$nvolsf13!=200) #not enough number of volumes
table(dataset_notwins$usemrif13==1 & is.na(dataset_notwins$nvolsf13))  # missing number of volumes

#1254+16

# check all have motion data
table(dataset_notwins$usemrif13==1 & dataset_notwins$nvolsf13==200 & 
        !is.na(dataset_notwins$mean_rms13))

# tsv file
table(dataset_notwins$usemrif13==1 & dataset_notwins$nvolsf13==200 & 
        dataset_notwins$tsvdataf13==1) #tsv file present
table(dataset_notwins$usemrif13==1 & dataset_notwins$nvolsf13==200 & 
        is.na(dataset_notwins$tsvdataf13)) #tsv file missing

#no braces
table(dataset_notwins$usemrif13==1 & dataset_notwins$nvolsf13==200 & 
        dataset_notwins$tsvdataf13==1 & dataset_notwins$exclude_braces!="braces") # no braces

table(dataset_notwins$exclude_braces[dataset_notwins$usemrif13==1 & 
                                       dataset_notwins$nvolsf13==200 & dataset_notwins$tsvdataf13==1])
#12

#to exclude
1254+16+12

#incidental findings F09
table(dataset_notwins$exclude_incidental[dataset_notwins$usemrif13==1 & 
                                           dataset_notwins$nvolsf13==200 & dataset_notwins$tsvdataf13==1  & dataset_notwins$exclude_braces!="braces"])
#14


#incidental findings F13
table(dataset_notwins$if_exclude[ dataset_notwins$usemrif13==1 & 
                                   dataset_notwins$nvolsf13==200 & dataset_notwins$tsvdataf13==1  & dataset_notwins$exclude_braces!="braces" & (dataset_notwins$exclude_incidental==0 | is.na(dataset_notwins$exclude_incidental))])
#10


#incidental findings both visits
table(dataset_notwins$usemrif13==1 & dataset_notwins$nvolsf13==200 & dataset_notwins$tsvdataf13==1 & 
        dataset_notwins$exclude_braces!="braces" & (dataset_notwins$exclude_incidental==0 | is.na(dataset_notwins$exclude_incidental)) & 
        dataset_notwins$if_exclude=="include") #no incidental finding or missing in F09 & no incidental finding F13
#2320


#rms
table(dataset_notwins$usemrif13==1  & dataset_notwins$nvolsf13==200 & 
        dataset_notwins$tsvdataf13==1 & dataset_notwins$exclude_braces!="braces" & (dataset_notwins$exclude_incidental==0 | is.na(dataset_notwins$exclude_incidental)) & 
        dataset_notwins$if_exclude=="include" & dataset_notwins$mean_rms13<=0.25) # low motion
table( dataset_notwins$usemrif13==1  & dataset_notwins$nvolsf13==200 & 
        dataset_notwins$tsvdataf13==1 & dataset_notwins$exclude_braces!="braces" & (dataset_notwins$exclude_incidental==0 | is.na(dataset_notwins$exclude_incidental)) & 
        dataset_notwins$if_exclude=="include" & dataset_notwins$mean_rms13>0.25) # high motion
#65

table(dataset_notwins$usemrif13==1  & dataset_notwins$nvolsf13==200 & 
        dataset_notwins$tsvdataf13==1 & dataset_notwins$exclude_braces!="braces" & (dataset_notwins$exclude_incidental==0 | is.na(dataset_notwins$exclude_incidental)) & 
        dataset_notwins$if_exclude=="include" & dataset_notwins$mean_rms13<=0.25 & dataset_notwins$rms_vols_bin13==0) #less than 20% of the volumes with high motion
table(dataset_notwins$usemrif13==1  & dataset_notwins$nvolsf13==200 & 
        dataset_notwins$tsvdataf13==1 & dataset_notwins$exclude_braces!="braces" & (dataset_notwins$exclude_incidental==0 | is.na(dataset_notwins$exclude_incidental)) & 
        dataset_notwins$if_exclude=="include" & dataset_notwins$mean_rms13<=0.25 & dataset_notwins$rms_vols_bin13==1) #more than 20% of the volumes with high motion

#101
65+101



#registration 
table(dataset_notwins$usemrif13==1  & dataset_notwins$nvolsf13==200 & 
        dataset_notwins$tsvdataf13==1 & dataset_notwins$exclude_braces!="braces" & (dataset_notwins$exclude_incidental==0 | is.na(dataset_notwins$exclude_incidental)) & 
        dataset_notwins$if_exclude=="include" & dataset_notwins$mean_rms13<=0.25 & dataset_notwins$rms_vols_bin13==0 & is.na(dataset_notwins$exclude_reg_f13)) # no registration problem
table(dataset_notwins$usemrif13==1  & dataset_notwins$nvolsf13==200 & 
        dataset_notwins$tsvdataf13==1 & dataset_notwins$exclude_braces!="braces" & (dataset_notwins$exclude_incidental==0 | is.na(dataset_notwins$exclude_incidental)) & 
        dataset_notwins$if_exclude=="include" & dataset_notwins$mean_rms13<=0.25 & dataset_notwins$rms_vols_bin13==0 & dataset_notwins$exclude_reg_f13==1) # registration problem

#8


#acute air pollution home
table(dataset_notwins$usemrif13==1  & dataset_notwins$nvolsf13==200 &
        dataset_notwins$tsvdataf13==1 & dataset_notwins$exclude_braces!="braces" & (dataset_notwins$exclude_incidental==0 | is.na(dataset_notwins$exclude_incidental)) &
        dataset_notwins$if_exclude=="include" & dataset_notwins$mean_rms13<=0.25 & dataset_notwins$rms_vols_bin13==0 & is.na(dataset_notwins$exclude_reg_f13) & !is.na(dataset_notwins$no2_week.F13))
#2121
table(dataset_notwins$usemrif13==1  & dataset_notwins$nvolsf13==200 &
        dataset_notwins$tsvdataf13==1 & dataset_notwins$exclude_braces!="braces" & (dataset_notwins$exclude_incidental==0 | is.na(dataset_notwins$exclude_incidental)) &
        dataset_notwins$if_exclude=="include" & dataset_notwins$mean_rms13<=0.25 & dataset_notwins$rms_vols_bin13==0 & is.na(dataset_notwins$exclude_reg_f13) & is.na(dataset_notwins$no2_week.F13))
#25


#acute air pollution school
table(dataset_notwins$usemrif13==1  & dataset_notwins$nvolsf13==200 &
        dataset_notwins$tsvdataf13==1 & dataset_notwins$exclude_braces!="braces" & (dataset_notwins$exclude_incidental==0 | is.na(dataset_notwins$exclude_incidental)) &
        dataset_notwins$if_exclude=="include" & dataset_notwins$mean_rms13<=0.25 & dataset_notwins$rms_vols_bin13==0 & is.na(dataset_notwins$exclude_reg_f13) & !is.na(dataset_notwins$no2_week_schools.F13))
#1312 

table(dataset_notwins$usemrif13==1  & dataset_notwins$nvolsf13==200 &
        dataset_notwins$tsvdataf13==1 & dataset_notwins$exclude_braces!="braces" & (dataset_notwins$exclude_incidental==0 | is.na(dataset_notwins$exclude_incidental)) &
        dataset_notwins$if_exclude=="include" & dataset_notwins$mean_rms13<=0.25 & dataset_notwins$rms_vols_bin13==0 & is.na(dataset_notwins$exclude_reg_f13) & is.na(dataset_notwins$no2_week_schools.F13))
#834 


#check overlap


table(dataset_notwins$complete9==1 | dataset_notwins$complete13==1)
table(dataset_notwins$complete9==1)
table(dataset_notwins$complete13==1)
table(dataset_notwins$complete9==1  & dataset_notwins$complete13==1)

table(dataset_notwins$complete9_sch==1 | dataset_notwins$complete13_sch==1)
table(dataset_notwins$complete9_sch==1)
table(dataset_notwins$complete13_sch==1)
table(dataset_notwins$complete9_sch==1  & dataset_notwins$complete13_sch==1)


###sociodemographic characteristics of sample (main: only home sample)

load("results/imputedmids_analysis_home.RData")
home25<-complete(imp_full, 25)

catVars <- c( "GENDER","educm_3cat","ethnmv2_6cat",  "MARDICH","parity_3cat", 'income_4cat')
contVars <- c("agemri.F09", "agemri.F13", 'AGE_M_v2','BMI_0')

descInfo <- mkDescriptiveTable_imp(contVars, catVars, home25 )
#write.xlsx(descInfo, file =  'results/Descriptives_imputed_sample_home.xlsx')

descInfo_cat<-as.data.frame(sprintf("%0.1f",descInfo[[1]]$percents))

descInfo[[2]]$value = ifelse(!is.na(descInfo[[2]]$means),paste0(sprintf("%0.1f",descInfo[[2]]$means), " (",sprintf("%0.1f",descInfo[[2]]$sds), ")"))

descInfo_cont<-as.data.frame(descInfo[[2]]$value)

colnames(descInfo_cont)=c("Distribution")
colnames(descInfo_cat)=c("Distribution")

descriptive_home<-rbind(descInfo_cont,descInfo_cat)


Categories = c("","", "","","Boy", "Girl","High", "Medium", "Low",
               "Netherlands","Morocco", "Surinam","Turkey","Other European","Other non-European",
               "Married/living together","No partner","Nulliparous","1 child","2+ children", "> 2200 euro", "1600-2200 euro","900-1600 euro",
               "<900 euro")
Variables =c("Participant age 10-years visit (mean, SD)","Participant age 14-years visit (mean, SD)","Maternal age at intake (mean, SD)","Maternal pre-pregnancy body mass index (kg/m2) (mean, SD)",
             "Participant sex (%)","","Maternal education level (%)","","" ,
             "Maternal national origin (%)","","","","","", "Marital status (%)","", "Parity (%)","","","Monthly household income during pregnancy (%)","","","" )

final_descriptive<-cbind(Variables,Categories,descriptive_home)
final_descriptive1<-rbind(final_descriptive[5:6,],final_descriptive[1:2,],final_descriptive[3:4,], final_descriptive[7:24,] )
write.xlsx(final_descriptive1, file =  'results/Descriptives_participants_home.xlsx')



###sociodemographic characteristics of samples (supplement) 

#HOME

str(dataset_ap)
catVars <- c( "GENDER","educm_3cat","educp_3cat","ethnmv2_6cat", "ethnfv2_6cat", "MARDICH", 'income_4cat', "SMOKE_ALL", "mdrink_updated", "FOLIUM_VALIDATED", "parity_3cat", "season")
contVars <- c('AGE_M_v2',"AGE_BF_v2",'BMI_0', 'BMI_P',  "APM_IQ", "ndvi300_preg", "STATUSSCORE")
descInfo_included <- mkDescriptiveTable(contVars, catVars, dataset_ap )
#write.xlsx(descInfo_included, file =  'results/Descriptives_included_sample_n3608.xlsx')
descInfo_included[[1]]
descInfo_included[[2]]

descInfo_excluded <- mkDescriptiveTable(contVars, catVars, dataset_ap_not_included)
#write.xlsx(descInfo_excluded, file =  'results/Descriptives_notincluded_sample_n5999.xlsx')
descInfo_excluded[[1]]
descInfo_excluded[[2]]

#get percents columns from included & excluded participants (categorical variables)

table_cat = cbind(descInfo_included[[1]]$percents, descInfo_excluded[[1]]$percents) %>% as.data.frame

#create a new column with mean & SD (continuous variables, included participants)

descInfo_included[[2]]$value = ifelse(!is.na(descInfo_included[[2]]$means),paste0(sprintf("%0.1f",descInfo_included[[2]]$means), " (",sprintf("%0.1f",descInfo_included[[2]]$sds), ")"))

#fix missings (continuous variables, included participants)

missings = round(descInfo_included[[2]][,c(4)], digits = 1)
missings[is.na(missings)] = 0

#bind mean & SD with missing % (continuous variables, included participants)

table_cont_incl = rbind(descInfo_included[[2]]$value, sprintf("%0.1f",missings)) %>% as.data.frame

#make long table (continuous variables, included participants) 

table_cont_incl = cbind(c(table_cont_incl$V1, table_cont_incl$V2,table_cont_incl$V3, table_cont_incl$V4,table_cont_incl$V5,table_cont_incl$V6,table_cont_incl$V7))%>% as.data.frame

#create a new column with mean & SD (continuous variables, excluded participants)

descInfo_excluded[[2]]$value = ifelse(!is.na(descInfo_excluded[[2]]$means),paste0(sprintf("%0.1f",descInfo_excluded[[2]]$means), " (",sprintf("%0.1f",descInfo_excluded[[2]]$sds), ")"))

#fix missings (continuous variables, excluded participants)

missings = round(descInfo_excluded[[2]][,c(4)], digits = 1)
missings[is.na(missings)] = 0

#bind mean & SD with missing % (continuous variables, excluded participants)

table_cont_excl = rbind(descInfo_excluded[[2]]$value, sprintf("%0.1f",missings)) %>% as.data.frame

#make long table (continuous variables, excluded participants) 

table_cont_excl = cbind(c(table_cont_excl$V1, table_cont_excl$V2,table_cont_excl$V3, table_cont_excl$V4,table_cont_excl$V5, table_cont_excl$V6,table_cont_excl$V7))%>% as.data.frame

#bind info from included and excluded participants (continuous variables)

table_cont=cbind(table_cont_incl, table_cont_excl)%>% as.data.frame

# check differences between included and excluded participants

var<-c()
p<-c()
for (v in catVars) {
  test<-chisq.test(dataset_notwins$incl_var, dataset_notwins[,v])
  var<-c(var,v)
  p <- c(p, test$p.value)%>% round(3)
  final_results_cat <- data.frame(var,p)
}


var<-c()
p<-c()
for (v in contVars) {
  x <-dataset_notwins[,v][dataset_notwins$incl_var==0]
  y<-dataset_notwins[,v][dataset_notwins$incl_var==1]
  test<-wilcox.test(x,y, paired = F )
  var<-c(var,v)
  p <- c(p, test$p.value)%>% round(3)
  final_results_cont <- data.frame(var,p)
}

#fix p values

final_results_cat$p = ifelse(final_results_cat$p==0,"<0.001",sprintf("%0.3f",final_results_cat$p))
final_results_cont$p = ifelse(final_results_cont$p==0,"<0.001",sprintf("%0.3f",final_results_cont$p))

#leave empty rows for categories and missings

final_results_cat=rbind(final_results_cat[1,], "","",final_results_cat[2,], "","","",final_results_cat[3,], "","","",final_results_cat[4,], "","","","","","",
                        final_results_cat[5,], "","","","","","",final_results_cat[6,], "","",final_results_cat[7,], "","","","",final_results_cat[8,], "","","",
                        final_results_cat[9,], "","","","",final_results_cat[10,], "","","", final_results_cat[11,], "","","", final_results_cat[12,], "","","","")


final_results_cont=rbind(final_results_cont[1,], "",final_results_cont[2,], "",final_results_cont[3,], "",final_results_cont[4,], "",
                         final_results_cont[5,], "",final_results_cont[6,], "",final_results_cont[7,], "")

#bind included/excluded/p values columns

table_cat = cbind(sprintf("%0.1f",descInfo_included[[1]]$percents), sprintf("%0.1f",descInfo_excluded[[1]]$percents), final_results_cat$p) %>% as.data.frame
table_cont=cbind(table_cont_incl, table_cont_excl, final_results_cont$p)%>% as.data.frame

#add column labels

colnames(table_cat) = c("Participants (n=3608)", "Non-participants (n=5999)", "p-value")
colnames(table_cont) = c("Participants (n=3608)", "Non-participants (n=5999)", "p-value")

#bind categorical and continuous variables

table = rbind(table_cat, table_cont)

###sociodemographic characteristics of sample 
#SCHOOL

descInfo_included <- mkDescriptiveTable(contVars, catVars, dataset_ap_sch )
#write.xlsx(descInfo_included, file =  'results/Descriptives_included_sample_n2305.xlsx')
descInfo_included[[1]]
descInfo_included[[2]]

descInfo_excluded <- mkDescriptiveTable(contVars, catVars, dataset_ap_sch_not_included)
#write.xlsx(descInfo_excluded, file =  'results/Descriptives_notincluded_sample_n7302.xlsx')
descInfo_excluded[[1]]
descInfo_excluded[[2]]

#get percents columns from included & excluded participants (categorical variables)

table_cat = cbind(descInfo_included[[1]]$percents, descInfo_excluded[[1]]$percents) %>% as.data.frame

#create a new column with mean & SD (continuous variables, included participants)

descInfo_included[[2]]$value = ifelse(!is.na(descInfo_included[[2]]$means),paste0(sprintf("%0.1f",descInfo_included[[2]]$means), " (",sprintf("%0.1f",descInfo_included[[2]]$sds), ")"))

#fix missings (continuous variables, included participants)

missings = round(descInfo_included[[2]][,c(4)], digits = 1)
missings[is.na(missings)] = 0

#bind mean & SD with missing % (continuous variables, included participants)

table_cont_incl = rbind(descInfo_included[[2]]$value, sprintf("%0.1f",missings)) %>% as.data.frame

#make long table (continuous variables, included participants) 

table_cont_incl = cbind(c(table_cont_incl$V1, table_cont_incl$V2,table_cont_incl$V3, table_cont_incl$V4,table_cont_incl$V5,table_cont_incl$V6,table_cont_incl$V7))%>% as.data.frame

#create a new column with mean & SD (continuous variables, excluded participants)

descInfo_excluded[[2]]$value = ifelse(!is.na(descInfo_excluded[[2]]$means),paste0(sprintf("%0.1f",descInfo_excluded[[2]]$means), " (",sprintf("%0.1f",descInfo_excluded[[2]]$sds), ")"))

#fix missings (continuous variables, excluded participants)

missings = round(descInfo_excluded[[2]][,c(4)], digits = 1)
missings[is.na(missings)] = 0

#bind mean & SD with missing % (continuous variables, excluded participants)

table_cont_excl = rbind(descInfo_excluded[[2]]$value, sprintf("%0.1f",missings)) %>% as.data.frame

#make long table (continuous variables, excluded participants) 

table_cont_excl = cbind(c(table_cont_excl$V1, table_cont_excl$V2,table_cont_excl$V3, table_cont_excl$V4,table_cont_excl$V5, table_cont_excl$V6,table_cont_excl$V7))%>% as.data.frame

#bind info from included and excluded participants (continuous variables)

table_cont=cbind(table_cont_incl, table_cont_excl)%>% as.data.frame

# check differences between included and excluded participants

var<-c()
p<-c()
for (v in catVars) {
  test<-chisq.test(dataset_notwins$incl_var_school, dataset_notwins[,v])
  var<-c(var,v)
  p <- c(p, test$p.value)%>% round(3)
  final_results_cat <- data.frame(var,p)
}


var<-c()
p<-c()
for (v in contVars) {
  x <-dataset_notwins[,v][dataset_notwins$incl_var_school==0]
  y<-dataset_notwins[,v][dataset_notwins$incl_var_school==1]
  test<-wilcox.test(x,y, paired = F )
  var<-c(var,v)
  p <- c(p, test$p.value)%>% round(3)
  final_results_cont <- data.frame(var,p)
}

#fix p values

final_results_cat$p = ifelse(final_results_cat$p==0,"<0.001",sprintf("%0.3f",final_results_cat$p))
final_results_cont$p = ifelse(final_results_cont$p==0,"<0.001",sprintf("%0.3f",final_results_cont$p))

#leave empty rows for categories and missings

final_results_cat=rbind(final_results_cat[1,], "","",final_results_cat[2,], "","","",final_results_cat[3,], "","","",final_results_cat[4,], "","","","","","",
                        final_results_cat[5,], "","","","","","",final_results_cat[6,], "","",final_results_cat[7,], "","","","",final_results_cat[8,], "","","",
                        final_results_cat[9,], "","","","",final_results_cat[10,], "","","", final_results_cat[11,], "","","", final_results_cat[12,], "","","","")


final_results_cont=rbind(final_results_cont[1,], "",final_results_cont[2,], "",final_results_cont[3,], "",final_results_cont[4,], "",
                         final_results_cont[5,], "",final_results_cont[6,], "",final_results_cont[7,], "")

#bind included/excluded/p values columns

table_cat = cbind(sprintf("%0.1f",descInfo_included[[1]]$percents), sprintf("%0.1f",descInfo_excluded[[1]]$percents), final_results_cat$p) %>% as.data.frame
table_cont=cbind(table_cont_incl, table_cont_excl, final_results_cont$p)%>% as.data.frame

#add column labels

colnames(table_cat) = c("Participants (n=2305)", "Non-participants (n=7302)", "p-value")
colnames(table_cont) = c("Participants (n=2305)", "Non-participants (n=7302)", "p-value")

#bind categorical and continuous variables

table_school = rbind(table_cat, table_cont)

###Final table

Categories = c("Boy", "Girl", "Missing rate", "High", "Medium", "Low","Missing rate", "High", "Medium", "Low","Missing rate",
                     "Netherlands","Morocco", "Surinam","Turkey","Other European","Other non-European","Missing rate",
                     "Netherlands","Morocco", "Surinam","Turkey","Other European","Other non-European","Missing rate",
                     "Married/living together","No partner","Missing rate", "> 2200 euro", "1600-2200 euro","900-1600 euro",
                     "<900 euro","Missing rate", "Never smoked during pregnancy", "Smoked until pregnancy was known",
                     "Continued smoking in pregnancy","Missing rate","Never drank during pregnancy","Drank until pregnancy was known",
                     "Continued drinking occasionally", "Continued drinking frequently (1 or more glass/week for at least 2 trimesters)","Missing rate",
                     "No","Start 1st 10 weeks","Start periconceptional" ,"Missing rate","Nulliparous","1 child","2+ children" ,"Missing rate",
                     "Winter", "Spring", "Summer", "Autumn","Missing rate","","Missing rate","","Missing rate","","Missing rate","","Missing rate","","Missing rate","","Missing rate","","Missing rate")


Variables =c("Participant's sex (%)","","","Maternal education level (%)" ,"","","", "Paternal education level (%)","","","", "Maternal national origin (%)","","","","","","",
                    "Paternal national origin (%)","","","","","","", "Marital status (%)", "","","Monthly household income during pregnancy  (%)", "","","","", "Maternal smoking during pregnancy (%)","","","",
                    "Maternal alcohol consumption during pregnancy (%)","","","","", "Folic acid supplementation during pregnancy (%)","","","", "Parity (%)", "","","","Season of birth (%)","","","","",
                    "Maternal age at intake (mean, SD)","", "Paternal age (mean, SD)","", "Maternal pre-pregnancy body mass index (kg/m2) (mean, SD)","",
                    "Paternal pre-pregnancy body mass index (kg/m2) (mean, SD)","", "Maternal intelligence quotient score (mean, SD)","", "Residential surrounding greenness during pregnancy (mean, SD)","",
                    "Socioeconomic status neighborhood during pregnancy (mean, SD)","")

table_final = cbind(Variables,Categories,table, table_school)
write.xlsx(table_final, file =  'results/Descriptives_participantsvsnonparticipants.xlsx')


###Exposures descriptive  

dataset_metricsF09<-dataset_metrics[dataset_metrics$ses=="F09",]
dataset_metricsF13<-dataset_metrics[dataset_metrics$ses=="F13",]

#histograms

pollutants_week<-c("no2_week","nox_week","pm25_week","pm10_week","pmcoarse_week","pm25abs_week")

plotsF09 <- list()
for (i in seq_along(pollutants_week)){
  p<-pollutants_week[i]
  plotsF09[[p]]<-ggplot(dataset_metricsF09, aes_string(p))+
    theme_minimal()+
    geom_histogram(col="black", fill="white")
}

plotsF13 <- list()
for (i in seq_along(pollutants_week)){
  p<-pollutants_week[i]
  plotsF13[[p]]<-ggplot(dataset_metricsF13, aes_string(p))+
    theme_minimal()+
    geom_histogram(col="black", fill="white")
}

grid.arrange(arrangeGrob(plotsF09$no2_week,plotsF13$no2_week,plotsF09$nox_week,plotsF13$nox_week,plotsF09$pm25_week,plotsF13$pm25_week,
                         plotsF09$pm25abs_week,plotsF13$pm25abs_week,plotsF09$pm10_week,plotsF13$pm10_week,plotsF09$pmcoarse_week,plotsF13$pmcoarse_week))
  
###Correlations among exposures

mydata.cor = stats::cor(dataset_metrics[, c(pollutants_week)],  method ="spearman",  use = "pairwise.complete.obs")
colnames(mydata.cor) = c('$NO[2]', '$NO[X]', 
                         '$PM[2.5]',
                         '$PM[10]','$PM[COARSE]','$PM[2.5]*~absorbance')

rownames(mydata.cor) = c('$NO[2]', '$NO[X]', 
                         '$PM[2.5]',
                         '$PM[10]','$PM[COARSE]','$PM[2.5]*~absorbance')
corrplot::corrplot(mydata.cor,  method = "square",  type = 'lower', diag = FALSE,
                   pch.cex = .8, tl.cex = .8,  tl.srt = 30,
                   tl.col = "black", addCoef.col ='black')




##Table acute values 2 visits/weekly average lags


exp<-c()
p25_F09<-c()
p50_F09<-c()
p75_F09<-c()
p25_F13<-c()
p50_F13<-c()
p75_F13<-c()
for (i in pollutants_week) {
  exp<-c(exp, i)
  p25_F09<-c(p25_F09, summary(dataset_metricsF09[,i])[2])
  p50_F09<-c(p50_F09, summary(dataset_metricsF09[,i])[3])
  p75_F09<-c(p75_F09, summary(dataset_metricsF09[,i])[5])
  p25_F13<-c(p25_F13, summary(dataset_metricsF13[,i])[2])
  p50_F13<-c(p50_F13, summary(dataset_metricsF13[,i])[3])
  p75_F13<-c(p75_F13, summary(dataset_metricsF13[,i])[5])
  final_results <- data.frame(exp,p25_F09,p50_F09,p75_F09,p25_F13,p50_F13,p75_F13)
}
#write.xlsx(final_results, file='results/pc_exposures_lags_week.xlsx')

###Correlations between chronic and acute (if it's too high use acute-chronic instead of both exposures in same model)
#childhood exposure vs week each visit

corr_ch_vs_ac<-stats::cor(dataset_metrics[, c("no2_week", "nox_week","pm25_week","pm25abs_week","pm10_week","pmcoarse_week",
                                              "no2","nox","pm25","pm25abs" , "pm10","pmcoarse")],  method ="spearman",  use = "pairwise.complete.obs")
testRes = cor.mtest(corr_ch_vs_ac, conf.level = 0.95)#check statistical differences


#histograms school
pollutants_week_school<-c("no2_week_schools","nox_week_schools","pm25_week_schools","pm10_week_schools","pmcoarse_week_schools")

plotsF09 <- list()
for (i in seq_along(pollutants_week_school)){
  p<-pollutants_week_school[i]
  plotsF09[[p]]<-ggplot(dataset_metricsF09, aes_string(p))+
    theme_minimal()+
    geom_histogram(col="black", fill="white")
}

plotsF13 <- list()
for (i in seq_along(pollutants_week_school)){
  p<-pollutants_week_school[i]
  plotsF13[[p]]<-ggplot(dataset_metricsF13, aes_string(p))+
    theme_minimal()+
    geom_histogram(col="black", fill="white")
}

grid.arrange(arrangeGrob(plotsF09$no2_week_schools,plotsF13$no2_week_schools,plotsF09$nox_week_schools,plotsF13$nox_week_schools,
                         plotsF09$pm25_week_schools,plotsF13$pm25_week_schools,
                         plotsF09$pm10_week_schools,plotsF13$pm10_week_schools,plotsF09$pmcoarse_week_schools,plotsF13$pmcoarse_week_schools))

###Correlations among exposures SCHOOL

mydata.cor = stats::cor(dataset_metrics[, c(pollutants_week_school)],  method ="spearman",  use = "pairwise.complete.obs")
colnames(mydata.cor) = c('$NO[2]', '$NO[X]', 
                         '$PM[2.5]',
                         '$PM[10]','$PM[COARSE]')

rownames(mydata.cor) = c('$NO[2]', '$NO[X]', 
                         '$PM[2.5]',
                         '$PM[10]','$PM[COARSE]')
corrplot::corrplot(mydata.cor,  method = "square",  type = 'lower', diag = FALSE,
                   pch.cex = .8, tl.cex = .8,  tl.srt = 30,
                   tl.col = "black", addCoef.col ='black')



##Table acute values 2 visits/weekly average lags  SCHOOL

exp<-c()
p25_F09<-c()
p50_F09<-c()
p75_F09<-c()
p25_F13<-c()
p50_F13<-c()
p75_F13<-c()
for (i in pollutants_week_school) {
  exp<-c(exp, i)
  p25_F09<-c(p25_F09, summary(dataset_metricsF09[,i])[2])
  p50_F09<-c(p50_F09, summary(dataset_metricsF09[,i])[3])
  p75_F09<-c(p75_F09, summary(dataset_metricsF09[,i])[5])
  p25_F13<-c(p25_F13, summary(dataset_metricsF13[,i])[2])
  p50_F13<-c(p50_F13, summary(dataset_metricsF13[,i])[3])
  p75_F13<-c(p75_F13, summary(dataset_metricsF13[,i])[5])
  final_results_school <- data.frame(exp,p25_F09,p50_F09,p75_F09,p25_F13,p50_F13,p75_F13)
}

final_results
final_results_school

exposures_table = rbind(final_results[1,],final_results_school[1,],final_results[2,],final_results_school[2,],
                        final_results[3,],final_results_school[3,],final_results[4,],final_results_school[4,],final_results[5,],final_results_school[5,],final_results[6,])

#boxplots

dataset_metrics1 <- dataset_metrics[c(pollutants_week_school, pollutants_week, "idc", "ses")]

dataset_metrics1 <- dataset_metrics1 %>% rename(
  no2_week.S = no2_week_schools,
  nox_week.S = nox_week_schools,
  pm25_week.S = pm25_week_schools,
  pm10_week.S = pm10_week_schools,
  pmcoarse_week.S = pmcoarse_week_schools,
  no2_week.H = no2_week,
  nox_week.H = nox_week,
  pm25_week.H = pm25_week,
  pm10_week.H = pm10_week,
  pmcoarse_week.H = pmcoarse_week,
  pm25abs_week.H = pm25abs_week)

dataset_metrics1$pm25abs_week.S = NA

varying = c("no2_week.S","nox_week.S","pm25_week.S","pm10_week.S","pmcoarse_week.S", "pm25abs_week.S","no2_week.H" , "nox_week.H","pm25_week.H",    
             "pm10_week.H","pmcoarse_week.H","pm25abs_week.H" ) 
idVar = c('idc','ses')
timeVar = 'place'

dataset_metrics2 <- reshape(dataset_metrics1, varying=varying, idvar=idVar, timevar=timeVar, direction='long')

dataset_metrics2$group = paste(dataset_metrics2$ses,dataset_metrics2$place )

no2=ggplot(dataset_metrics2, aes(group,no2_week)) + 
  geom_boxplot(width = .1, outlier.size = 0.2)+
  labs(x="", y=str2expression('NO[2]~(µg/m^3)'))+
  scale_x_discrete(labels = c('Age-10 visit\n(home)','Age-10 visit\n(school)','Age-14 visit\n(home)','Age-14 visit\n(school)')) +
  theme(aspect.ratio = 1)+ theme_minimal()

nox=ggplot(dataset_metrics2, aes(group,nox_week)) + 
  geom_boxplot(width = .1, outlier.size = 0.2)+
  labs(x="", y=str2expression('NO[X]~(µg/m^3)'))+
  scale_x_discrete(labels = c('Age-10 visit\n(home)','Age-10 visit\n(school)','Age-14 visit\n(home)','Age-14 visit\n(school)')) +
  theme(aspect.ratio = 1)+ theme_minimal()

pm25=ggplot(dataset_metrics2, aes(group,pm25_week)) +
  labs(x="", y=str2expression('PM[2.5]~(µg/m^3)'))+
  scale_x_discrete(labels = c('Age-10 visit\n(home)','Age-10 visit\n(school)','Age-14 visit\n(home)','Age-14 visit\n(school)')) +
  geom_boxplot(width = .1, outlier.size = 0.2)+
  theme(aspect.ratio = 1)+  theme_minimal()

pm10=ggplot(dataset_metrics2, aes(group,pm10_week)) + 
  labs(x="", y=str2expression('PM[10]~(µg/m^3)'))+
  scale_x_discrete(labels = c('Age-10 visit\n(home)','Age-10 visit\n(school)','Age-14 visit\n(home)','Age-14 visit\n(school)')) +
  geom_boxplot(width = .1, outlier.size = 0.2)+
  theme(aspect.ratio = 1)+ theme_minimal()

pmcoarse=ggplot(dataset_metrics2, aes(group,pmcoarse_week)) + 
  labs(x="", y=str2expression('PM[COARSE]~(µg/m^3)'))+
  scale_x_discrete(labels = c('Age-10 visit\n(home)','Age-10 visit\n(school)','Age-14 visit\n(home)','Age-14 visit\n(school)')) +
  geom_boxplot(width = .1, outlier.size = 0.2)+
  theme(aspect.ratio = 1)+ theme_minimal()

pm25abs=ggplot(dataset_metrics2, aes(group,pm25abs_week)) + 
  labs(x="", y=str2expression('PM[2.5]*~absorbance~(10^-5*m^-1)'))+
  scale_x_discrete(labels = c('Age-10 visit\n(home)','','Age-14 visit\n(home)','')) +
  geom_boxplot(width = .1, outlier.size = 0.2)+
  theme(aspect.ratio = 1)+ theme_minimal()

pdf("results/pollutants_distributions.pdf")
grid.arrange(arrangeGrob(no2,nox,pm25,pm10,pmcoarse,pm25abs))
dev.off()

##correlations between home and school levels

stats::cor(dataset_metricsF09[, c("no2_week","no2_week_schools")], method ="spearman",  use = "pairwise.complete.obs")
stats::cor(dataset_metricsF09[, c("nox_week","nox_week_schools")], method ="spearman",  use = "pairwise.complete.obs")
stats::cor(dataset_metricsF09[, c("pm25_week","pm25_week_schools")], method ="spearman",  use = "pairwise.complete.obs")
stats::cor(dataset_metricsF09[, c("pm10_week","pm10_week_schools")], method ="spearman",  use = "pairwise.complete.obs")
stats::cor(dataset_metricsF09[, c("pmcoarse_week","pmcoarse_week_schools")], method ="spearman",  use = "pairwise.complete.obs")

stats::cor(dataset_metricsF13[, c("no2_week","no2_week_schools")], method ="spearman",  use = "pairwise.complete.obs")
stats::cor(dataset_metricsF13[, c("nox_week","nox_week_schools")], method ="spearman",  use = "pairwise.complete.obs")
stats::cor(dataset_metricsF13[, c("pm25_week","pm25_week_schools")], method ="spearman",  use = "pairwise.complete.obs")
stats::cor(dataset_metricsF13[, c("pm10_week","pm10_week_schools")], method ="spearman",  use = "pairwise.complete.obs")
stats::cor(dataset_metricsF13[, c("pmcoarse_week","pmcoarse_week_schools")], method ="spearman",  use = "pairwise.complete.obs")

