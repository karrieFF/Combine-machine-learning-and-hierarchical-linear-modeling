
library(lme4)
library (jtools)
library(lmerTest)
library(dtplyr)
library(dplyr)
data = read.csv('E:\\EIE-Storage\\E1003 Onedrive HKU\\OneDrive - The University Of Hong Kong\\EIA-Research\\2023-01 SEL_ML_HLM_PISA2018\\HK\\01 Data\\2-after imputation.csv')

# rename DV
rename_data = rename(data, c(RDM=Responsible_decsion_making,
                             SEM=Self_management,
                             SEA=Self_awareness,
                             SOA=Social_awareness,
                             RES=Relationship_skills))

# select variables
cols_remain = c('SOA',
                'CNTSCHID',
                'STIMREAD','DISCLIMA','TCOTLCOMP',
                'BELONG','SATJOB','ATTLNACT','DISCRIM','PERCOOP','PERCOMP', 'PASCHPOL',
                'ESCS','EMOSUPS','CURSUPP','JOYREADP')

# create new dataset for analysis
new_data = rename_data[,colnames(rename_data) %in% cols_remain ] 


# HLM analysis
m1 <- lmer(SOA~STIMREAD+DISCLIMA+TCOTLCOMP+BELONG+SATJOB+ATTLNACT+DISCRIM+PERCOOP+PERCOMP+PASCHPOL+ESCS+EMOSUPS+CURSUPP+JOYREADP+(1|CNTSCHID), data = new_data, REML = FALSE)
summary (m1)
summ(m1)
