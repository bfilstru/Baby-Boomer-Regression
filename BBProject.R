#==============================================================================
#   Data Analysis: import data, summary stats, regression
#==============================================================================

# original scripts by Bill Sundstrom, Michael Kevane and Matt Holian 
# This version combines various earlier scripts and is by Ben Filstrup 11/8/2018
# Carries out regression on migration

#==============================================================================
#   1. Settings, packages, and options
#==============================================================================

# Clear the working space
rm(list = ls())

# Set working directory
setwd("C:/Users/filst/Desktop/IPUMS") #Windows

# Load the packages even though you won't usually need all of them 
library(countrycode)
library(doBy)
library(dplyr)
library(foreign)
library(gdata)
library(ggplot2)
library(knitr)
library(lmtest)
library(readstata13)
library(reshape)
library(sandwich)
library(stargazer)
library(WDI)
library(XML)
library(AER)
library(car)
library(ipumsr)

# turn off scientific notation except for big numbers
options(scipen = 9)
# function to calculate corrected SEs for regression 
cse <- function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

#==============================================================================
#   2. Data section
#==============================================================================

# Read data from csv file
babyBoomersMigration = read.csv("usa_00003.csv",header=TRUE,sep=",")

# generate a variable which is a recoded version of the migration variable
babyBoomersMigration$MIGRATED<-recode(babyBoomersMigration$MIGRATE1,"0:1=0; 4:9=0; 2:3=1")

babyBoomersMigration$FEMALE<-recode(babyBoomersMigration$SEX,"1=0;2=1")

babyBoomersMigration$BLACK<-recode(babyBoomersMigration$RACE,"2=1; else=0")
babyBoomersMigration$AMERICANINDIAN_OR_ALASKANATIVE<-recode(babyBoomersMigration$RACE,"3=1; else=0")
babyBoomersMigration$ASIAN_OR_PACIFICISLANDER<-recode(babyBoomersMigration$RACE,"4:6=1; else=0")
babyBoomersMigration$OTHER_OR_MULTIPLE<-recode(babyBoomersMigration$RACE,"7:9=1; else=0")
babyBoomersMigration$HISPANIC<-recode(babyBoomersMigration$HISPAN,"1:4=1; else=0")

babyBoomersMigration$CHILD<-recode(babyBoomersMigration$YNGCH,"0:98=1; else=0")

babyBoomersMigration$LESSTHANHIGH<-recode(babyBoomersMigration$EDUC,"0:2=1; else=0")
babyBoomersMigration$BACHELORSDEG<-recode(babyBoomersMigration$EDUC,"10=1; else=0")
babyBoomersMigration$GRADUATEDEG<-recode(babyBoomersMigration$EDUC,"11=1; else=0")

babyBoomersMigration$DISABILITY<-0
babyBoomersMigration[babyBoomersMigration$DIFFREM == 2 | babyBoomersMigration$DIFFPHYS == 2 | babyBoomersMigration$DIFFMOB == 2 | babyBoomersMigration$DIFFCARE == 2 | babyBoomersMigration$DIFFSENS == 2 | babyBoomersMigration$DIFFEYE == 2 | babyBoomersMigration$DIFFHEAR == 2, "DISABILITY"] <- 1

babyBoomersMigration$VETERAN<-recode(babyBoomersMigration$VETSTAT,"2=1; else=0")

babyBoomersMigration$MARRIED<-recode(babyBoomersMigration$MARST,"1:2=1; else=0")

babyBoomersMigration$INLABFORCE<-recode(babyBoomersMigration$LABFORCE,"2=1; else=0")

babyBoomersMigration$LESSTHAN25K<-recode(babyBoomersMigration$INCTOT,"0000001:0024999=1; else=0")
babyBoomersMigration$BET25KTO50K<-recode(babyBoomersMigration$INCTOT,"0025000:0050000=1; else=0")
babyBoomersMigration$BET100KTO250K<-recode(babyBoomersMigration$INCTOT,"0100000:0250000=1; else=0")
babyBoomersMigration$MORETHAN250K<-recode(babyBoomersMigration$INCTOT,"0250001:9999998=1; else=0")

# subsamples
ageGate = subset(babyBoomersMigration, AGE<=65 & AGE>=45)
ageGate2 = subset(ageGate, GQ!=3 & GQ !=4 & GQ !=5)

# Read data from csv file
babyBoomersMigration2 = read.csv("usa_00004.csv",header=TRUE,sep=",")

# generate a variable which is a recoded version of the migration variable
babyBoomersMigration2$MIGRATED<-recode(babyBoomersMigration2$MIGRATE1,"0:1=0; 4:9=0; 2:3=1")

babyBoomersMigration2$FEMALE<-recode(babyBoomersMigration2$SEX,"1=0;2=1")

babyBoomersMigration2$BLACK<-recode(babyBoomersMigration2$RACE,"2=1; else=0")
babyBoomersMigration2$AMERICANINDIAN_OR_ALASKANATIVE<-recode(babyBoomersMigration2$RACE,"3=1; else=0")
babyBoomersMigration2$ASIAN_OR_PACIFICISLANDER<-recode(babyBoomersMigration2$RACE,"4:6=1; else=0")
babyBoomersMigration2$OTHER_OR_MULTIPLE<-recode(babyBoomersMigration2$RACE,"7:9=1; else=0")
babyBoomersMigration2$HISPANIC<-recode(babyBoomersMigration2$HISPAN,"1:4=1; else=0")

babyBoomersMigration2$CHILD<-recode(babyBoomersMigration2$YNGCH,"0:98=1; else=0")

babyBoomersMigration2$LESSTHANHIGH<-recode(babyBoomersMigration2$EDUC,"0:2=1; else=0")
babyBoomersMigration2$BACHELORSDEG<-recode(babyBoomersMigration2$EDUC,"10=1; else=0")
babyBoomersMigration2$GRADUATEDEG<-recode(babyBoomersMigration2$EDUC,"11=1; else=0")

babyBoomersMigration2$DISABILITY<-0
babyBoomersMigration2[babyBoomersMigration2$DIFFREM == 2 | babyBoomersMigration2$DIFFPHYS == 2 | babyBoomersMigration2$DIFFMOB == 2 | babyBoomersMigration2$DIFFCARE == 2 | babyBoomersMigration2$DIFFSENS == 2 | babyBoomersMigration2$DIFFEYE == 2 | babyBoomersMigration2$DIFFHEAR == 2, "DISABILITY"] <- 1

babyBoomersMigration2$VETERAN<-recode(babyBoomersMigration2$VETSTAT,"2=1; else=0")

babyBoomersMigration2$MARRIED<-recode(babyBoomersMigration2$MARST,"1:2=1; else=0")

babyBoomersMigration2$INLABFORCE<-recode(babyBoomersMigration2$LABFORCE,"2=1; else=0")

babyBoomersMigration2$LESSTHAN25K<-recode(babyBoomersMigration2$INCTOT,"0000001:0024999=1; else=0")
babyBoomersMigration2$BET25KTO50K<-recode(babyBoomersMigration2$INCTOT,"0025000:0050000=1; else=0")
babyBoomersMigration2$BET100KTO250K<-recode(babyBoomersMigration2$INCTOT,"0100000:0250000=1; else=0")
babyBoomersMigration2$MORETHAN250K<-recode(babyBoomersMigration2$INCTOT,"0250001:9999998=1; else=0")

# subsamples
ageGate3 = subset(babyBoomersMigration2, AGE<=65 & AGE>=45)
ageGate4 = subset(ageGate3, GQ!=3 & GQ !=4 & GQ !=5)

#==============================================================================
#   3. Analysis section
#==============================================================================

# Look at basic stats for all the numeric variables in the dataset

# stargazer(data, type="text", summary.stat=c("n", "mean", "median", "sd", "min", "max" ), digits=2, title="SW EE 4.2", out = "Table_EE4.2_sumstats.txt")

# summary statistics for a subsample
stargazer(ageGate2, type="text", summary.stat=c("n", "mean", "median", "sd", "min", "max" ), digits=2, title="Summary Statistics 2009-2011", out = "Summary_Statistics_MEGA_2009_2011.txt")
stargazer(ageGate4, type="text", summary.stat=c("n", "mean", "median", "sd", "min", "max" ), digits=2, title="Summary Statistics 2015-2017", out = "Summary_Statistics_MEGA_2015_2017.txt")

# difference in means test

# t.test(migrated~LESSTHAN25K, data=ageGate2, FUN=c(mean), na.rm=TRUE)

# scatterplot with regression line
#ggplot(data=ageGate2, aes(x=LESSTHAN25K, y=MIGRATED))+geom_point(shape=1)+labs(title="Elderly Migration Regression (Wage) MEGA")+labs(x="Income less than 25k", y="Migrated")+geom_smooth(method=lm)

# running regressions;

# standard order
# reg1 = lm(MIGRATED~AGE+FEMALE+BLACK+AMERICANINDIAN_OR_ALASKANATIVE+ASIAN_OR_PACIFICISLANDER+OTHER_OR_MULTIPLE+HISPANIC+CHILD+LESSTHANHIGH+BACHELORSDEG+GRADUATEDEG+DISABILITY+VETERAN+MARRIED+INLABFORCE+LESSTHAN25K+BET25KTO50K+BET100KTO250K+MORETHAN250K, data=ageGate2)

# income focused order w/ weights
reg1 = lm(MIGRATED~LESSTHAN25K+BET25KTO50K+BET100KTO250K+MORETHAN250K, data=ageGate2)
reg2 = lm(MIGRATED~LESSTHAN25K+BET25KTO50K+BET100KTO250K+MORETHAN250K, data=ageGate2, weights=ageGate2$PERWT)
reg3 = lm(MIGRATED~LESSTHAN25K+BET25KTO50K+BET100KTO250K+MORETHAN250K+AGE+FEMALE+BLACK+AMERICANINDIAN_OR_ALASKANATIVE+ASIAN_OR_PACIFICISLANDER+OTHER_OR_MULTIPLE+HISPANIC+CHILD+LESSTHANHIGH+BACHELORSDEG+GRADUATEDEG+DISABILITY+VETERAN+MARRIED+INLABFORCE, data=ageGate2)
reg4 = lm(MIGRATED~LESSTHAN25K+BET25KTO50K+BET100KTO250K+MORETHAN250K+AGE+FEMALE+BLACK+AMERICANINDIAN_OR_ALASKANATIVE+ASIAN_OR_PACIFICISLANDER+OTHER_OR_MULTIPLE+HISPANIC+CHILD+LESSTHANHIGH+BACHELORSDEG+GRADUATEDEG+DISABILITY+VETERAN+MARRIED+INLABFORCE, data=ageGate2, weights=ageGate2$PERWT)
reg5 = lm(MIGRATED~LESSTHAN25K+BET25KTO50K+BET100KTO250K+MORETHAN250K, data=ageGate4)
reg6 = lm(MIGRATED~LESSTHAN25K+BET25KTO50K+BET100KTO250K+MORETHAN250K, data=ageGate4, weights=ageGate4$PERWT)
reg7 = lm(MIGRATED~LESSTHAN25K+BET25KTO50K+BET100KTO250K+MORETHAN250K+AGE+FEMALE+BLACK+AMERICANINDIAN_OR_ALASKANATIVE+ASIAN_OR_PACIFICISLANDER+OTHER_OR_MULTIPLE+HISPANIC+CHILD+LESSTHANHIGH+BACHELORSDEG+GRADUATEDEG+DISABILITY+VETERAN+MARRIED+INLABFORCE, data=ageGate4)
reg8 = lm(MIGRATED~LESSTHAN25K+BET25KTO50K+BET100KTO250K+MORETHAN250K+AGE+FEMALE+BLACK+AMERICANINDIAN_OR_ALASKANATIVE+ASIAN_OR_PACIFICISLANDER+OTHER_OR_MULTIPLE+HISPANIC+CHILD+LESSTHANHIGH+BACHELORSDEG+GRADUATEDEG+DISABILITY+VETERAN+MARRIED+INLABFORCE, data=ageGate4, weights=ageGate4$PERWT)

# Report regression results in a table with corrected standard errors
stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8,
          se=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5), cse(reg6), cse(reg7), cse(reg8)), 
          title="Elderly Migration Regression (Wage) 2009-2011 + 2015-2017", type="text", 
          df=FALSE, digits=3, out="Regression_Results_MEGA.txt")


