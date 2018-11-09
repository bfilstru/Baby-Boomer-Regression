#==============================================================================
#   Data Analysis: import data, summary stats, regression
#==============================================================================

# original scripts by Bill Sundstrom, Michael Kevane and Matt Holian 
# This version combines various earlier scripts and is by Ben Filstrup 11/1/2018
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
ageGate2 = subset(ageGate, GQ!=3 )

#==============================================================================
#   3. Analysis section
#==============================================================================

# Look at basic stats for all the numeric variables in the dataset

# stargazer(data, type="text", summary.stat=c("n", "mean", "median", "sd", "min", "max" ), digits=2, title="SW EE 4.2", out = "Table_EE4.2_sumstats.txt")

# summary statistics for a subsample
stargazer(ageGate2, type="text", summary.stat=c("n", "mean", "median", "sd", "min", "max" ), digits=2, title="Summary Statistics Age Gated", out = "Summary Statistics Age Gated.txt")

# difference in means test

# t.test(migrated~AGE, data=babyBoomersMigration, FUN=c(mean), na.rm=TRUE)

# scatterplot

# ggplot(data=earningsheight, aes(x=height, y=earnings))+geom_point(shape=1)+labs(title="Earnings Height")+labs(x="Height", y="Earnings")

# scatterplot with regression line
ggplot(data=ageGate2, aes(x=LESSTHAN25K, y=MIGRATED))+geom_point(shape=1)+labs(title="Migration Logit")+labs(x="Income less than 25k", y="Migrated")+geom_smooth(method=lm)

# running regressions;
# standard order
# reg1 = lm(MIGRATED~AGE+FEMALE+BLACK+AMERICANINDIAN_OR_ALASKANATIVE+ASIAN_OR_PACIFICISLANDER+OTHER_OR_MULTIPLE+HISPANIC+CHILD+LESSTHANHIGH+BACHELORSDEG+GRADUATEDEG+DISABILITY+VETERAN+MARRIED+INLABFORCE+LESSTHAN25K+BET25KTO50K+BET100KTO250K+MORETHAN250K, data=ageGate2)

# income focused order
# reg1 = lm(MIGRATED~LESSTHAN25K+BET25KTO50K+BET100KTO250K+MORETHAN250K+AGE+FEMALE+BLACK+AMERICANINDIAN_OR_ALASKANATIVE+ASIAN_OR_PACIFICISLANDER+OTHER_OR_MULTIPLE+HISPANIC+CHILD+LESSTHANHIGH+BACHELORSDEG+GRADUATEDEG+DISABILITY+VETERAN+MARRIED+INLABFORCE, data=ageGate2)

# income focused order w/ weights
reg1 = lm(MIGRATED~LESSTHAN25K+BET25KTO50K+BET100KTO250K+MORETHAN250K+AGE+FEMALE+BLACK+AMERICANINDIAN_OR_ALASKANATIVE+ASIAN_OR_PACIFICISLANDER+OTHER_OR_MULTIPLE+HISPANIC+CHILD+LESSTHANHIGH+BACHELORSDEG+GRADUATEDEG+DISABILITY+VETERAN+MARRIED+INLABFORCE, data=ageGate2)
reg2 = lm(MIGRATED~LESSTHAN25K+BET25KTO50K+BET100KTO250K+MORETHAN250K+AGE+FEMALE+BLACK+AMERICANINDIAN_OR_ALASKANATIVE+ASIAN_OR_PACIFICISLANDER+OTHER_OR_MULTIPLE+HISPANIC+CHILD+LESSTHANHIGH+BACHELORSDEG+GRADUATEDEG+DISABILITY+VETERAN+MARRIED+INLABFORCE, data=ageGate2, weights=ageGate2$PERWT)


# Report regression results in a table with corrected standard errors
stargazer(reg1, reg2,
          se=list(cse(reg1), cse(reg2)), 
          title="Elderly Migration Regression (Wage)", type="text", 
          df=FALSE, digits=3, out="Elderly_Migration_Wage")

# summed values from data

# original population size
sum(babyBoomersMigration$DATANUM)

sum(ageGate2$DATANUM)
sum(ageGate2$MIGRATED)
sum(ageGate2$LESSTHAN25K)
sum(ageGate2$MORETHAN250K)
