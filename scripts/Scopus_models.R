

####################
#
# Modelling Scopus citation rates 
#
####################

rm(list=ls())
setwd("~/GitHub/open-science-cc/")


## load packages
library(ggplot2)
library(plyr)
library(lme4)


## load data
load("./Data/scopus_OA_climate_clean.Rdata")  ## load scopus data 
sub.dat<-dat[,c("Source.title","Year","Cited.by","OA","Open.Access","jour.bin")]

names(sub.dat)
# "Source.title" = journal
# "Year" = publication year
# "Cited.by" = number of citations
# "OA" = open access (TRUE/FALSE)
# "Open.Access" = open access (OPEN/CLOSED)
# "jour.bin" = journal impact factor bin (low, medium, high, very high)

## summarise average citation by journal, by impact bin + access + year
mod.dat<-ddply(sub.dat,.(Source.title,Year,Open.Access,OA,jour.bin),summarize,
               MeanCite = mean(Cited.by,na.rm=T))

## log10 transform citation response and scale journal bin
mod.dat$log10MeanCite<-log10(mod.dat$MeanCite+1)
mod.dat$jour.bin.scaled<-scale(as.numeric(mod.dat$jour.bin))


## Fit mixed effects model ##
## Journal impact bin * open access as fixed effect with interaction
## Publication year and journal title as random intercept

fit<-lmer(log10MeanCite ~ jour.bin.scaled*OA + 
							(1|Year) + (1 | Source.title),
							data=mod.dat)
summary(fit)
hist(resid(fit))

## save model output
save(fit, mod.dat, file='./Data/scopus_glmerfit.Rdata')




