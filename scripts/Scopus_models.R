

####################
#
# Modelling Scopus citation rates 
#
####################

rm(list=ls())
library(here)
setwd(here())



## load packages
library(ggplot2)
library(plyr)
library(lme4)


## load data
## ***This is private Scopus data that we cannot provide. Please refer to the query.md for Scopus search terms***
load("data/scopus_OA_climate_clean.Rdata")  ## load scopus data
sub.dat<-dat[,c("Source.title","Year","Cited.by","OA","Open.Access","SJRbin")]

names(sub.dat)
# "Source.title" = journal
# "Year" = publication year
# "Cited.by" = number of citations
# "OA" = open access (TRUE/FALSE)
# "Open.Access" = open access (OPEN/CLOSED)
# "SJRbin" = journal impact factor bin (low, medium, high, very high)

## summarise average citation by journal, by impact bin + access + year
scopus<-ddply(sub.dat,.(Source.title,Year,Open.Access,OA,SJRbin),summarize,
               MeanCite = mean(Cited.by,na.rm=T))

## log10 transform citation response and scale journal bin
## +1 needed for papers without citations
scopus$log10MeanCite<-log10(scopus$MeanCite+1)
scopus$jour.bin.scaled<-scale(as.numeric(scopus$SJRbin))


## Fit mixed effects model ##
## Journal impact bin * open access as fixed effect with interaction
## Publication year and journal title as random intercept
## data frame 'scopus' is provided in scopus_lmer_fit.Rdata

m.citations<-lmer(log10MeanCite ~ jour.bin.scaled*OA + 
							(1|Year) + (1 | Source.title),
							data=scopus)
summary(m.citations)
hist(resid(m.citations))

## save model output
save(m.citations, scopus, file='data/scopus_lmer_fit.Rdata')
## this Rdata is used for Fig 2 panel A



