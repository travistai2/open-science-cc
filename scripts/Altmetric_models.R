

####################
#
# Modelling Almetric mentions
#
####################

rm(list=ls())
setwd("~/GitHub/open-science-cc/")

## load packages
library(dplyr)
library(tidyr)
library(lme4)

## load Almetric dataset
load(file='Data/altmetric_OA_clean.Rdata')


## Subset to relevant columns and average mentions for each journal in each year
ratio<-alt %>% mutate(OA = ifelse(OA == TRUE, 'Open', 'Closed')) %>%
	group_by(SJRfac, OA, Journal, year) %>% 
	summarise(news = mean(News.mentions), 
				policy=mean(Policy.mentions), 
				twitter=mean(Twitter.mentions)) %>%
	gather(source, mentions, -SJRfac, -OA, -Journal, -year) 

## scale journal factor
ratio$SJRfac.scaled<-scale(as.numeric(ratio$SJRfac))

names(means)
# Journal = journal title
# OA = open access (TRUE/FALSE)
# year = publication year
# news = number of news mentions
# policy = number of policy mentions
# twitter = number of twitter mentions
# SJRfac = journal impact factor bin
# SJRfac.scaled = journal impact factor bin scaled

## Fit mixed effects model ##
## Journal impact bin * open access as fixed effect with interaction
## Publication year and journal title as random intercept
## GLMER with Poisson distribution

## News mentions
m.news<-glmer(News.mentions ~ SJR * OA + (1 | year) + (1 | Journal), alt, family='poisson')
summary(m.news)
hist(resid(m.news))

## Twitter mentions
m.twitter<-glmer(Twitter.mentions ~ SJR * OA + (1 | year) + (1 | Journal), alt, family='poisson')
summary(m.twitter)
hist(resid(m.twitter))

## Policy mentions
m.policy<-glmer(Policy.mentions ~ SJR * OA + (1 | year) + (1 | Journal), alt, family='poisson')
summary(m.policy)
hist(resid(m.policy))


save(m.news, m.twitter, m.policy, file='Data/altmetric_glmer_fit.Rdata')