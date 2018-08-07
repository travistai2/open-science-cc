

####################
#
# Modelling Almetric mentions
#
####################

rm(list=ls())

## load packages
library(dplyr)
library(tidyr)
library(lme4)
library(here)

setwd(here())

## load Almetric dataset
## ***This is private Altmetric data that we cannot provide. Please refer to the query.md for Altmetric search terms***
load('data/altmetric_OA_clean.Rdata')


## Subset to relevant columns and average mentions for each journal in each year
mentions<-alt %>% mutate(OA = ifelse(OA == TRUE, 'Open', 'Closed')) %>%
	group_by(SJRfac, OA, Journal, year) %>% 
	summarise(news = mean(News.mentions), 
				policy=mean(Policy.mentions), 
				twitter=mean(Twitter.mentions)) %>%
	gather(source, mentions, -SJRfac, -OA, -Journal, -year) 

## scale journal factor
mentions$SJRfac.scaled<-scale(as.numeric(mentions$SJRfac))

## drop zero mention entries
mentions<-mentions[mentions$mentions!=0,]

## log10 transform for modelling
mentions$mentions10<-log10(mentions$mentions)


names(mentions)
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
## LMER with gaussian distribution

## News mentions
m.news<-lmer(News.mentions ~ SJR * OA + (1 | year) + (1 | Journal), mentions)
summary(m.news)
hist(resid(m.news))

## Twitter mentions
m.twitter<-lmer(Twitter.mentions ~ SJR * OA + (1 | year) + (1 | Journal), mentions)
summary(m.twitter)
hist(resid(m.twitter))

## Policy mentions
m.policy<-lmer(Policy.mentions ~ SJR * OA + (1 | year) + (1 | Journal), mentions)
summary(m.policy)
hist(resid(m.policy))


## make predictions for Fig. 2
news.dat<-expand.grid(OA = unique(mentions$OA), SJRfac.scaled=unique(mentions$SJRfac.scaled), year = 2010, Journal='Nature')
news.dat$p<-predict(news, newdat=news.dat, re.form=NA, type='response')
news.dat$p10<-10^news.dat$p
news.dat$source<-'news'

twitter.dat<-expand.grid(OA = unique(mentions$OA), SJRfac.scaled=unique(mentions$SJRfac.scaled), year = 2010, Journal='Nature')
twitter.dat$p<-predict(twitter, newdat=twitter.dat, re.form=NA, type='response')
twitter.dat$p10<-10^twitter.dat$p
twitter.dat$source<-'twitter'

policy.dat<-expand.grid(OA = unique(mentions$OA), SJRfac.scaled=unique(mentions$SJRfac.scaled), year = 2010, Journal='Nature')
policy.dat$p<-predict(policy, newdat=policy.dat, re.form=NA, type='response')
policy.dat$p10<-10^policy.dat$p
policy.dat$source<-'policy'

predicted.mentions<-rbind(news.dat, twitter.dat, policy.dat)
predicted.mentions$xlim<-ifelse(predicted.mentions$OA=='Open', 
	as.numeric(predicted.mentions$SJRfac.scaled)+0.1, as.numeric(predicted.mentions$SJRfac)-0.1)


save(predicted.mentions, m.news, m.twitter, m.policy, file='data/altmetric_lmer_fit.Rdata')
## this Rdata is used for Fig 2
