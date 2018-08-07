
########################
## CC Open science
#
## Figure 1
#
## OA articles over time 
########################

rm(list=ls())
library(here)
setwd(here())


# load packages
library(plyr)
library(ggplot2)

## load data
## ***This is private Scopus data that we cannot provide. Please refer to the query.md for Scopus search terms***
load("data/scopus_OA_climate_clean.Rdata") 

## summary of open access over time
sum.dat<-ddply(dat,.(Year,bin),summarize,
               OpenArt = length(OA[which(OA==T)]),
               ClosArt = length(OA[which(OA==F)]))
sum.dat$Ratio<-with(sum.dat, OpenArt/ClosArt)

sum.dat$OpenPer<-with(sum.dat, OpenArt/(OpenArt+ClosArt)*100)
sum.dat$ClosPer<-with(sum.dat, ClosArt/(OpenArt+ClosArt)*100)
sum.dat$ClosDif<-with(sum.dat, ClosPer-100)

sum.totdat<-ddply(dat,.(Year),summarize,
                  OpenArt = length(OA[which(OA==T)]),
                  ClosArt = length(OA[which(OA==F)]))
sum.totdat$Tot<-with(sum.totdat,OpenArt + ClosArt)

p1<-ggplot(sum.dat)
p1f<-p1 + theme_classic() + 
  theme(text = element_text(size=6),
        legend.text = element_text(size=4),
        legend.title = element_text(size=4),
        legend.key.height = unit(6,units="points")) +
  geom_line(aes(x=Year,y=OpenPer,colour=bin),lwd=0.3) + 
  geom_line(aes(x=Year,y=(OpenArt/Tot*100)),data=sum.totdat,
            lwd=0.7) +
  ylab("OA publications (%)") + 
  coord_cartesian(expand=F) + 
  scale_color_discrete(labels = c("Low","Medium","High","Very high"),name="Journal impact")
  


pdf(file="figures/Figure1.pdf", height=2, width=3)
#quartz(height=2,width=3)
p1f
dev.off()











