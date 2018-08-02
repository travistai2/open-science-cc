
########################
## CC Open science
#
## Figure 2
#
## Citations and mentions by journal bin
########################

rm(list=ls())
library(here)
setwd(here())



## load libraries
library(dplyr)
library(tidyr)
library(plotrix)
library(lme4)
library(scales)

## plotting functions
se<-function(x) sd(x)/sqrt(length(x))
add_label <- function(xfrac, yfrac, label, pos = 4, ...){
  u <- par("usr")
  x <- u[1] + xfrac * (u[2] - u[1])
  y <- u[4] - yfrac * (u[4] - u[3])
  text(x, y, label, pos = pos, ...)
}



## load scopus data
load('data/scopus_glmerfit.Rdata')

## create prediction dataframe
pred<-expand.grid(OA =unique(mod.dat$OA), jour.bin.scaled=unique(mod.dat$jour.bin.scaled), Year = 2007, Source.title='Nature')
pred$p<-predict(fit, newdata=pred, re.form = NA)
pred$xlim<-ifelse(pred$OA==TRUE, as.numeric(pred$jour.bin.scaled)+0.1, as.numeric(pred$jour.bin.scaled)-0.1)

## pred lines for average citation rate
pred2<-expand.grid(OA =unique(mod.dat$OA), jour.bin.scaled=0, Year = 2007, Source.title='Nature')
scopus.avg<-predict(fit, newdata=pred2, re.form = NA)



## load altmetric data
load('data/altmetric_glmer_fit.Rdata')

## pred lines for average mention rate
pred2<-expand.grid(OA =unique(alt$OA), SJRfac.scaled=0, Year = 2007, Source.title='Nature')
news.avg<-predict(m.news, newdata=pred2, re.form = NA, type='response')
twitter.avg<-predict(m.twitter, newdata=pred2, re.form = NA, type='response')
policy.avg<-predict(m.policy, newdata=pred2, re.form = NA, type='response')



## plot 
pdf(file='figures/Figure2.pdf', height=2, width=8)

## plotting info
layout(matrix(c(1,2,3,4), nrow=1))
cols<-c('#d8b365', '#5ab4ac')
cx.ax=0.9

par(oma = c(0.5,2,0,0), mgp=c(3,0.6,0),mar=c(1.5,1.5, 1.5, 1.5), xpd=FALSE)

## scopus
with(pred[pred$OA==FALSE,], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0, 
			axes=F, xlim=c(-1.5, 1.5), xlab='', ylab='', ylim=c(1, 1.75),
			scol=cols[1], col=cols[1]))
with(pred[pred$OA==TRUE,], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0,
			scol=cols[2], col=cols[2], add=TRUE))
axis(1, at=sort(unique(mod.dat$jour.bin.scaled)), labels=c('Low', 'Medium', 'High', 'Very high'), cex.axis=cx.ax)
# axis(2, at=seq(1, 1.75, 0.05), labels=c(10^seq(1, 1.75, 0.05)), cex.axis=cx.ax)

### log axes labels
xBig = log10(c(seq(10, 25, 5), seq(25, 50, 5)))
axis(2, at=c(1, log10(25), log10(50)), labels=c(10, 25, 50), cex.axis=cx.ax, tcl=0)
axis(2, xBig , labels=rep("", length(xBig)), tcl=-0.4)
add_label(0.01, 0.1, 'A) Citations', font=2, cex=0.9)
mtext(2, text='Mean #', line=2, cex=0.8)

clip(-10, max(ratio.plot$xlim), 0, 100)
abline(h=scopus.avg[1], col=alpha(cols[1], 0.5), lty=2)
abline(h=scopus.avg[2], col=alpha(cols[2], 0.5), lty=2)

## news
par(mar=c(1.5,1.5, 1.5, 1.5), xpd=FALSE)
with(ratio.plot[ratio.plot$source=='news' & ratio.plot$OA=='Closed',], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0, 
			axes=F, xlim=c(-1.5, 1.5), xlab='', ylab='', ylim=c(0, 1.8),
			scol=cols[1], col=cols[1]))
with(ratio.plot[ratio.plot$source=='news' & ratio.plot$OA=='Open',], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0,
			scol=cols[2], col=cols[2], add=TRUE))
axis(1, at=unique(ratio.plot$SJRfac.scaled), labels=c('Low', 'Medium', 'High', 'Very high'), cex.axis=cx.ax)
axis(2, cex.axis=cx.ax)
add_label(0.01, 0.1, 'B) News', font=2, cex=0.9)

clip(-10, max(ratio.plot$xlim), 0, 100)
abline(h=news.avg[1], col=alpha(cols[1], 0.5), lty=2)
abline(h=news.avg[2], col=alpha(cols[2], 0.5), lty=2)

## twitter
par(mar=c(1.5,1.5, 1.5, 1.5), xpd=FALSE)
with(ratio.plot[ratio.plot$source=='twitter' & ratio.plot$OA=='Closed',], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0, 
			axes=F, xlim=c(-1.5, 1.5), xlab='', ylab='', ylim=c(0,10),
			scol=cols[1], col=cols[1]))
with(ratio.plot[ratio.plot$source=='twitter' & ratio.plot$OA=='Open',], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0,
			scol=cols[2], col=cols[2], add=TRUE))
axis(1, at=unique(ratio.plot$SJRfac.scaled), labels=c('Low', 'Medium', 'High', 'Very high'), cex.axis=cx.ax)
axis(2, cex.axis=cx.ax)
add_label(0.01, 0.1, 'C) Twitter', font=2, cex=0.9)

clip(-10, max(ratio.plot$xlim), 0, 100)
abline(h=twitter.avg[1], col=alpha(cols[1], 0.5), lty=2)
abline(h=twitter.avg[2], col=alpha(cols[2], 0.5), lty=2)



## policy
par(mar=c(1.5,1.5, 1.5, 1.5), xpd=FALSE)
with(ratio.plot[ratio.plot$source=='policy' & ratio.plot$OA=='Closed',], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0, 
			axes=F, xlim=c(-1.5, 1.5), xlab='', ylab='', ylim=c(0.1,0.7),
			scol=cols[1], col=cols[1]))
with(ratio.plot[ratio.plot$source=='policy' & ratio.plot$OA=='Open',], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0,
			scol=cols[2], col=cols[2], add=TRUE))
axis(1, at=unique(ratio.plot$SJRfac.scaled), labels=c('Low', 'Medium', 'High', 'Very high'), cex.axis=cx.ax)
axis(2, cex.axis=cx.ax)
add_label(0.01, 0.1, 'D) Policy', font=2, cex=0.9)

clip(-10, max(ratio.plot$xlim), 0, 100)
abline(h=policy.avg[1], col=alpha(cols[1], 0.5), lty=2)
abline(h=policy.avg[2], col=alpha(cols[2], 0.5), lty=2)


par(xpd=T)
legend('topright', legend=c('Open', 'Closed'), 
	col=rev(cols), pch=19, bty='n', inset=c(-0.05, -0.12), cex=0.9)

dev.off()