## setwd 
setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data")

## load data
load("Data for Analysis/ANALYSIS_1_DATA.RData")


## packages
library(dplyr)
library(mgcv)
library(ggplot2)
library(stringr)
library(tictoc)

################################################
## This file is for species richness analysis ##
################################################


## get a feel for the data
summary(species_richness_analysis)

hist(species_richness_analysis$SR, breaks=50) # possion or negbin?


## Start analysis by splitting exotic and native species into two separate dataframes
exotic_SR <- species_richness_analysis %>% 
  filter(CLASSIFICATION == "E")
exotic_SR$BCR_name <- as.factor(exotic_SR$BCR_name)


native_SR <- species_richness_analysis %>%
  filter(CLASSIFICATION == "N")
native_SR$BCR_name <- as.factor(native_SR$BCR_name)

###########################################
#### run analysis on exotic species ####

## check histogram again
hist(exotic_SR$SR, breaks=50)
hist(log(exotic_SR$SR))

## look at raw data
ggplot(exotic_SR, aes(x=SR, colour=AGGREGATED_LANDCOVER))+
  geom_density() 

## run a gaussian model, transforming species richness by log transform
tic(
exotic_SR.gauss <- bam(log(SR) ~ AGGREGATED_LANDCOVER + SEASON  + s(BCR_name, bs="re") +
                         s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING),
                       family=gaussian(), data=exotic_SR, chunk.size = 75000))
toc()

## model checking
par(mfrow=c(2,2))
gam.check(exotic_SR.gauss, k.rep=1000)
dev.off()

## create predictions based on model
preds.exotic.richness <- predict(exotic_SR.gauss, exclude = "s(BCR_name)", se.fit = TRUE)
plot.exotic.richness <- data.frame(exotic_SR,
                                   mu   = exp(preds.exotic.richness$fit),
                                   low  = exp(preds.exotic.richness$fit - 1.96 * preds.exotic.richness$se.fit),
                                   high = exp(preds.exotic.richness$fit + 1.96 * preds.exotic.richness$se.fit))


## run a negbin model, without any transformations
tic(
  exotic_SR.nb <- bam(SR ~ AGGREGATED_LANDCOVER + SEASON  + s(BCR_name, bs="re") +
                           s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING),
                         family=nb(), data=exotic_SR, chunk.size = 75000))
toc()


## create predictions based on model
preds.exotic.richness.nb <- predict(exotic_SR.nb, exclude = "s(BCR_name)", se.fit = TRUE)
plot.exotic.richness.nb <- data.frame(exotic_SR,
                                   mu   = exp(preds.exotic.richness.nb$fit),
                                   low  = exp(preds.exotic.richness.nb$fit - 1.96 * preds.exotic.richness.nb$se.fit),
                                   high = exp(preds.exotic.richness.nb$fit + 1.96 * preds.exotic.richness.nb$se.fit))

########################################
#### run analysis on native species ####

## check histogram again
hist(native_SR$SR, breaks=50)
hist(log(native_SR$SR))

## look at raw data
ggplot(exotic_SR, aes(x=SR, colour=AGGREGATED_LANDCOVER))+
  geom_density() 

## run a gaussian model again, transforming species richness by log transform
tic(
  native_SR.gauss <- bam(log(SR) ~ AGGREGATED_LANDCOVER + SEASON  + s(BCR_name, bs="re") +
                           s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING),
                         family=gaussian(), data=native_SR, chunk.size = 75000))
toc()


## model checking
par(mfrow=c(2,2))
gam.check(native_SR.gauss, k.rep=1000)
dev.off()


## create predictions based on model
preds.native.richness <- predict(native_SR.gauss, exclude = "s(BCR_name)", se.fit = TRUE)
plot.native.richness <- data.frame(native_SR,
                                   mu   = exp(preds.native.richness$fit),
                                   low  = exp(preds.native.richness$fit - 1.96 * preds.native.richness$se.fit),
                                   high = exp(preds.native.richness$fit + 1.96 * preds.native.richness$se.fit))


## run a negbin model, without any transformations
tic(
  native_SR.nb <- bam(SR ~ AGGREGATED_LANDCOVER + SEASON  + s(BCR_name, bs="re") +
                        s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING),
                      family=nb(), data=native_SR, chunk.size = 75000))
toc()



## create predictions based on model
preds.native.richness.nb <- predict(native_SR.nb, exclude = "s(BCR_name)", se.fit = TRUE)
plot.native.richness.nb <- data.frame(native_SR,
                                   mu   = exp(preds.native.richness.nb$fit),
                                   low  = exp(preds.native.richness.nb$fit - 1.96 * preds.native.richness.nb$se.fit),
                                   high = exp(preds.native.richness.nb$fit + 1.96 * preds.native.richness.nb$se.fit))






## combine the two richness files into one for plotting
plot.species.richness <- rbind(plot.native.richness.nb, plot.exotic.richness.nb)


## plot of mean predicted richness +/- standard error for native and exotics
pr <- plot.species.richness %>%
  mutate(CLASSIFICATION = gsub("E", "Exotic Richnes", .$CLASSIFICATION)) %>%
  mutate(CLASSIFICATION = gsub("N", "Native Richness", .$CLASSIFICATION)) %>%
  group_by(CLASSIFICATION, AGGREGATED_LANDCOVER) %>%
  summarise(mean=mean(mu), sd=sd(mu)) %>%
  mutate(se=sd/n()) %>%
  ggplot(., aes(x=AGGREGATED_LANDCOVER, y=mean))+
  geom_point()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se))+
  xlab("")+
  ylab("Predicted Species Richness")+
  theme_bw()+
  scale_x_discrete(limits=c("Natural Green Area", "Agriculture", "Urban Green Area", "Open-urban", 
                            "Low-intensity Developed", "Medium/high-intensity Developed"), 
                   labels=function(x) str_wrap(x, width=15))+
  theme(axis.text.x=element_text(size=10, color="black"))+
  theme(axis.text.y=element_text(size=12, color='black'))+
  theme(axis.title.y=element_text(size=15, vjust=1))+
  theme(panel.border = element_rect(linetype = "solid", colour = "black"))+
  theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="gray83"))+
  theme(legend.background=element_rect(linetype="solid", color="black"))+
  theme(legend.title=element_text(size=14))+
  theme(legend.text=element_text(size=12))+
  guides(fill=FALSE)+
  facet_wrap(~CLASSIFICATION, scales="free")



## extract parameter estimates for landcover for both models and merge to plot
exotic.richness.lc <- termplot(exotic_SR.nb, se=TRUE, plot=FALSE)$AGGREGATED_LANDCOVER
exotic.richness.lc$variable <- 'Exotic Richness'

native.richness.lc <- termplot(native_SR.nb, se=TRUE, plot=FALSE)$AGGREGATED_LANDCOVER
native.richness.lc$variable <- 'Native Richness'

## landcover
landcover <- rbind(exotic.richness.lc, native.richness.lc)
landcover$upr <- landcover$y+(1.96*landcover$se)
landcover$lwr <- landcover$y-(1.96*landcover$se)

## plot parameter estimates for landcover in analysis of species richness
per <- ggplot(landcover, aes(x=x, y=y))+
  geom_point()+
  geom_errorbar(aes(ymax=upr, ymin=lwr))+
  theme(axis.text.x=element_text(angle=40, hjust=1))+
  xlab("")+
  ylab("Parameter Estimate")+
  theme_bw()+
  scale_x_discrete(limits=c("Natural Green Area", "Agriculture", "Urban Green Area", "Open-urban", 
                            "Low-intensity Developed", "Medium/high-intensity Developed"), 
                   labels=function(x) str_wrap(x, width=15))+
  theme(axis.text.x=element_text(size=10, color="black"))+
  theme(axis.text.y=element_text(size=12, color='black'))+
  theme(axis.title.y=element_text(size=15, vjust=1))+
  theme(panel.border = element_rect(linetype = "solid", colour = "black"))+
  theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="gray83"))+
  facet_wrap(~variable, scales="free")


### making and exporting plot for publication
library(cowplot)
library(ggpubr)

richness_figure <- plot_grid(pr, per,
                              labels=c("A", "B"),
                              ncol=1, nrow=2)

setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Figures/Figure 2")

ggexport(richness_figure, filename="Figure_2.png", width=7100, height=5350, res=600)




### results
### exports summary of gam model results exported to separate csvs and then combined by hand into an appendix
gamOut <- function(res, file="test.csv", ndigit=5, writecsv=T) {
  if (length(grep("summary", class(res)))==0) res <- summary(res)
  co <- res$p.table     # result table
  nvar <- nrow(co)      # No. row as in summary()$coefficients
  ncoll <- ncol(co)     # No. col as in summary()$coefficients
  
  formatter <- function(x) format(round(x,ndigit),nsmall=ndigit)
  nstats <- 4          # sets the number of rows to record the coefficients           
  
  # s table
  StartRow<- nstats+ nvar +2 # starting row of for second table
  st <- res$s.table
  ncol2 <- ncol(st)
  nrow2 <- nrow(st)
  # create a matrix
  G <- matrix("", nrow=(nvar+nstats+StartRow), ncol=(ncoll+1))       # storing data for output
  # equation, deviance and R square
  G[1,1] <- toString(res$formula)
  G[1,2] <- "Deviance explained"  # save AIC value
  G[2,2] <- res$dev.expl
  G[1,3] <- "R-sq. (adj)"
  G[2,3] <- res$r.sq
  # P table
  G[(nstats+1):(nvar+nstats), 1] <- rownames(co) # save rownames and colnames
  G[nstats,  2:(ncoll+1)] <- colnames(co)
  G[(nstats+1):(nvar+nstats), 2:(ncoll+1)] <- formatter(co)  # save coefficients
  # S table 
  G[(StartRow+1):(StartRow+nrow2), 1] <- rownames(st)
  G[(StartRow), 2: (ncol2+1)]         <- colnames(st)
  G[(StartRow+1):(StartRow+nrow2), 2:(ncol2+1)] <- formatter(st)
  
  # for output
  print(G)
  write.csv(G, file=file, row.names=F)
}




### exotic
anova.gam(native_SR.nb)
summary(native_SR.nb)

gamOut(native_SR.nb, "H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Appendices/Appendix 2/native_richness.csv")

### native
anova.gam(exotic_SR.nb)
summary(exotic_SR.nb)

gamOut(exotic_SR.nb, "H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Appendices/Appendix 2/exotic_richness.csv")





