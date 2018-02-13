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

#################################################
## This file is for species diversity analysis ##
#################################################


## get a feel for the data
summary(species_diversity_abundance_analysis)

hist(species_diversity_abundance_analysis$EFFECTIVE_SD, breaks=50)

## Start analysis by splitting exotic and native species into two separate dataframes
exotic_SD <- species_diversity_abundance_analysis %>% 
  filter(CLASSIFICATION == "E")
exotic_SD$BCR_name <- as.factor(exotic_SD$BCR_name)


native_SD <- species_diversity_abundance_analysis %>%
  filter(CLASSIFICATION == "N")
native_SD$BCR_name <- as.factor(native_SD$BCR_name)


## run analysis on exotic species
## check histogram again
hist(exotic_SD$EFFECTIVE_SD)
hist(log(exotic_SD$EFFECTIVE_SD))

## look at raw data
ggplot(exotic_SD, aes(x=EFFECTIVE_SD, colour=AGGREGATED_LANDCOVER))+
  geom_density() 



## run a gaussian model, transforming species diversity by log transform
tic(
  exotic_SD.gauss <- bam(log(EFFECTIVE_SD) ~ AGGREGATED_LANDCOVER + SEASON  + s(BCR_name, bs="re") +
                           s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING),
                         family=gaussian(), data=exotic_SD, chunk.size = 75000))
toc()

par(mfrow=c(2,2))
gam.check(exotic_SD.gauss, k.rep=1000)
dev.off()

preds.exotic.diversity <- predict(exotic_SD.gauss, exclude = "s(BCR_name)", se.fit = TRUE)
plot.exotic.diversity <- data.frame(exotic_SD,
                                   mu   = exp(preds.exotic.diversity$fit),
                                   low  = exp(preds.exotic.diversity$fit - 1.96 * preds.exotic.diversity$se.fit),
                                   high = exp(preds.exotic.diversity$fit + 1.96 * preds.exotic.diversity$se.fit))

## run a negbin model, no transforming
tic(
  exotic_SD.nb <- bam(EFFECTIVE_SD ~ AGGREGATED_LANDCOVER + SEASON  + s(BCR_name, bs="re") +
                           s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING),
                         family=nb(), data=exotic_SD, chunk.size = 75000))
toc()

## create predictions based on model
preds.exotic.diversity.nb <- predict(exotic_SD.nb, exclude = "s(BCR_name)", se.fit = TRUE)
plot.exotic.diversity.nb <- data.frame(exotic_SD,
                                    mu   = exp(preds.exotic.diversity.nb$fit),
                                    low  = exp(preds.exotic.diversity.nb$fit - 1.96 * preds.exotic.diversity.nb$se.fit),
                                    high = exp(preds.exotic.diversity.nb$fit + 1.96 * preds.exotic.diversity.nb$se.fit))




## run analysis on native species
## check histogram again
hist(native_SD$EFFECTIVE_SD)
hist(log(native_SD$EFFECTIVE_SD))

## look at raw data
ggplot(exotic_SD, aes(x=EFFECTIVE_SD, colour=AGGREGATED_LANDCOVER))+
  geom_density() 



## run a gaussian model again, transforming species diversity by log transform
tic(
  native_SD.gauss <- bam(log(EFFECTIVE_SD) ~ AGGREGATED_LANDCOVER + SEASON  + s(BCR_name, bs="re") +
                           s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING),
                         family=gaussian(), data=native_SD, chunk.size = 75000))
toc()

par(mfrow=c(2,2))
gam.check(native_SD.gauss, k.rep=1000)
dev.off()

## create predictions based on model
preds.native.diversity <- predict(native_SD.gauss, exclude = "s(BCR_name)", se.fit = TRUE)
plot.native.diversity <- data.frame(native_SD,
                                   mu   = exp(preds.native.diversity$fit),
                                   low  = exp(preds.native.diversity$fit - 1.96 * preds.native.diversity$se.fit),
                                   high = exp(preds.native.diversity$fit + 1.96 * preds.native.diversity$se.fit))



## run a negbin model, no transforming
tic(
  native_SD.nb <- bam(EFFECTIVE_SD ~ AGGREGATED_LANDCOVER + SEASON  + s(BCR_name, bs="re") +
                        s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING),
                      family=nb(), data=native_SD, chunk.size = 75000))
toc()


## create predictions based on model
preds.native.diversity.nb <- predict(native_SD.nb, exclude = "s(BCR_name)", se.fit = TRUE)
plot.native.diversity.nb <- data.frame(native_SD,
                                    mu   = exp(preds.native.diversity.nb$fit),
                                    low  = exp(preds.native.diversity.nb$fit - 1.96 * preds.native.diversity.nb$se.fit),
                                    high = exp(preds.native.diversity.nb$fit + 1.96 * preds.native.diversity.nb$se.fit))



## combine the two richness files into one for plotting
plot.species.diversity <- rbind(plot.native.diversity.nb, plot.exotic.diversity.nb)


## plot of mean predicted effective species diversity +/- standard error for native and exotics
pd <- plot.species.diversity %>%
  mutate(CLASSIFICATION = gsub("E", "Exotic Diversity", .$CLASSIFICATION)) %>%
  mutate(CLASSIFICATION = gsub("N", "Native Diversity", .$CLASSIFICATION)) %>%
  group_by(CLASSIFICATION, AGGREGATED_LANDCOVER) %>%
  summarise(mean=mean(mu), sd=sd(mu)) %>%
  mutate(se=sd/n()) %>%
  ggplot(., aes(x=AGGREGATED_LANDCOVER, y=mean))+
  geom_point()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se))+
  xlab("")+
  ylab("Predicted Effective Species Diversity")+
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
exotic.diversity.lc <- termplot(exotic_SD.nb, se=TRUE, plot=FALSE)$AGGREGATED_LANDCOVER
exotic.diversity.lc$variable <- 'Exotic Diversity'

native.diversity.lc <- termplot(native_SD.nb, se=TRUE, plot=FALSE)$AGGREGATED_LANDCOVER
native.diversity.lc$variable <- 'Native Diversity'

## landcover
landcover <- rbind(exotic.diversity.lc, native.diversity.lc)
landcover$upr <- landcover$y+(1.96*landcover$se)
landcover$lwr <- landcover$y-(1.96*landcover$se)

## plot parameter estimates for landcover in analysis of species diversity
ped <- ggplot(landcover, aes(x=x, y=y))+
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

diversity_figure <- plot_grid(pd, ped,
          labels=c("A", "B"),
          ncol=1, nrow=2)

setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Figures/Figure 3")

ggexport(diversity_figure, filename="Figure_3.png", width=7100, height=5350, res=600)




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
anova.gam(native_SD.nb)
summary(native_SD.nb)

gamOut(native_SD.nb, "H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Appendices/Appendix 2/native_diversity.csv")


### native
anova.gam(exotic_SD.nb)
summary(exotic_SD.nb)

gamOut(exotic_SD.nb, "H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Appendices/Appendix 2/exotic_diversity.csv")





