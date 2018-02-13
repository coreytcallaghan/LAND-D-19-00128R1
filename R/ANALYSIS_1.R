## setwd 
setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data")

## load data
load("Data for Analysis/ANALYSIS_1_DATA.RData")


## packages
library(dplyr)
library(mgcv)
library(ggplot2)
library(stringr) 




## get a feel for the data
summary(species_richness_analysis)
summary(species_diversity_abundance_analysis)

hist(species_richness_analysis$SR, breaks=50) # possion or negbin?
hist(species_diversity_abundance_analysis$A, breaks=50)
hist(species_diversity_abundance_analysis$EFFECTIVE_SD, breaks=50)

####################################################
## Let's conduct species richness analysis first! ##
####################################################

## Start analysis by splitting exotic and native species into two separate dataframes
exotic_SR <- species_richness_analysis %>% 
  filter(CLASSIFICATION == "E")

native_SR <- species_richness_analysis %>%
  filter(CLASSIFICATION == "N")

## run analysis on exotic species
## check histogram again
hist(exotic_SR$SR)
hist(log(exotic_SR$SR))

hist(native_SR$SR)
hist(log(native_SR$SR))

## look at raw data
ggplot(exotic_SR, aes(x=SR, colour=AGGREGATED_LANDCOVER))+
  geom_density() 

## randomly sample 5,000 observations from each BCR to run model on
exotic_SR_test <- exotic_SR %>%
  group_by(BCR_name) %>%
  sample_n(5000)

## in order to treat BCR as a random effect, it needs to be specified as a factor
exotic_SR_test$BCR_name <- as.factor(exotic_SR_test$BCR_name)

## Try a negative binomial model first
exotic_SR.nb <- gam(SR ~ AGGREGATED_LANDCOVER + SEASON  + s(BCR_name, bs="re") +
                      s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING),
                    family=nb(), data=exotic_SR_test)

## Some diagnostics
par(mfrow=c(2,2))
gam.check(exotic_SR.nb, k.rep=1000)

## Try a possion to compare with the negbin
exotic_SR.p <- gam(SR ~ AGGREGATED_LANDCOVER + SEASON + s(BCR_name, bs="re") +
                      s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING),
                    family=poisson(), data=exotic_SR_test)

## Some diagnostics
par(mfrow=c(2,2))
gam.check(exotic_SR.p, k.rep=1000)


# Let's just check the AIC of the two models (poission and negbin)
AIC(exotic_SR.nb)
AIC(exotic_SR.p)


#####################################################################################
## run analysis on native species
## check histogram again
hist(native_SR$SR)

## look at raw data
ggplot(native_SR, aes(x=SR, colour=AGGREGATED_LANDCOVER))+
  geom_density() 

## randomly sample 5,000 observations from each BCR to run model on
native_SR_test <- native_SR %>%
  group_by(BCR_name) %>%
  sample_n(5000)

## in order to treat BCR as a random effect, it needs to be specified as a factor
native_SR_test$BCR_name <- as.factor(native_SR_test$BCR_name)

## Try a negative binomial model first
native_SR.nb <- gam(SR ~ AGGREGATED_LANDCOVER + SEASON + s(BCR_name, bs="re") +
                      s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING),
                    family=nb(), data=native_SR_test)

## Some diagnostics
par(mfrow=c(2,2))
gam.check(native_SR.nb, k.rep=1000)

## Try a possion to compare with the negbin
native_SR.p <- gam(SR ~ AGGREGATED_LANDCOVER + SEASON + s(BCR_name, bs="re") +
                     s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING),
                   family=poisson(), data=native_SR_test)

## Some diagnostics
par(mfrow=c(2,2))
gam.check(native_SR.p, k.rep=1000)


# Let's just check the AIC of the two models (poission and negbin)
AIC(native_SR.nb)
AIC(native_SR.p)

## Now, let's predict mu from the model and plot it
#### First for exotic
preds.exotic.richness.nb   <- predict(exotic_SR.nb , se.fit = TRUE)
plot.exotic.richness <- data.frame(exotic_SR_test,
                                    mu   = exp(preds.exotic.richness.nb$fit),
                                    low  = exp(preds.exotic.richness.nb$fit - 1.96 * preds.exotic.richness.nb$se.fit),
                                    high = exp(preds.exotic.richness.nb$fit + 1.96 * preds.exotic.richness.nb$se.fit))

par(mfrow=c(1,1))

ggplot(plot.exotic.richness, aes(x=AGGREGATED_LANDCOVER, y=mu, fill=AGGREGATED_LANDCOVER))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=18, size=3)+
  coord_flip()

#### second for native
preds.native.richness.nb   <- predict(native_SR.nb , se.fit = TRUE)
plot.native.richness <- data.frame(native_SR_test,
                                   mu   = exp(preds.native.richness.nb$fit),
                                   low  = exp(preds.native.richness.nb$fit - 1.96 * preds.native.richness.nb$se.fit),
                                   high = exp(preds.native.richness.nb$fit + 1.96 * preds.native.richness.nb$se.fit))

par(mfrow=c(1,1))

ggplot(plot.native.richness, aes(x=AGGREGATED_LANDCOVER, y=mu, fill=AGGREGATED_LANDCOVER))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=18, size=3)+
  coord_flip()

## combine the two richness files into one for plotting
plot.species.richness <- rbind(plot.native.richness, plot.exotic.richness)

## Boxplot landcover ##
ggplot(plot.species.richness, aes(x=AGGREGATED_LANDCOVER, y=log(mu), fill=CLASSIFICATION))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, position=position_dodge(width=0.75))+
  scale_fill_manual(values=c("gray41", "gray81"))+
  xlab("")+
  ylab("Log(Predicted Species Richness)")+
  theme_bw()+
  scale_x_discrete(limits=c("Natural Green Area", "Agriculture", "Urban Green Area", "Open-urban", 
                            "Low-intensity Developed", "Medium/high-intensity Developed"), 
                   labels=function(x) str_wrap(x, width=15))+
  theme(axis.text.x=element_text(size=12, color="black"))+
  theme(axis.text.y=element_text(size=12, color='black'))+
  theme(axis.title.y=element_text(size=15, vjust=1))+
  theme(panel.border = element_rect(linetype = "solid", colour = "black"))+
  theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
  theme(legend.background=element_rect(linetype="solid", color="black"))+
  theme(legend.title=element_text(size=14))+
  theme(legend.text=element_text(size=12))+
  guides(fill=FALSE)

#################################################################
#################################################################
#################################################################
#### repeat the above process, but for species diversity ########
#################################################################
#################################################################
#################################################################
## Start analysis by splitting exotic and native species into two separate dataframes
exotic_SD <- species_diversity_abundance_analysis %>% 
  filter(CLASSIFICATION == "E")

native_SD <- species_diversity_abundance_analysis %>%
  filter(CLASSIFICATION == "N")

## run analysis on exotic species
## check histogram again
hist(log(exotic_SD$EFFECTIVE_SD))

## look at raw data
ggplot(exotic_SD, aes(x=EFFECTIVE_SD, colour=AGGREGATED_LANDCOVER))+
  geom_density() 

## randomly sample 5,000 observations from each BCR to run model on
exotic_SD_test <- exotic_SD %>%
  group_by(BCR_name) %>%
  sample_n(5000)

## in order to treat BCR as a random effect, it needs to be specified as a factor
exotic_SD_test$BCR_name <- as.factor(exotic_SD_test$BCR_name)

## Try a negative binomial model first
exotic_SD.nb <- gam(EFFECTIVE_SD ~ AGGREGATED_LANDCOVER + SEASON  + s(BCR_name, bs="re") +
                      s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING),
                    family=nb(), data=exotic_SD_test)

## Some diagnostics
par(mfrow=c(2,2))
gam.check(exotic_SD.nb, k.rep=1000)

## Try a possion to compare with the negbin
exotic_SD.p <- gam(EFFECTIVE_SD ~ AGGREGATED_LANDCOVER + SEASON + s(BCR_name, bs="re") +
                     s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING),
                   family=poisson(), data=exotic_SD_test)

## Some diagnostics
par(mfrow=c(2,2))
gam.check(exotic_SD.p, k.rep=1000)


# Let's just check the AIC of the two models (poission and negbin)
AIC(exotic_SD.nb)
AIC(exotic_SD.p)


#####################################################################################
## run analysis on native species
## check histogram again
hist(native_SD$EFFECTIVE_SD)

## look at raw data
ggplot(native_SD, aes(x=EFFECTIVE_SD, colour=AGGREGATED_LANDCOVER))+
  geom_density() 

## randomly sample 5,000 observations from each BCR to run model on
native_SD_test <- native_SD %>%
  group_by(BCR_name) %>%
  sample_n(5000)

## in order to treat BCR as a random effect, it needs to be specified as a factor
native_SD_test$BCR_name <- as.factor(native_SD_test$BCR_name)

## Try a negative binomial model first
native_SD.nb <- gam(EFFECTIVE_SD ~ AGGREGATED_LANDCOVER + SEASON + s(BCR_name, bs="re") +
                      s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING),
                    family=nb(), data=native_SD_test)

## Some diagnostics
par(mfrow=c(2,2))
gam.check(native_SD.nb, k.rep=1000)

## Try a possion to compare with the negbin
native_SD.p <- gam(EFFECTIVE_SD ~ AGGREGATED_LANDCOVER + SEASON + s(BCR_name, bs="re") +
                     s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING),
                   family=poisson(), data=native_SD_test)

## Some diagnostics
par(mfrow=c(2,2))
gam.check(native_SD.p, k.rep=1000)


# Let's just check the AIC of the two models (poission and negbin)
AIC(native_SD.nb)
AIC(native_SD.p)

## Now, let's predict mu from the model and plot it
#### First for exotic
preds.exotic.diversity.nb   <- predict(exotic_SD.nb , se.fit = TRUE)
plot.exotic.diversity <- data.frame(exotic_SD_test,
                                   mu   = exp(preds.exotic.diversity.nb$fit),
                                   low  = exp(preds.exotic.diversity.nb$fit - 1.96 * preds.exotic.diversity.nb$se.fit),
                                   high = exp(preds.exotic.diversity.nb$fit + 1.96 * preds.exotic.diversity.nb$se.fit))

par(mfrow=c(1,1))

ggplot(plot.exotic.diversity, aes(x=AGGREGATED_LANDCOVER, y=mu, fill=AGGREGATED_LANDCOVER))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=18, size=3)+
  coord_flip()

#### second for native
preds.native.diversity.nb   <- predict(native_SD.nb , se.fit = TRUE)
plot.native.diversity <- data.frame(native_SD_test,
                                   mu   = exp(preds.native.diversity.nb$fit),
                                   low  = exp(preds.native.diversity.nb$fit - 1.96 * preds.native.diversity.nb$se.fit),
                                   high = exp(preds.native.diversity.nb$fit + 1.96 * preds.native.diversity.nb$se.fit))

par(mfrow=c(1,1))

ggplot(plot.native.richness, aes(x=AGGREGATED_LANDCOVER, y=mu, fill=AGGREGATED_LANDCOVER))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=18, size=3)+
  coord_flip()

## combine the two richness files into one for plotting
plot.species.diversity <- rbind(plot.native.diversity, plot.exotic.diversity)

## Boxplot landcover ##
ggplot(plot.species.diversity, aes(x=AGGREGATED_LANDCOVER, y=log(mu), fill=CLASSIFICATION))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, position=position_dodge(width=0.75))+
  scale_fill_manual(values=c("gray41", "gray81"))+
  xlab("")+
  ylab("Log(Predicted Effective Species Diversity)")+
  theme_bw()+
  scale_x_discrete(limits=c("Natural Green Area", "Agriculture", "Urban Green Area", "Open-urban", 
                            "Low-intensity Developed", "Medium/high-intensity Developed"), 
                   labels=function(x) str_wrap(x, width=15))+
  theme(axis.text.x=element_text(size=12, color="black"))+
  theme(axis.text.y=element_text(size=12, color='black'))+
  theme(axis.title.y=element_text(size=15, vjust=1))+
  theme(panel.border = element_rect(linetype = "solid", colour = "black"))+
  theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
  theme(legend.background=element_rect(linetype="solid", color="black"))+
  theme(legend.title=element_text(size=14))+
  theme(legend.text=element_text(size=12))+
  guides(fill=FALSE)




