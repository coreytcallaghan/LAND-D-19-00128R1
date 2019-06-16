## packages
library(dplyr)
library(mgcv)
library(ggplot2)
library(stringr)
library(tictoc)

native <- readRDS("Data/abund_div_native.RDS")
exotic <- readRDS("Data/abund_div_exotic.RDS")

species_diversity_abundance_analysis <- bind_rows(native, exotic)

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
  exotic_SD.gauss <- bam(log(EFFECTIVE_SD) ~ AGGREGATED_LANDCOVER + s(Week, bs="cc", k=52)  + s(BCR_name, bs="re") +
                           s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING) - 1,
                         family=gaussian(), data=exotic_SD, method="GCV.Cp", chunk.size = 75000))
toc()

par(mfrow=c(2,2))
gam.check(exotic_SD.gauss, k.rep=1000)
dev.off()

preds.exotic.diversity <- predict(exotic_SD.gauss, exclude = "s(BCR_name)", se.fit = TRUE)
plot.exotic.diversity <- data.frame(exotic_SD,
                                    mu   = exp(preds.exotic.diversity$fit),
                                    low  = exp(preds.exotic.diversity$fit - 1.96 * preds.exotic.diversity$se.fit),
                                    high = exp(preds.exotic.diversity$fit + 1.96 * preds.exotic.diversity$se.fit))

## run a tweedie model, no transforming
tic(
  exotic_SD.tw <- bam(EFFECTIVE_SD ~ AGGREGATED_LANDCOVER + s(Week, bs="cc", k=52)  + s(BCR_name, bs="re") +
                        s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING),
                      family=tw(), data=exotic_SD, method="GCV.Cp", chunk.size = 75000))
toc()

## create predictions based on model
preds.exotic.diversity.tw <- predict(exotic_SD.tw, exclude = "s(BCR_name)", se.fit = TRUE)
plot.exotic.diversity.tw <- data.frame(exotic_SD,
                                       mu   = exp(preds.exotic.diversity.tw$fit),
                                       low  = exp(preds.exotic.diversity.tw$fit - 1.96 * preds.exotic.diversity.tw$se.fit),
                                       high = exp(preds.exotic.diversity.tw$fit + 1.96 * preds.exotic.diversity.tw$se.fit))

## compare the two
summary(exotic_SD.gauss)
summary(exotic_SD.tw)


############################################################################
## run analysis on native species
## check histogram again
hist(native_SD$EFFECTIVE_SD)
hist(log(native_SD$EFFECTIVE_SD))

## look at raw data
ggplot(exotic_SD, aes(x=EFFECTIVE_SD, colour=AGGREGATED_LANDCOVER))+
  geom_density() 



## run a gaussian model again, transforming species diversity by log transform
tic(
  native_SD.gauss <- bam(log(EFFECTIVE_SD) ~ AGGREGATED_LANDCOVER + s(Week, bs="cc", k=52)  + s(BCR_name, bs="re") +
                           s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING) - 1,
                         family=gaussian(), data=native_SD, method="GCV.Cp", chunk.size = 75000))
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



## combine the two diversity files into one for plotting
plot.species.diversity <- rbind(plot.native.diversity, plot.exotic.diversity)


## plot of mean predicted effective species diversity +/- standard error for native and exotics
pd <- plot.species.diversity %>%
  mutate(CLASSIFICATION = gsub("E", "Exotic Diversity", .$CLASSIFICATION)) %>%
  mutate(CLASSIFICATION = gsub("N", "Native Diversity", .$CLASSIFICATION)) %>%
  group_by(CLASSIFICATION, AGGREGATED_LANDCOVER) %>%
  summarise(mean=mean(mu), sd=sd(mu)) %>%
  mutate(se=sd/n()) %>%
  ggplot(., aes(x=AGGREGATED_LANDCOVER, y=mean))+
  geom_point()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5)+
  xlab("")+
  ylab("Predicted Effective Species Diversity")+
  theme_bw()+
  scale_x_discrete(limits=c("Natural Green Area", "Agriculture", "Urban Green Area", "Open-urban", 
                            "Low-intensity Developed", "Medium/high-intensity Developed"), 
                   labels=c("Nat. Green", "Ag.", "Urb. Green", "Open Urb.", "Low dev.", "Med/high dev."))+
  theme(axis.text.x=element_text(size=8, color="black", angle=45, hjust=1))+
  theme(axis.text.y=element_text(size=12, color='black'))+
  theme(axis.title.y=element_text(size=12, vjust=1))+
  theme(panel.border = element_rect(linetype = "solid", colour = "black"))+
  theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="gray83"))+
  theme(legend.background=element_rect(linetype="solid", color="black"))+
  theme(legend.title=element_text(size=14))+
  theme(legend.text=element_text(size=12))+
  guides(fill=FALSE)+
  facet_wrap(~CLASSIFICATION, scales="free")

ggsave(pd, filename="H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Submissions/Landscape Ecology/Figures/predicted_diversity.png",
       width=6, height=4, units="in")

## extract parameter estimates for landcover for both models and merge to plot
exotic.diversity.lc <- termplot(exotic_SD.gauss, se=TRUE, plot=FALSE)$AGGREGATED_LANDCOVER
exotic.diversity.lc$variable <- 'Exotic Diversity'

native.diversity.lc <- termplot(native_SD.gauss, se=TRUE, plot=FALSE)$AGGREGATED_LANDCOVER
native.diversity.lc$variable <- 'Native Diversity'

## landcover
landcover <- rbind(exotic.diversity.lc, native.diversity.lc)
landcover$y <- exp(landcover$y)
landcover$se <- exp(landcover$se)
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
                   labels=c("Nat. Green", "Ag.", "Urb. Green", "Open Urb.", "Low dev.", "Med/high dev."))+
  theme(axis.text.x=element_text(size=8, color="black", angle=45, hjust=1))+
  theme(axis.text.y=element_text(size=12, color='black'))+
  theme(axis.title.y=element_text(size=12, vjust=1))+
  theme(panel.border = element_rect(linetype = "solid", colour = "black"))+
  theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="gray83"))+
  facet_wrap(~variable, scales="free")

ggsave(ped, filename="H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Submissions/Landscape Ecology/Figures/parameter_diversity.png",
       width=6, height=3.1, units="in")

### making and exporting plot for publication
library(cowplot)
library(ggpubr)

diversity_figure <- plot_grid(pd, ped,
                              labels=c("a)", "b)"),
                              ncol=1, nrow=2)

setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Figures/Figure 4")

ggexport(diversity_figure, filename="Figure_4.png", width=6500, height=4567, res=600)




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
anova.gam(native_SD.gauss)
summary(native_SD.gauss)

gamOut(native_SD.gauss, "H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Appendices/Appendix 2/native_diversity.csv")


### native
anova.gam(exotic_SD.gauss)
summary(exotic_SD.gauss)

gamOut(exotic_SD.gauss, "H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Appendices/Appendix 2/exotic_diversity.csv")


rm(exotic_SD)
rm(native_SD)
rm(plot.exotic.diversity)
rm(plot.native.diversity)
rm(species_diversity_abundance_analysis)
rm(species_richness_analysis)

save.image("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/Model results/species_diversity_model.RData")









################################################################################################################
################################################################################################################
################### Run a second analysis, with a focus on seasonal trends #####################################

## run a gaussian model, transforming species diversity by log transform
## first for exotic diversity
tic(
  exotic_SD.gauss_seasonal <- bam(log(EFFECTIVE_SD) ~ AGGREGATED_LANDCOVER + s(Week, by=as.factor(AGGREGATED_LANDCOVER), bs="cc", k=52)  + s(BCR_name, bs="re") +
                           s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING) - 1,
                         family=gaussian(), data=exotic_SD, method="GCV.Cp", chunk.size = 75000))
toc()


plotting_data <- plot.gam(exotic_SD.gauss_seasonal, page=1)

Agriculture_exotic <- data.frame(x=plotting_data[[1]]$x,
                                 y=plotting_data[[1]]$fit,
                                 se=plotting_data[[1]]$se,
                                 Landcover="Agriculture",
                                 Classification="Exotic")

Low_dev_exotic <- data.frame(x=plotting_data[[2]]$x,
                                 y=plotting_data[[2]]$fit,
                                 se=plotting_data[[2]]$se,
                                 Landcover="Low-intensity Developed",
                                 Classification="Exotic")

Med_high_dev_exotic <- data.frame(x=plotting_data[[3]]$x,
                                 y=plotting_data[[3]]$fit,
                                 se=plotting_data[[3]]$se,
                                 Landcover="Medium/High-intensity Developed",
                                 Classification="Exotic")

Natural_green_exotic <- data.frame(x=plotting_data[[4]]$x,
                                 y=plotting_data[[4]]$fit,
                                 se=plotting_data[[4]]$se,
                                 Landcover="Natural Green Area",
                                 Classification="Exotic")

Open_urban_exotic <- data.frame(x=plotting_data[[5]]$x,
                                 y=plotting_data[[5]]$fit,
                                 se=plotting_data[[5]]$se,
                                 Landcover="Open-urban",
                                 Classification="Exotic")

Urban_green_exotic <- data.frame(x=plotting_data[[6]]$x,
                                 y=plotting_data[[6]]$fit,
                                 se=plotting_data[[6]]$se,
                                 Landcover="Urban Green Area",
                                 Classification="Exotic")

exotic_temporal_plots <- bind_rows(Agriculture_exotic, Low_dev_exotic,
                                   Med_high_dev_exotic, Natural_green_exotic,
                                   Open_urban_exotic, Urban_green_exotic)

## then for native diversity
tic(
  native_SD.gauss_seasonal <- bam(log(EFFECTIVE_SD) ~ AGGREGATED_LANDCOVER + s(Week, by=as.factor(AGGREGATED_LANDCOVER), bs="cc", k=52) + 
                                    s(BCR_name, bs="re") + s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING) - 1,
                         family=gaussian(), data=native_SD, method="GCV.Cp", chunk.size = 75000))
toc()


plotting_data <- plot.gam(native_SD.gauss_seasonal, page=1)

Agriculture_native <- data.frame(x=plotting_data[[1]]$x,
                                 y=plotting_data[[1]]$fit,
                                 se=plotting_data[[1]]$se,
                                 Landcover="Agriculture",
                                 Classification="Native")

Low_dev_native <- data.frame(x=plotting_data[[2]]$x,
                             y=plotting_data[[2]]$fit,
                             se=plotting_data[[2]]$se,
                             Landcover="Low-intensity Developed",
                             Classification="Native")

Med_high_dev_native <- data.frame(x=plotting_data[[3]]$x,
                                  y=plotting_data[[3]]$fit,
                                  se=plotting_data[[3]]$se,
                                  Landcover="Medium/High-intensity Developed",
                                  Classification="Native")

Natural_green_native <- data.frame(x=plotting_data[[4]]$x,
                                   y=plotting_data[[4]]$fit,
                                   se=plotting_data[[4]]$se,
                                   Landcover="Natural Green Area",
                                   Classification="Native")

Open_urban_native <- data.frame(x=plotting_data[[5]]$x,
                                y=plotting_data[[5]]$fit,
                                se=plotting_data[[5]]$se,
                                Landcover="Open-urban",
                                Classification="Native")

Urban_green_native <- data.frame(x=plotting_data[[6]]$x,
                                 y=plotting_data[[6]]$fit,
                                 se=plotting_data[[6]]$se,
                                 Landcover="Urban Green Area",
                                 Classification="Native")

native_temporal_plots <- bind_rows(Agriculture_native, Low_dev_native,
                                   Med_high_dev_native, Natural_green_native,
                                   Open_urban_native, Urban_green_native)

## Make a plot of temporal patterns for effective species diversity
bind_rows(exotic_temporal_plots, native_temporal_plots) %>%
  ggplot(., aes(x=x, y=y, color=Landcover))+
  geom_line()+
  facet_wrap(~Classification, scales="free")+
  xlim(0,53)+
  xlab("Week")+
  ylab("Estimated smooths")+
  theme(legend.position="bottom")+
  ggtitle("Effective species diversity")


ggsave(filename="H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Submissions/Landscape Ecology/Figures/diversity_temporal_smooth.png", 
       width=8.7, height=5, units="in")


