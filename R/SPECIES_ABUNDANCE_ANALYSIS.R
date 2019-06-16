
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
## This file is for species abundance analysis ##
#################################################


## get a feel for the data
summary(species_diversity_abundance_analysis)

hist(species_diversity_abundance_analysis$A, breaks=50)

## Start analysis by splitting exotic and native species into two separate dataframes
exotic_A <- species_diversity_abundance_analysis %>% 
  filter(CLASSIFICATION == "E")
exotic_A$BCR_name <- as.factor(exotic_A$BCR_name)


native_A <- species_diversity_abundance_analysis %>%
  filter(CLASSIFICATION == "N")
native_A$BCR_name <- as.factor(native_A$BCR_name)


## run analysis on exotic species
## check histogram again
hist(exotic_A$A,breaks=50)
hist(log(exotic_A$A), breaks=50)

## look at raw data
ggplot(exotic_A, aes(x=A, colour=AGGREGATED_LANDCOVER))+
  geom_density()


## run a gaussian model, transforming species abundance by log transform
tic(
  exotic_A.gauss <- bam(log(A) ~ AGGREGATED_LANDCOVER + s(Week, bs="cc", k=52)  + s(BCR_name, bs="re") +
                           s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING) - 1,
                         family=gaussian(), data=exotic_A, method="GCV.Cp", chunk.size = 75000))
toc()

par(mfrow=c(2,2))
gam.check(exotic_A.gauss, k.rep=1000)
dev.off()

preds.exotic.abundance <- predict(exotic_A.gauss, exclude = "s(BCR_name)", se.fit = TRUE)
plot.exotic.abundance <- data.frame(exotic_A,
                                    mu   = exp(preds.exotic.abundance$fit),
                                    low  = exp(preds.exotic.abundance$fit - 1.96 * preds.exotic.abundance$se.fit),
                                    high = exp(preds.exotic.abundance$fit + 1.96 * preds.exotic.abundance$se.fit))


## run a negbin model, no transforming
tic(
  exotic_A.nb <- bam(A ~ AGGREGATED_LANDCOVER + s(Week, bs="cc", k=52)  + s(BCR_name, bs="re") +
                         s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING) - 1,
                         family=nb(), data=exotic_A, method="GCV.Cp", chunk.size = 75000))
toc()

preds.exotic.abundance.nb <- predict(exotic_A.nb, exclude = "s(BCR_name)", se.fit = TRUE)
plot.exotic.abundance.nb  <- data.frame(exotic_A,
                                    mu   = exp(preds.exotic.abundance.nb$fit),
                                    low  = exp(preds.exotic.abundance.nb$fit - 1.96 * preds.exotic.abundance.nb$se.fit),
                                    high = exp(preds.exotic.abundance.nb$fit + 1.96 * preds.exotic.abundance.nb$se.fit))


## run analysis on native species
## check histogram again
hist(native_A$A, breaks=50)
hist(log(native_A$A), breaks=50)

## look at raw data
ggplot(exotic_A, aes(x=A, colour=AGGREGATED_LANDCOVER))+
  geom_density() 

## run a gaussian model again, transforming species abundance by log transform
tic(
  native_A.gauss <- bam(log(A) ~ AGGREGATED_LANDCOVER + s(Week, bs="cc", k=52)  + s(BCR_name, bs="re") +
                           s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING) - 1,
                         family=gaussian(), data=native_A, method="GCV.Cp", chunk.size = 75000))
toc()

par(mfrow=c(2,2))
gam.check(native_A.gauss, k.rep=1000)
dev.off()

preds.native.abundance <- predict(native_A.gauss, exclude = "s(BCR_name)", se.fit = TRUE)
plot.native.abundance <- data.frame(native_A,
                                    mu   = exp(preds.native.abundance$fit),
                                    low  = exp(preds.native.abundance$fit - 1.96 * preds.native.abundance$se.fit),
                                    high = exp(preds.native.abundance$fit + 1.96 * preds.native.abundance$se.fit))


## run a negbin model
tic(
  native_A.nb <- bam(A ~ AGGREGATED_LANDCOVER + s(Week, bs="cc", k=52)  + s(BCR_name, bs="re") +
                          s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING) - 1,
                        family=nb(), data=native_A, method="GCV.Cp", chunk.size = 75000))
toc()


preds.native.abundance.nb <- predict(native_A.nb, exclude = "s(BCR_name)", se.fit = TRUE)
plot.native.abundance.nb <- data.frame(native_A,
                                    mu   = exp(preds.native.abundance.nb$fit),
                                    low  = exp(preds.native.abundance.nb$fit - 1.96 * preds.native.abundance.nb$se.fit),
                                    high = exp(preds.native.abundance.nb$fit + 1.96 * preds.native.abundance.nb$se.fit))

## combine the two abundance files into one for plotting
plot.species.abundance <- rbind(plot.native.abundance, plot.exotic.abundance)


## plot of mean predicted species abundance +/- standard error for native and exotics
pa <- plot.species.abundance %>%
  mutate(CLASSIFICATION = gsub("E", "Exotic Abundance", .$CLASSIFICATION)) %>%
  mutate(CLASSIFICATION = gsub("N", "Native Abundance", .$CLASSIFICATION)) %>%
  group_by(CLASSIFICATION, AGGREGATED_LANDCOVER) %>%
  summarise(mean=mean(mu), sd=sd(mu)) %>%
  mutate(se=sd/n()) %>%
  ggplot(., aes(x=AGGREGATED_LANDCOVER, y=mean))+
  geom_point()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se))+
  xlab("")+
  ylab("Predicted Species Abundance")+
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
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(~CLASSIFICATION, scales="free")

ggsave(pa, filename="H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Submissions/Landscape Ecology/Figures/predicted_abundance.png",
       width=6, height=3.1, units="in")

## extract parameter estimates for landcover for both models and merge to plot
exotic.abundance.lc <- termplot(exotic_A.gauss, se=TRUE, plot=FALSE)$AGGREGATED_LANDCOVER
exotic.abundance.lc$variable <- 'Exotic Abundance'

native.abundance.lc <- termplot(native_A.gauss, se=TRUE, plot=FALSE)$AGGREGATED_LANDCOVER
native.abundance.lc$variable <- 'Native Abundance'

## landcover
landcover <- rbind(exotic.abundance.lc, native.abundance.lc)
landcover$y <- exp(landcover$y)
landcover$se <- exp(landcover$se)
landcover$upr <- landcover$y+(1.96*landcover$se)
landcover$lwr <- landcover$y-(1.96*landcover$se)

## plot parameter estimates for landcover in analysis of species richness
pea <- ggplot(landcover, aes(x=x, y=y))+
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

ggsave(pea, filename="H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Submissions/Landscape Ecology/Figures/parameter_abundance.png",
       width=6, height=3.1, units="in")


### making and exporting plot for publication
library(cowplot)
library(ggpubr)

abundance_figure <- plot_grid(pa, pea,
                              labels=c("a)", "b)"),
                              ncol=1, nrow=2)

setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Figures/Figure 2")

ggexport(abundance_figure, filename="Figure_2.png", width=7100, height=5350, res=600)



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
anova.gam(native_A.nb)
summary(native_A.nb)

gamOut(native_A.nb, "H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Appendices/Appendix 2/native_abundance.csv")


### native
anova.gam(exotic_A.nb)
summary(exotic_A.nb)


gamOut(exotic_A.nb, "H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Appendices/Appendix 2/exotic_abundance.csv")


rm(exotic_A)
rm(native_A)
rm(plot.exotic.abundance.nb)
rm(plot.native.abundance.nb)
rm(species_diversity_abundance_analysis)
rm(species_richness_analysis)

save.image("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/Model results/species_abundance_model.RData")





################################################################################################################
################################################################################################################
################### Run a second analysis, with a focus on seasonal trends #####################################

## run a gaussian model, transforming species diversity by log transform
## first for exotic diversity
tic(
  exotic_A.gauss_seasonal <- bam(log(A) ~ AGGREGATED_LANDCOVER + s(Week, by=as.factor(AGGREGATED_LANDCOVER), bs="cc", k=52)  + s(BCR_name, bs="re") +
                                    s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING) - 1,
                                  family=gaussian(), data=exotic_A, method="GCV.Cp", chunk.size = 75000))
toc()


plotting_data <- plot.gam(exotic_A.gauss_seasonal, page=1)

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
  native_A.gauss_seasonal <- bam(log(A) ~ AGGREGATED_LANDCOVER + s(Week, by=as.factor(AGGREGATED_LANDCOVER), bs="cc", k=52) + 
                                    s(BCR_name, bs="re") + s(LATITUDE, LONGITUDE) + s(DURATION_SAMPLING) - 1,
                                  family=gaussian(), data=native_A, method="GCV.Cp", chunk.size = 75000))
toc()


plotting_data <- plot.gam(native_A.gauss_seasonal, page=1)

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



## Make a plot of temporal patterns for abundance
bind_rows(exotic_temporal_plots, native_temporal_plots) %>%
  ggplot(., aes(x=x, y=y, color=Landcover))+
  geom_line()+
  facet_wrap(~Classification, scales="free")+
  xlim(0,53)+
  xlab("Week")+
  ylab("Estimated smooths")+
  theme(legend.position="bottom")+
  ggtitle("Abundance")


ggsave(filename="H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Submissions/Landscape Ecology/Figures/abundance_temporal_smooth.png", 
       width=8.7, height=5, units="in")

