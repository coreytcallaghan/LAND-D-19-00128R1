library(dplyr)
library(tidyr)
library(ggplot2)

source("R/community_cluster_diagnostics_funs.R")

# load allocations calculate in community_cluster_fit-k-means.R
allocations <- readRDS("Outputs/cluster_allocations/bcr_kmeans_allocations.rds")

# load the localities that were used for clustering
localities_list <- lapply(allocations, function(x) x[["localities"]])



# inspect the sum of squares ----------------------------------------------

# flatten
allocations_diagnostics <- bind_rows(lapply(allocations, flatten_list), .id = "BCR") # this is where the BCR unlisting is done

# add information criterion
allocations_diagnostics_summary <- allocations_diagnostics %>%
  mutate(AIC = total_within_ss + (2 * n_vars * k),
         BIC = total_within_ss + (log(n_obs) * n_vars * k))

# go long for plotting
allocations_diagnostics_plot <- allocations_diagnostics_summary %>%
  select(BCR, k, total_within_ss, AIC, BIC) %>%
  gather(metric, value, total_within_ss:BIC)

ggplot(allocations_diagnostics_plot, aes(x = k, group = metric)) +
  geom_line(aes(y = value, colour = metric)) +
  facet_wrap(~BCR, scales = "free")

ggplot(filter(allocations_diagnostics_plot, metric == "AIC"), aes(x = k, group = metric)) +
  geom_line(aes(y = value, colour = metric)) +
  facet_wrap(~BCR, scales = "free")

ggplot(filter(allocations_diagnostics_plot, metric == "total_within_ss"), aes(x = k, group = metric)) +
  geom_line(aes(y = value, colour = metric)) +
  facet_wrap(~BCR, scales = "free")

### looks like AIC is more informative at this point
### will try re-run with a distance metric on the data?



# choose best clusters ----------------------------------------------------

min_AIC_data <- allocations_diagnostics_summary %>%
  group_by(BCR) %>%
  filter(AIC == min(AIC))

min_AIC_clusters <- purrr::map2(allocations, min_AIC_data$k, retreive_k_clusters)



# compare communities to land cover classification ------------------------

# join the data together
clusters_landcover <- lapply(names(min_AIC_clusters), join_lc_data, min_AIC_clusters, localities_list)
names(clusters_landcover) <- names(min_AIC_clusters)

# entropy values: examine distriubtion of assembleges
# low entropy indicates less even distribution among groups (often indicates gruops with low or no membership)

# urban_diff values: examine whether (proportionally) more of the communities are in non-urban areas
# positive values will mean more proportional membership of sites to the non-urban component


#### urban / non-urban comparison

urban_entropy <- gather(bind_rows(lapply(clusters_landcover, calculate_entropy, type = "urban"),
                                  .id = "BCR"), "zone", "value", -BCR)

ggplot(urban_entropy, aes(y = value, x = zone)) +
  geom_boxplot() +
  geom_point(aes(colour = BCR)) +
  geom_line(aes(group = BCR))

summary(nlme::lme(fixed = value ~ zone, random = ~ 1 | BCR, data = urban_entropy))


urban_urbandiff <- bind_rows(lapply(clusters_landcover, calculate_urban_diff, type = "urban"),
                             .id = "BCR")

ggplot(urban_urbandiff, aes(y = urban_diff, x = BCR)) +
  #geom_violin() +
  geom_point(alpha = 0.5, size = 3) +
  geom_hline(yintercept = 0, colour = "red") +
  theme_classic()


#### land cover comparison

# entropy values - examine distriubtion of assembleges
landcover_entropy <- gather(bind_rows(lapply(clusters_landcover, calculate_entropy, type = "landcover"), 
                                      .id = "BCR"),
                            "zone", "value", -BCR) %>%
  filter(zone %in% c("Natural.Green.Area", "Urban.Green.Area")) %>%
  mutate(zone = gsub("Natural.Green.Area", "Natural Green Area", .$zone)) %>%
  mutate(zone = gsub("Urban.Green.Area", "Urban Green Area", .$zone))

ggplot(landcover_entropy, aes(y = value, x = zone)) +
  geom_violin(fill="azure3") +
  geom_point(aes(colour = BCR)) +
  geom_line(aes(group = BCR)) +
  theme_classic() +
  xlab("") +
  ylab("Shannon diversity of cluster assignments") +
  coord_flip()

summary(nlme::lme(fixed = value ~ zone, random = ~ 1 | BCR, data = landcover_entropy))

# urban_diff values - examine whether (proportionally) more of the communities are in non-urban areas
landcover_urbandiff <- bind_rows(lapply(clusters_landcover, calculate_urban_diff, type = "landcover"),
                                 .id = "BCR")

ggplot(landcover_urbandiff, aes(y = urban_diff, x = BCR)) +
  #geom_violin() +
  geom_point(alpha = 0.5, size = 3) +
  geom_hline(yintercept = 0, colour = "red") +
  theme_classic()





# calculate per-cluster species metrics (i.e. community metrics) ------------------------

clusters_metrics <- lapply(names(min_AIC_clusters), calculate_cluster_metrics,
                           min_AIC_clusters, localities_list)
names(clusters_metrics) <- names(min_AIC_clusters)

cluster_metrics_df <- bind_rows(clusters_metrics, .id = "BCR") %>%
  mutate(cluster = as.integer(cluster))

# join with other data...
cluster_metrics_urbandiff <- inner_join(urban_urbandiff, cluster_metrics_df)
cluster_metrics_lcdiff <- inner_join(landcover_urbandiff, cluster_metrics_df)
# check for trends...



plot(urban_diff~richness, data = cluster_metrics_urbandiff)
plot(urban_diff~richness, data = cluster_metrics_lcdiff)
plot(urban_diff~diversity, data = cluster_metrics_urbandiff)
plot(urban_diff~diversity, data = cluster_metrics_lcdiff)
















