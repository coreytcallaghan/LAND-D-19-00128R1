library(dplyr)
library(tidyr)
library(ggplot2)

# load allocations calculate in community_cluster_fit-k-means.R
allocations <- readRDS("Outputs/cluster_allocations/bcr_kmeans_allocations.rds")



# inspect the sum of squares ----------------------------------------------


# specific function to flatten the output list
## this is a bit tricky
## basically it unlists a couple times, adding in appropriate column names as needed
flatten_list <- function(x) {
  diagnostic_dfs <- lapply(x, `[[`, 2)
  #diagnostic_dfs <- lapply(diagnostic_dfs, function(x){x$k = nrow(x); x})
  bind_rows(diagnostic_dfs)
}
allocations_diagnostics <- bind_rows(lapply(allocations, flatten_list), .id = "BCR") # this is where the BCR unlisting is done

# make a plot friendly output
## now i wish i didn't make those data.frames - this is just to get the values that don't change for each cluster...
allocations_diagnostics_summary <- allocations_diagnostics %>%
  group_by(BCR, k) %>%
  summarise(total_ss = mean(total_ss),
            total_within_ss = mean(total_within_ss),
            between_ss = mean(between_ss),
            n_obs = mean(n_obs), # we need number of obs and species for AIC/BIC calculation
            n_vars = mean(n_vars))

# add information criterion
allocations_diagnostics_summary <- allocations_diagnostics_summary %>%
  mutate(AIC = total_within_ss + (2 * n_vars * k),
         BIC = total_within_ss + (log(n_obs) * n_vars * k))

# go long for plotting
allocations_diagnostics_plot <- allocations_diagnostics_summary %>%
  select(BCR, k, AIC, BIC) %>%
  gather(metric, value, AIC:BIC)

ggplot(allocations_diagnostics_plot, aes(x = k, group = metric)) +
  geom_line(aes(y = value, colour = metric)) +
  facet_wrap(~BCR, scales = "free")

ggplot(filter(allocations_diagnostics_plot, metric == "AIC"), aes(x = k, group = metric)) +
  geom_line(aes(y = value, colour = metric)) +
  facet_wrap(~BCR, scales = "free")

### looks like AIC is more informative at this point
### will try re-run with a distance metric on the data



# choose best clusters ----------------------------------------------------

min_AIC_data <- allocations_diagnostics_summary %>%
  group_by(BCR) %>%
  filter(AIC == min(AIC))


# #### in next version, i've named the lists by k, so they can be directly indexed, i.e.
# min_AIC_k <- min_AIC_data$k
# retreive_k_clusters <- function(bcrs, ks) {
#   bcrs[[as.character(ks)]][["allocations"]]
# }
# min_AIC_clusters <- purrr::map2(allocations, min_AIC_k, retreive_k_clusters)

# #### for the mean time:
min_AIC_k <- min_AIC_data$k - 1 # -1 because clustering was done k = 2:100
retreive_k_clusters <- function(bcrs, ks) {
  bcrs[[ks]][["allocations"]]
}
min_AIC_clusters <- purrr::map2(allocations, min_AIC_k, retreive_k_clusters)




# compare communities to land cover classification ------------------------

# join the data together
join_lc_data <- function(x, clustering) {
  matrix <- get(load(paste0("Data/Matrix for each BCR/", x, "_matrix.RData")))
  #gsub("2", "URBAN", matrix$URBAN_NONURBAN) # check if correct
  out <- data.frame(clusters = clustering[[x]],
                    urban = matrix$URBAN_NONURBAN,
                    landcover = matrix$AGGREGATED_LANDCOVER,
                    stringsAsFactors = F)
  filter(out, urban != "2") # for the meantime filter them out
}

clusters_landcover <- lapply(names(min_AIC_clusters), join_lc_data, min_AIC_clusters)
names(clusters_landcover) <- names(min_AIC_clusters)


## calculate entropy
# function to skip zeros
log0 <- function(x) {
  ifelse(x<=0, 0, log2(x))
}
# shannon entropy
shannon_entropy <- function(x) {
  -sum(x * log0(x))
}
# calculate entropy on xtab freqs
calculate_entropy <- function(x, type = "urban") {
  if (type == "urban") {
    xtab <- prop.table(table(x$urban, x$clusters), margin = 1) # frequencies
    as.data.frame(as.list(apply(xtab, 1, shannon_entropy)))
  } else if (type == "landcover") {
    xtab <- prop.table(table(x$landcover, x$clusters), margin = 1) # frequencies
    as.data.frame(as.list(apply(xtab, 1, shannon_entropy)))
  }
}


urban_entropy <- gather(bind_rows(lapply(clusters_landcover, calculate_entropy, type = "urban"), .id = "BCR"),
                        "zone", "value", -BCR)
landcover_entropy <- gather(bind_rows(lapply(clusters_landcover, calculate_entropy, type = "landcover"), .id = "BCR"),
                            "zone", "value", -BCR)


ggplot(urban_entropy, aes(y = value, x = zone)) +
  geom_boxplot() +
  geom_point(aes(colour = BCR)) +
  geom_line(aes(group = BCR))

ggplot(landcover_entropy, aes(y = value)) +
  geom_boxplot(aes(x = zone))

















