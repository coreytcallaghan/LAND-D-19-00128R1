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
            n_vars = mean(n_vars),
            k = mean(k))

# add information criterion
allocations_diagnostics_summary <- allocations_diagnostics_summary %>%
  mutate(AIC = total_within_ss + (2 * n_vars * k),
         BIC = total_within_ss + (log(n_obs) * n_vars * k))

# save off for plotting later
saveRDS(allocations_diagnostics_summary, "Outputs/cluster_allocations/bcr_kmeans_diagnostics.rds")
# load
allocations_diagnostics_summary <- readRDS("Outputs/cluster_allocations/bcr_kmeans_diagnostics.rds")

allocations_diagnostics_plot <- allocations_diagnostics_summary %>%
  select(BCR, k, AIC, BIC) %>%
  gather(metric, value, AIC:BIC)

ggplot(allocations_diagnostics_plot, aes(x = k, group = metric)) +
  geom_line(aes(y = value, colour = metric)) +
  facet_wrap(~BCR, scales = "free")

### looks like AIC is more informative at this point
### re-run with more clusters will tell us more
