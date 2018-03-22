library(dplyr)
library(parallel)
library(stats) # somtimes not loaded via rscript


# functions to load data and fit k-means ---------------------------------------

# function to make kmeans work with lapply and tidy output
# You can pass other kmeans arguments to it as required
my_kmeans <- function(k, data, ...) {
  message(paste0(k, " clusters"))
  fm <- kmeans(data, k, ...)
  list(allocations = as.integer(fm$cluster),
       diagnostics = data.frame(total_ss = fm$totss,
                                #within_ss = fm$withinss, # don't need for the meantime
                                total_within_ss = fm$tot.withinss,
                                between_ss = fm$betweenss,
                                n_obs = length(fm$cluster),
                                n_vars = ncol(fm$centers),
                                k = nrow(fm$centers)))
}  # this is really wasteful if you want within_ss, as only within_ss changes each row, but meh, saves an unlist later

# function to fit the kmeans to different matrices
## k_range is the range of cluster numbers to fir and test
## do.clustering is whether to actually perform the clustering (TRUE) or just check all data can be loaded and is numeric (FALSE)
## pres.abs=TRUE will do the clustering on presence absense data instead of proportions
fit_kmeans <- function(file, k_range, min.checklists = 1, do.clustering = TRUE, pres.abs = FALSE, ...) {
  message(paste0("Clustering ", file))
  # load and check data
  matrix <- get(load(paste0("Data/Matrix for each BCR/", file)))
  matrix <- matrix %>%
    ungroup() %>%
    filter(total_checklists >= min.checklists)
  localities <- matrix$LOCALITY_ID
  matrix <- matrix %>%
    select(-LOCALITY_ID, -total_checklists, -URBAN_NONURBAN, 
           -LC, -AGGREGATED_LANDCOVER, -NAME10) %>%
    select(which(colSums(.) > 0))
  if (pres.abs) {matrix <- ifelse(matrix > 0, 1, 0)}
  if (any(colSums(matrix) == 0)) stop("There are columns with zero data")
  #names(matrix) <- 1:ncol(matrix)
  # do the k-means
  if (do.clustering) {
    out <- lapply(k_range, my_kmeans, matrix, ...)
    names(out) <- k_range
    out$localities <- localities
    out
  }
}



# fit the kmeans ----------------------------------------------------------

### Here we have implemented on a computing cluster, so use mclapply
### If you're on windows or using smaller data sets and want to do it locally you could use lapply:
# files <- list.files("Data/Matrix for each BCR/")
# allocations <- lapply(files, fit_kmeans, 5:30)

files <- list.files("Data/Matrix for each BCR/")
allocations <- mclapply(mc.cores = 16, # number of cores - can set to 1 if on windows or else use detectCores()
                        X = files, FUN = fit_kmeans, 2:30, pres.abs = FALSE, 
                        iter.max = 30, nstart = 10) # extra arugments to pass to kmeans
names(allocations) <- gsub("_matrix.RData", "", files)

saveRDS(allocations, file = "bcr_kmeans_allocations.rds")


