# specific function to flatten the output list
## this is a bit tricky
## basically it unlists a couple times, adding in appropriate column names as needed
flatten_list <- function(x) {
  diagnostic_dfs <- lapply(head(x, -1), `[[`, 2) # use head() to ignore the localities vector
  #diagnostic_dfs <- lapply(diagnostic_dfs, function(x){x$k = nrow(x); x})
  bind_rows(diagnostic_dfs)
}

#### i've named the lists by k, so they can be directly indexed, i.e.
retreive_k_clusters <- function(bcrs, ks) {
  bcrs[[as.character(ks)]][["allocations"]]
}

# function to join data on
join_lc_data <- function(x, clustering, localities) {
  locality_df <- data.frame(LOCALITY_ID = localities[[x]], stringsAsFactors = F)
  matrix <- get(load(paste0("Data/Matrix for each BCR/", x, "_matrix.RData")))
  matrix <- inner_join(matrix, locality_df, by = "LOCALITY_ID")
  #gsub("2", "URBAN", matrix$URBAN_NONURBAN) # check if correct
  out <- data.frame(clusters = clustering[[x]],
                    urban = matrix$URBAN_NONURBAN,
                    landcover = matrix$AGGREGATED_LANDCOVER,
                    stringsAsFactors = F)
  filter(out, urban != "2") # filter out 2's if they exist
}

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

## calculate membership disparity
## positive values will mean more proportional membership of sites to the non-urban component
calculate_urban_diff <- function(x, type = "urban") {
  if (type == "urban") {
    xtab <- prop.table(table(x$urban, x$clusters), margin = 1) # frequencies
    data.frame(urban_diff = xtab["NONURBAN",] - xtab["URBAN",],
    cluster = 1:ncol(xtab))
  } else if (type == "landcover") {
    xtab <- prop.table(table(x$landcover, x$clusters), margin = 1) # frequencies
    data.frame(urban_diff = xtab["Natural Green Area",] - xtab["Urban Green Area",],
               cluster = 1:ncol(xtab))
  }
}

# calculate metrics (richess, diversity etc.) given a clustering solution to index by
metrics_per_cluster <- function(x, cluster_allocation, data) {
  data <- data[which(cluster_allocation %in% x),] %>%
    select(which(colSums(.) > 0))
  data.frame(richness = ncol(data),
             diversity = shannon_entropy(apply(data, 2, function(x) sum(x)/length(x))))
}

# subset to appropriate data and get metrics per cluster per bcr
calculate_cluster_metrics <- function(x, clustering, localities) {
  locality_df <- data.frame(LOCALITY_ID = localities[[x]], stringsAsFactors = F)
  matrix <- get(load(paste0("Data/Matrix for each BCR/", x, "_matrix.RData")))
  matrix <- matrix %>%
    inner_join(locality_df, by = "LOCALITY_ID") %>%
    ungroup() %>%
    select(-(LOCALITY_ID:NAME10)) %>%
    select(which(colSums(.) > 0))
  bind_rows(lapply(unique(clustering[[x]]), metrics_per_cluster, clustering[[x]], matrix), .id = "cluster")
}