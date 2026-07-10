## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  fig.alt = "Visualization"
)

## ----data---------------------------------------------------------------------
library(cograph)
library(Nestimate)
data("human_long")
head(human_long)

## -----------------------------------------------------------------------------
net <- build_network(human_long,
                     method = "tna",
                     action = "cluster",
                     actor  = "session_id",
                     time   = "timestamp")

## ----cluster-basic------------------------------------------------------------
clust <- build_clusters(net, k = 3)

clust

## ----cluster-components-------------------------------------------------------
# Cluster assignments (first 20 sessions)
head(clust$assignments, 20)

# Cluster sizes
clust$sizes

# Silhouette score (clustering quality: higher is better)
clust$silhouette

## ----cluster-plot, fig.alt = "Silhouette plot showing cluster quality"--------
plot(clust, type = "silhouette")

## ----cluster-mds, fig.alt = "MDS plot showing cluster separation"-------------
plot(clust, type = "mds")

## ----cluster-metrics----------------------------------------------------------
# Levenshtein distance (allows insertions/deletions)
clust_lv <- build_clusters(net, k = 3, dissimilarity = "lv")
clust_lv$silhouette

# Longest common subsequence
clust_lcs <- build_clusters(net, k = 3, dissimilarity = "lcs")
clust_lcs$silhouette

## ----cluster-weighted---------------------------------------------------------
# Emphasize earlier positions (higher lambda = faster decay)
clust_weighted <- build_clusters(net, 
                               k = 3,
                               dissimilarity = "hamming",
                               weighted = TRUE,
                               lambda = 0.5)
clust_weighted$silhouette

## ----cluster-methods----------------------------------------------------------
# Ward's method (minimizes within-cluster variance)
clust_ward <- build_clusters(net, k = 3, method = "ward.D2")
clust_ward$silhouette

# Complete linkage
clust_complete <- build_clusters(net, k = 3, method = "complete")
clust_complete$silhouette

## ----choose-k-----------------------------------------------------------------
ch <- cluster_choice(net, k = 2:4,
                      method = c("pam", "ward.D2", "complete", "average"))
ch

## ----choose-k-plot, fig.alt = "Silhouette across k for each clustering method"----
plot(ch, type = "lines")

## ----choose-d-----------------------------------------------------------------
ch_d <- cluster_choice(net, k = 2,
                        dissimilarity = c("hamming", "lv", "lcs"),
                        method = "ward.D2")
ch_d

## ----choose-d-plot, fig.alt = "Silhouette per dissimilarity at k = 2"---------
plot(ch_d, type = "bars", abbrev = TRUE)

## ----choose-tradeoff, fig.alt = "Quality vs cluster-size balance"-------------
plot(ch_d, type = "tradeoff", abbrev = TRUE)

## -----------------------------------------------------------------------------
clust <- build_clusters(net, k = 2, method = "ward.D2", seed = 42)
summary(clust)

## ----cluster-diagnostics------------------------------------------------------
diag <- cluster_diagnostics(clust)
diag

## ----cluster-diagnostics-plot, fig.alt = "Per-observation silhouette by cluster"----
plot(diag, type = "silhouette")

## -----------------------------------------------------------------------------
mmm_fit <- build_mmm(net, k = 2)
summary(mmm_fit)

## ----mmm-diagnostics----------------------------------------------------------
diag_mmm <- cluster_diagnostics(mmm_fit)
diag_mmm

## ----mmm-diagnostics-plot, fig.alt = "Posterior certainty per MMM cluster"----
plot(diag_mmm, type = "posterior")

## ----cluster-networks---------------------------------------------------------
clust <- build_clusters(net, k = 2, method = "ward.D2")
cluster_net <- build_network(clust)
cluster_net

## ----cluster-networks-plot, fig.alt = "Per-cluster transition networks"-------
plot(cluster_net)

## ----cluster-network-shortcut-------------------------------------------------
## `cograph::cluster_network()` also exists with a different signature
## (matrix aggregation); qualify with `Nestimate::` to avoid masking.
grp_dist <- Nestimate::cluster_network(net, k = 2, cluster_by = "ward.D2")
grp_dist

## ----cluster-mmm--------------------------------------------------------------
grp_mmm <- cluster_mmm(net, k = 2)
grp_mmm

## -----------------------------------------------------------------------------
cluster_diagnostics(grp_dist)
cluster_diagnostics(grp_mmm)

## -----------------------------------------------------------------------------
grp_dist[["Cluster 1"]]

## -----------------------------------------------------------------------------
comparison <- permutation(grp_dist, iter = 100)

## ----workflow, eval = FALSE---------------------------------------------------
# # 1. Build the network from long-format data
# net <- build_network(human_long, method = "tna",
#                      actor = "session_id",
#                      action = "cluster",
#                      time   = "timestamp")
# 
# # 2. Sweep the parameter space
# ch <- cluster_choice(net, k = 2:5,
#                      dissimilarity = c("hamming", "lcs", "cosine"),
#                      method = c("pam", "ward.D2"))
# plot(ch, type = "facet", abbrev = TRUE)
# 
# # 3. Pick a configuration and fit
# clust <- build_clusters(net, k = 2,
#                          dissimilarity = "hamming",
#                          method = "ward.D2")
# 
# # 4. Validate
# diag <- cluster_diagnostics(clust)
# diag
# plot(diag, type = "silhouette")
# 
# # 5. Build per-cluster networks
# grp <- build_network(clust)
# 
# # 6. Optional: model-based second opinion
# mmm <- build_mmm(net, k = 2)
# plot(cluster_diagnostics(mmm), type = "posterior")

