## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  fig.alt = "Visualization"
)

## ----data---------------------------------------------------------------------
library(Nestimate)
data("human_long")

# Subsample for vignette speed (CRAN build-time limit)
set.seed(1)
keep <- sample(unique(human_long$session_id), 80)
human_sub <- human_long[human_long$session_id %in% keep, ]

head(human_sub)

## -----------------------------------------------------------------------------
net <- build_network(human_sub,
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
methods <- c("pam", "ward.D2", "complete", "average")

silhouettes <- lapply(methods, function(m) {
  sapply(2:4, function(k) {
    build_clusters(net, k = k, method = m, seed = 42)$silhouette
  })
})

names(silhouettes) <- methods

silhouettes

## ----choose-k-plot, fig.alt = "Silhouette scores across different k values"----
methods <- names(silhouettes)
colors <- rainbow(length(methods))

plot(2:4, silhouettes[[1]], type = "b", pch = 19, col = colors[1],
     xlab = "Number of clusters (k)",
     ylab = "Average silhouette width",
     ylim = c(0, 1),
     main = "Choosing k")

for (i in 2:length(methods)) {
  lines(2:4, silhouettes[[i]], type = "b", pch = 19, col = colors[i])
}

legend("topright", legend = methods, col = colors, lty = 1, pch = 19)

## -----------------------------------------------------------------------------
clust <- build_clusters(net, k = 2, method = "ward.D2", seed = 42)

summary(clust)

## -----------------------------------------------------------------------------
mmm_default <- build_mmm(net)

## -----------------------------------------------------------------------------
summary(mmm_default)
head(mmm_default$assignments,10)

## ----cluster-networks---------------------------------------------------------
cluster_net <- build_network(clust)

## -----------------------------------------------------------------------------
comparison <- permutation(cluster_net, iter = 100)

