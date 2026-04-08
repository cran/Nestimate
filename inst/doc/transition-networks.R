## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  fig.width = 7,
  fig.height = 5,
  fig.dpi = 90,
  dpi = 300,
  warning = FALSE,
  message = FALSE
)

## ----data---------------------------------------------------------------------
library(Nestimate)

# Subsample for vignette speed (CRAN build-time limit)
set.seed(1)
keep <- sample(unique(human_cat$session_id), 100)
human_sub <- human_cat[human_cat$session_id %in% keep, ]

head(human_sub)

## ----tna----------------------------------------------------------------------
net_tna <- build_network(human_sub, method = "tna",
                         action = "category", actor = "session_id",
                         time = "timestamp")
print(net_tna)

## ----ftna---------------------------------------------------------------------
net_ftna <- build_network(human_sub, method = "ftna",
                          action = "category", actor = "session_id",
                          time = "timestamp")
print(net_ftna)

## ----atna---------------------------------------------------------------------
net_atna <- build_network(human_sub, method = "atna",
                          action = "category", actor = "session_id",
                          time = "timestamp")
print(net_atna)

## ----onehot-------------------------------------------------------------------
data(learning_activities)
net <- build_network(learning_activities, method = "cna", actor = "student")
print(net)

## ----wtna-freq----------------------------------------------------------------
net_wtna <- wtna(learning_activities, actor = "student",
                 method = "transition", type = "frequency")
print(net_wtna)

## ----wtna-relative------------------------------------------------------------
net_wtna_rel <- wtna(learning_activities, method = "transition", type = "relative")
print(net_wtna_rel)

## ----wtna-mixed---------------------------------------------------------------
net_wtna_mixed <- wtna(learning_activities, method = "both", type = "relative")
print(net_wtna_mixed)

## ----reliability--------------------------------------------------------------
reliability(net_tna)

## ----bootstrap----------------------------------------------------------------
set.seed(42)
boot <- bootstrap_network(net_tna, iter = 100)
boot

## ----cs-----------------------------------------------------------------------
centrality_stability(net_tna, iter = 100)

## ----clustering---------------------------------------------------------------
Cls <- cluster_data(net_tna, k = 3)
Clusters <- build_network(Cls, method = "tna")
Clusters

## ----centrality---------------------------------------------------------------
Nestimate::centrality(Clusters)

## ----perm-clusters------------------------------------------------------------
perm <- permutation_test(Clusters$`Cluster 1`, Clusters$`Cluster 2`,
                         iter = 100)
perm

## ----mmm----------------------------------------------------------------------
data("group_regulation_long")

net_GR <- build_network(group_regulation_long, method = "tna",
                        action = "Action", actor = "Actor",
                        time = "Time")

mmmCls <- build_mmm(net_GR, k = 2, covariates = c("Group"))
summary(mmmCls)

## ----mmm-networks-------------------------------------------------------------
Mnets <- build_network(mmmCls)
Mnets

## ----posthoc------------------------------------------------------------------
Post <- cluster_data(net_GR, k = 2, covariates = c("Achiever"))
summary(Post)

## ----posthoc-networks---------------------------------------------------------
Postgr <- build_network(Post)
Postgr

## ----pna-data-----------------------------------------------------------------
data(srl_strategies)
head(srl_strategies)

## ----cor----------------------------------------------------------------------
net_cor <- build_network(srl_strategies, method = "cor")
net_cor

## ----pcor---------------------------------------------------------------------
net_pcor <- build_network(srl_strategies, method = "pcor")
net_pcor

## ----glasso-------------------------------------------------------------------
net_glasso <- build_network(srl_strategies, method = "glasso",
                            params = list(gamma = 0.5))
net_glasso

## ----predictability-----------------------------------------------------------
pred <- predictability(net_glasso)
round(pred, 3)

## ----boot-glasso--------------------------------------------------------------
boot_gl <- boot_glasso(net_glasso, iter = 100,
                       centrality = c("strength", "expected_influence"),
                       seed = 42)

## ----boot-edges---------------------------------------------------------------
summary(boot_gl, type = "edges")

## ----boot-stability-----------------------------------------------------------
summary(boot_gl, type = "centrality")

