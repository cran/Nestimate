## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  fig.width = 7,
  fig.height = 5,
  dpi = 96,
  warning = FALSE,
  message = FALSE,
  output.lines = 40
)
local({
  hook_output <- knitr::knit_hooks$get("output")
  knitr::knit_hooks$set(output = function(x, options) {
    n <- options$output.lines
    if (!is.null(n)) {
      lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
      if (length(lines) > n) {
        x <- paste(c(utils::head(lines, n),
                     sprintf("#> [... %d more lines ...]", length(lines) - n)),
                   collapse = "\n")
      }
    }
    hook_output(x, options)
  })
})
options(max.print = 100)

## ----data---------------------------------------------------------------------
library(Nestimate)

# Subsample for vignette speed (CRAN build-time limit)
set.seed(1)
keep <- sample(unique(human_long$session_id), 100)
human_sub <- human_long[human_long$session_id %in% keep, ]

head(human_sub)

## ----tna----------------------------------------------------------------------
net_tna <- build_network(human_sub, method = "tna",
                         action = "code", actor = "session_id",
                         time = "timestamp")
print(net_tna)

## ----ftna---------------------------------------------------------------------
net_ftna <- build_network(human_sub, method = "ftna",
                          action = "code", actor = "session_id",
                          time = "timestamp")
print(net_ftna)

## ----atna---------------------------------------------------------------------
net_atna <- build_network(human_sub, method = "atna",
                          action = "code", actor = "session_id",
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

## ----network-reliability------------------------------------------------------
network_reliability(net_tna)

## ----bootstrap----------------------------------------------------------------
set.seed(42)
boot <- bootstrap_network(net_tna, iter = 100)
boot

## ----cs-----------------------------------------------------------------------
centrality_stability(net_tna, iter = 100)

## ----clustering---------------------------------------------------------------
Cls <- build_clusters(net_tna, k = 3)
Clusters <- build_network(Cls, method = "tna")
Clusters

## ----perm-clusters------------------------------------------------------------
perm <- permutation(Clusters$`Cluster 1`, Clusters$`Cluster 2`,
                         iter = 100)
perm

## ----posthoc-data-------------------------------------------------------------
data("group_regulation_long")
net_GR <- build_network(group_regulation_long, method = "tna",
                        action = "Action", actor = "Actor",
                        time = "Time")

## ----posthoc------------------------------------------------------------------
Post <- build_clusters(net_GR, k = 2, covariates = c("Achiever"))
summary(Post)

## ----posthoc-networks---------------------------------------------------------
Postgr <- build_network(Post)
Postgr

## ----pna-data-----------------------------------------------------------------
data(chatgpt_srl)
head(chatgpt_srl)

## ----glasso-------------------------------------------------------------------
net_glasso <- build_network(chatgpt_srl, method = "glasso",
                            params = list(gamma = 0.5))
net_glasso

