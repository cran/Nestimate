## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  fig.alt = "Visualization",
  warning = FALSE,
  message = FALSE
)

## ----data---------------------------------------------------------------------
library(Nestimate)
data(group_regulation_long)
head(group_regulation_long)

## ----network------------------------------------------------------------------
net <- build_network(
  group_regulation_long, method = "relative",
  actor = "Actor", action = "Action", time = "Time"
)
net

## ----predict------------------------------------------------------------------
pred <- predict_links(net, exclude_existing = FALSE)
pred

## ----consensus----------------------------------------------------------------
head(pred$consensus, 10)

## ----single-method------------------------------------------------------------
pred_katz <- predict_links(net, methods = "katz", exclude_existing = FALSE)
pred_katz

## ----sparse-------------------------------------------------------------------
net_sparse <- build_network(
  group_regulation_long, method = "relative",
  actor = "Actor", action = "Action", time = "Time",
  threshold = 0.05
)
pred_sparse <- predict_links(net_sparse, methods = "resource_allocation")
pred_sparse

## ----evaluation---------------------------------------------------------------
true_edges <- extract_edges(net)
pred_eval <- predict_links(net_sparse, exclude_existing = FALSE)
evaluate_links(pred_eval, true_edges[, c("from", "to")], k = c(3, 5, 10))

## ----per-group-link-----------------------------------------------------------
nets <- build_network(
  group_regulation_long, method = "relative",
  actor = "Actor", action = "Action", time = "Time",
  group = "Achiever", threshold = 0.05
)
lapply(nets, function(g) predict_links(g, methods = "katz", top_n = 3))

## ----pathways-link------------------------------------------------------------
pathways(pred_sparse, top = 5, evidence = TRUE)

## ----pathways-link-simple-----------------------------------------------------
pathways(pred_sparse, top = 5, evidence = FALSE)

## ----rules-basic--------------------------------------------------------------
rules <- association_rules(net, min_support = 0.3,
                           min_confidence = 0.5, min_lift = 1.0)
rules

## ----rules-summary------------------------------------------------------------
summary(rules)

## ----threshold-comparison-----------------------------------------------------
rules_broad  <- association_rules(net, min_support = 0.1,
                                  min_confidence = 0.3, min_lift = 0)
rules_strict <- association_rules(net, min_support = 0.5,
                                  min_confidence = 0.8, min_lift = 1.0)
data.frame(
  Setting = c("Broad (sup>=0.1, conf>=0.3)", "Strict (sup>=0.5, conf>=0.8, lift>=1)"),
  Rules   = c(rules_broad$n_rules, rules_strict$n_rules)
)

## ----per-group-rules----------------------------------------------------------
nets <- build_network(
  group_regulation_long, method = "relative",
  actor = "Actor", action = "Action", time = "Time",
  group = "Achiever"
)
lapply(nets, function(g) {
  association_rules(g, min_support = 0.3, min_confidence = 0.5, min_lift = 1.0)
})

## ----rules-plot, fig.alt = "Association rules scatter plot"-------------------
rules <- association_rules(net, min_support = 0.2,
                           min_confidence = 0.4, min_lift = 1.0)
if (rules$n_rules > 0) plot(rules)

## ----pathways-rules-----------------------------------------------------------
rules <- association_rules(net, min_support = 0.3,
                           min_confidence = 0.5, min_lift = 1.0)
pathways(rules)

## ----pathways-rules-filtered--------------------------------------------------
pathways(rules, top = 5, min_lift = 1.2)

## ----rules-list---------------------------------------------------------------
transactions <- list(
  c("plan", "discuss", "execute"),
  c("plan", "research", "analyze"),
  c("discuss", "execute", "reflect"),
  c("plan", "discuss", "execute", "reflect"),
  c("research", "analyze", "reflect")
)
association_rules(transactions, min_support = 0.3,
                  min_confidence = 0.5, min_lift = 0)

## ----rules-matrix-------------------------------------------------------------
mat <- matrix(c(
  1, 1, 1, 0, 0,
  1, 0, 0, 1, 1,
  0, 1, 1, 0, 1,
  1, 1, 1, 1, 0,
  1, 0, 1, 0, 1
), nrow = 5, byrow = TRUE)
colnames(mat) <- c("plan", "discuss", "execute", "research", "reflect")
association_rules(mat, min_support = 0.3, min_confidence = 0.5, min_lift = 0)

## ----simplicial-rules, eval = requireNamespace("cograph", quietly = TRUE), fig.width = 8, fig.height = 6, fig.alt = "Simplicial visualization of association rules"----
library(cograph)
net_sparse <- build_network(
  group_regulation_long, method = "relative",
  actor = "Actor", action = "Action", time = "Time",
  threshold = 0.05
)

# Association rules as simplicial blobs
rules <- association_rules(net_sparse, min_support = 0.3,
                           min_confidence = 0.5, min_lift = 1.0)
plot_simplicial(net_sparse, pathways(rules, top = 5),
                title = "Top Association Rules")

## ----simplicial-pred, eval = requireNamespace("cograph", quietly = TRUE), fig.width = 8, fig.height = 6, fig.alt = "Simplicial visualization of predicted links"----
# Predicted links with structural evidence
pred <- predict_links(net_sparse, methods = "resource_allocation")
plot_simplicial(net_sparse, pathways(pred, top = 5, evidence = TRUE),
                title = "Predicted Links with Evidence",
                dismantled = TRUE)

