## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  out.width = "100%",
  fig.width = 7,
  fig.height = 5,
  dpi       = 96,
  warning   = FALSE,
  message   = FALSE
)

## -----------------------------------------------------------------------------
library(Nestimate)

## -----------------------------------------------------------------------------
dim(trajectories)
table(as.vector(trajectories), useNA = "always")

## -----------------------------------------------------------------------------
sub <- trajectories[rowSums(trajectories == "Active", na.rm = TRUE) > 7, ]
nrow(sub)

## ----fig.height=7-------------------------------------------------------------
sequence_plot(trajectories, type = "heatmap")

## ----fig.height=5-------------------------------------------------------------
sequence_plot(sub, type = "heatmap")

## -----------------------------------------------------------------------------
net_all <- build_network(trajectories, method = "relative")
net_sub <- build_network(sub, method = "relative")

round(net_all$weights, 3)
round(net_sub$weights, 3)

## -----------------------------------------------------------------------------
pt_all <- passage_time(net_all)
pt_sub <- passage_time(net_sub)

## -----------------------------------------------------------------------------
print(pt_all, digits = 2)

## -----------------------------------------------------------------------------
print(pt_sub, digits = 2)

## ----fig.height=4-------------------------------------------------------------
plot(pt_all, title = "Full sample (n = 138)")

## ----fig.height=4-------------------------------------------------------------
plot(pt_sub, title = "Mostly-active learners (n = 42)")

## ----echo=FALSE---------------------------------------------------------------
data.frame(
  State           = names(pt_all$stationary),
  `Full sample`   = paste0(round(pt_all$stationary * 100, 1), "%"),
  `Mostly active` = paste0(round(pt_sub$stationary * 100, 1), "%"),
  check.names     = FALSE
)

## -----------------------------------------------------------------------------
ms_all <- markov_stability(net_all)
ms_sub <- markov_stability(net_sub)

print(ms_all)
print(ms_sub)

## ----fig.height=6-------------------------------------------------------------
plot(ms_all, title = "Full sample")

## ----fig.height=6-------------------------------------------------------------
plot(ms_sub, title = "Mostly-active learners")

