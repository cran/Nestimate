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
df <- as.data.frame(trajectories)
active_count  <- rowSums(df == "Active", na.rm = TRUE)
mostly_active <- active_count > ncol(df) / 2
cat(sum(mostly_active), "of", nrow(df), "learners qualify\n")
sub <- df[mostly_active, ]

## ----fig.height=7-------------------------------------------------------------
state_pal <- c(Active = "#1a7a1a", Average = "#E69F00", Disengaged = "#CC79A7")

sequence_plot(
  df,
  type         = "heatmap",
  sort         = "lcs",
  k            = 3,
  k_color      = "white",
  k_line_width = 2,
  state_colors = state_pal,
  na_color     = "grey88",
  main         = "Full sample - all 138 learners",
  time_label   = "Time-step",
  y_label      = "Learner",
  legend       = "bottom"
)

## ----fig.height=5-------------------------------------------------------------
sequence_plot(
  sub,
  type         = "heatmap",
  sort         = "lcs",
  k            = 2,
  k_color      = "white",
  k_line_width = 2,
  state_colors = state_pal,
  na_color     = "grey88",
  main         = "Mostly-active learners - Active > 7 of 15 steps (n = 42)",
  time_label   = "Time-step",
  y_label      = "Learner",
  legend       = "bottom"
)

## -----------------------------------------------------------------------------
net_all <- build_network(df,  method = "relative")
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

