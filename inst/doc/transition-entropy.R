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
has_cograph <- requireNamespace("cograph", quietly = TRUE)

## -----------------------------------------------------------------------------
library(Nestimate)

## ----fit----------------------------------------------------------------------
net <- build_network(group_regulation_long, method = "relative",
                     actor = "Actor", action = "Action", time = "Time")
te <- transition_entropy(net)
te

## ----states-------------------------------------------------------------------
summary(te)
plot(te)

## ----entropy-network----------------------------------------------------------
ent <- entropy_network(net, scaling = "share")
ent

## ----entropy-network-plot, eval = has_cograph, fig.width = 10, fig.height = 6----
op <- par(mfrow = c(1, 2))
cograph::splot(net, minimum = 0, title = "Transition probabilities",
               edge_label_digits = 2)
cograph::splot(ent, title = "Share of process entropy (%)")
par(op)

## ----trajectory, fig.width = 8, fig.height = 4.5------------------------------
tr <- entropy_trajectory(group_regulation_long,
                         action = "Action", actor = "Actor", time = "Time",
                         group = "Achiever", window = 500, step = 100)
summary(tr)
plot(tr)

## ----bayes--------------------------------------------------------------------
eb <- entropy_bayes(net, draws = 1000, seed = 1)
eb

## ----bayes-plot, fig.width = 7.5, fig.height = 6------------------------------
plot(eb)

