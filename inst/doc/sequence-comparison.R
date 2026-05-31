## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 12,
  fig.height = 5,
  fig.align = "center",
  out.width = "100%",
  dpi = 300,
  message = FALSE,
  warning = FALSE
)
library(Nestimate)
set.seed(20260413)

## ----load---------------------------------------------------------------------
data(human_long, package = "Nestimate")
head(human_long)

## ----split--------------------------------------------------------------------
dat <- human_long[order(human_long$session_id, human_long$order_in_session), ]
n_per <- ave(dat$order_in_session, dat$session_id, FUN = length)
pos   <- ave(dat$order_in_session, dat$session_id, FUN = seq_along)
dat$half <- ifelse(pos <= n_per %/% 2, "early", "late")
table(dat$half)

## ----build--------------------------------------------------------------------
net <- build_network(
  data   = dat,
  actor  = "session_id",
  action = "cluster",
  group  = "half",
  method = "relative"
)
net

## ----compare------------------------------------------------------------------
res <- sequence_compare(net, min_freq = 25L, test = "chisq")
res

## ----top-table----------------------------------------------------------------
head(res$patterns, 10)

## ----pyramid------------------------------------------------------------------
plot(res, style = "pyramid", show_residuals = TRUE)

## ----heatmap------------------------------------------------------------------
plot(res, style = "heatmap")

## ----sort-freq----------------------------------------------------------------
plot(res, style = "pyramid", sort = "frequency", show_residuals = TRUE)

