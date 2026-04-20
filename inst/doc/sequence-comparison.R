## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 12,
  fig.height = 5,
  fig.align = "center",
  out.width = "100%",
  dpi = 96,
  message = FALSE,
  warning = FALSE
)
library(Nestimate)
set.seed(20260413)

## ----load---------------------------------------------------------------------
data(human_long, package = "Nestimate")
dat <- as.data.frame(human_long)
cat("rows:", nrow(dat),
    "| sessions:", length(unique(dat$session_id)),
    "| projects:", length(unique(dat$project)), "\n\n")
print(table(dat$cluster))

## ----split--------------------------------------------------------------------
dat <- dat[order(dat$session_id, dat$order_in_session), ]
n_per <- ave(dat$order_in_session, dat$session_id, FUN = length)
pos   <- ave(dat$order_in_session, dat$session_id, FUN = seq_along)
dat$half <- ifelse(pos <= n_per %/% 2, "early", "late")
print(table(dat$half))

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
res <- sequence_compare(
  net,
  sub      = 3:5,
  min_freq = 25L,
  test     = "chisq",
  adjust   = "fdr"
)
res

## ----top-table----------------------------------------------------------------
head(res$patterns, 10)

## ----pyramid------------------------------------------------------------------
plot(res, style = "pyramid", show_residuals = TRUE)

## ----heatmap------------------------------------------------------------------
plot(res, style = "heatmap")

## ----sort-freq----------------------------------------------------------------
plot(res, style = "pyramid", sort = "frequency", show_residuals = TRUE)

