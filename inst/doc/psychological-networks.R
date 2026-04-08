## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----data---------------------------------------------------------------------
library(Nestimate)
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

## ----bootstrap----------------------------------------------------------------
boot <- boot_glasso(net_glasso, iter = 100,
                    centrality = c("strength", "expected_influence"),
                    seed = 42)

## ----boot-edges---------------------------------------------------------------
summary(boot, type = "edges")

## ----boot-stability-----------------------------------------------------------
summary(boot, type = "centrality")

