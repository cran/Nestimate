## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      fig.width = 7, fig.height = 5.5,
                      out.width = "100%",
                      dev = "png", dpi = 72)

## ----setup, message = FALSE---------------------------------------------------
library(Nestimate)

## ----data---------------------------------------------------------------------
data(trajectories)
dim(trajectories)
head(trajectories[, 1:8])
sort(unique(as.vector(trajectories)), na.last = NA)

## ----h-default----------------------------------------------------------------
sequence_plot(trajectories)

## ----h-freq-------------------------------------------------------------------
sequence_plot(trajectories, sort = "frequency",
              main = "sort = 'frequency'")

## ----h-hamming----------------------------------------------------------------
sequence_plot(trajectories, sort = "hamming",
              main = "sort = 'hamming'")

## ----h-start------------------------------------------------------------------
sequence_plot(trajectories, sort = "start",
              main = "sort = 'start' (no dendrogram)")

## ----h-k3---------------------------------------------------------------------
sequence_plot(trajectories, k = 3,
              main = "k = 3 — white separators")

## ----h-k5-black---------------------------------------------------------------
sequence_plot(trajectories, k = 5,
              k_color = "black", k_line_width = 1.2,
              main = "k = 5 — thin black")

## ----h-legend-----------------------------------------------------------------
sequence_plot(trajectories,
              legend = "bottom",
              legend_title = "Engagement",
              state_colors = c("#2a9d8f", "#e9c46a", "#e76f51"),
              main = "Custom palette + bottom legend")

## ----h-borders----------------------------------------------------------------
sequence_plot(trajectories,
              cell_border = "grey60", tick = 3,
              main = "Cell grid + every-3rd tick")

## ----h-frame------------------------------------------------------------------
sequence_plot(trajectories, frame = TRUE,
              main = "frame = TRUE")

## ----i-default----------------------------------------------------------------
sequence_plot(trajectories, type = "index",
              main = "index — single panel")

## ----i-gap--------------------------------------------------------------------
sequence_plot(trajectories, type = "index", row_gap = 0.25,
              main = "index with row_gap = 0.25")

## ----i-cluster----------------------------------------------------------------
cl <- build_clusters(trajectories, k = 3L,
                   dissimilarity = "hamming", method = "ward.D2")
sequence_plot(cl, type = "index",
              main = "index faceted by build_clusters(k = 3)")

## ----i-row, fig.width=9, fig.height=4-----------------------------------------
sequence_plot(cl, type = "index", ncol = 3, nrow = 1,
              main = "index — ncol = 3, nrow = 1")

## ----d-area-------------------------------------------------------------------
sequence_plot(trajectories, type = "distribution",
              main = "distribution — stacked area")

## ----d-bar-count--------------------------------------------------------------
sequence_plot(trajectories, type = "distribution",
              geom = "bar", scale = "count",
              main = "distribution — bars, count scale")

## ----d-na-true----------------------------------------------------------------
sequence_plot(trajectories, type = "distribution", na = TRUE,
              main = "na = TRUE")

## ----d-na-false---------------------------------------------------------------
sequence_plot(trajectories, type = "distribution", na = FALSE,
              main = "na = FALSE")

## ----d-cluster----------------------------------------------------------------
sequence_plot(cl, type = "distribution",
              main = "distribution by cluster (k = 3)")

## ----cheatsheet---------------------------------------------------------------
# Always explore first with the default:
sequence_plot(trajectories)

# Zoom in on cluster structure:
sequence_plot(trajectories, k = 3)
sequence_plot(trajectories, sort = "hamming", k = 4)

# Compare cluster compositions:
cl <- build_clusters(trajectories, k = 3,
                   dissimilarity = "hamming", method = "ward.D2")
sequence_plot(cl, type = "index")
sequence_plot(cl, type = "distribution")

# Polish for a paper:
sequence_plot(trajectories, k = 3,
              state_colors = c("#2a9d8f", "#e9c46a", "#e76f51"),
              legend_title = "Engagement",
              legend = "bottom",
              cell_border = "grey70",
              main = "Student engagement trajectories")

