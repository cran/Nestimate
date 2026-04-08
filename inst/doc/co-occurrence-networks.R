## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  fig.alt = "Network visualization"
)

## ----data---------------------------------------------------------------------
library(Nestimate)
data(learning_activities)
head(learning_activities, 10)

## ----activities---------------------------------------------------------------
activities <- c("Reading", "Video", "Forum", "Quiz", "Coding", "Review")
colSums(learning_activities[, activities])

## ----concurrent---------------------------------------------------------------
# How many activities are active at each time point?
n_active <- rowSums(learning_activities[, activities])
table(n_active)

## ----cna----------------------------------------------------------------------
net_cna <- build_network(learning_activities,
                         method = "co_occurrence",
                         codes = activities,
                         actor = "student")
net_cna

## ----cna-weights--------------------------------------------------------------
# View the co-occurrence matrix
round(net_cna$weights, 0)

## ----cna-window---------------------------------------------------------------
net_cna_windowed <- build_network(learning_activities,
                                   method = "co_occurrence",
                                   codes = activities,
                                   actor = "student",
                                   window_size = 10)
net_cna_windowed

## ----ising-check, include = FALSE---------------------------------------------
has_glmnet <- requireNamespace("glmnet", quietly = TRUE)

## ----ising, eval = has_glmnet-------------------------------------------------
# Aggregate to student-level summaries for Ising (requires cross-sectional data)
student_summary <- aggregate(learning_activities[, activities],
                              by = list(student = learning_activities$student),
                              FUN = function(x) as.integer(mean(x) > 0.5))
student_summary <- student_summary[, -1]  # Remove student column

net_ising <- build_network(student_summary,
                           method = "ising",
                           params = list(gamma = 0.25))
net_ising

## ----ising-alt, eval = !has_glmnet, echo = FALSE------------------------------
# cat("Ising networks require the 'glmnet' package.\n")
# cat("Install with: install.packages('glmnet')\n")

## ----ising-sparse, eval = has_glmnet------------------------------------------
net_ising_sparse <- build_network(student_summary,
                                   method = "ising",
                                   params = list(gamma = 0.5))

# Compare edge counts
cat("Gamma = 0.25:", sum(net_ising$weights != 0), "edges\n")
cat("Gamma = 0.50:", sum(net_ising_sparse$weights != 0), "edges\n")

## ----ising-rules, eval = has_glmnet-------------------------------------------
net_and <- build_network(student_summary, method = "ising",
                         params = list(gamma = 0.25, rule = "AND"))
net_or <- build_network(student_summary, method = "ising",
                        params = list(gamma = 0.25, rule = "OR"))

cat("AND rule edges:", sum(net_and$weights != 0), "\n")
cat("OR rule edges:", sum(net_or$weights != 0), "\n")

