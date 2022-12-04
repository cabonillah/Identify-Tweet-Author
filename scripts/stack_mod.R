source("../scripts/recipes.R")
library("stacks")


ridge <- readRDS("../stores/result_ridge.Rds")
lasso <- readRDS("../stores/result_lasso.Rds")
elastic <- readRDS("../stores/result_elastic.Rds")
rf <- readRDS("../stores/result_rf.Rds")
xgb <- readRDS("../stores/result_xgb.Rds")
mlp <- readRDS("../stores/result_mlp.Rds")

cl <- parallel::makeCluster(3)
final_st <-
  # initialize the stack
  stacks() %>%
  # add each of the models
  add_candidates(ridge) %>%
  add_candidates(lasso) %>%
  add_candidates(elastic) %>%
  add_candidates(rf) %>%
  add_candidates(xgb) %>%
  add_candidates(mlp) %>%
  blend_predictions(
    control = tune::control_grid(allow_par = TRUE)
  ) %>% # evaluate candidate models
  fit_members()
parallel::stopCluster(cl)
