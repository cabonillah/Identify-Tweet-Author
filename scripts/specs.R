library("tidymodels")
library("ranger")
library("xgboost")
library("brulee")

specs <- function(model) {

    # Elastic - Classification
    if (model == "elastic") {
        spec <- multinom_reg(
            penalty = tune(),
            mixture = tune()
        ) %>%
            set_engine("glmnet")
    }

    # Ridge - Classification
    if (model == "ridge") {
        spec <- multinom_reg(
            penalty = tune(),
            mixture = 0
        ) %>%
            set_engine("glmnet")
    }

    # Lasso - Classification
    if (model == "lasso") {
        spec <- multinom_reg(
            penalty = tune(),
            mixture = 1
        ) %>%
            set_engine("glmnet")
    }


    # RF - Classification
    if (model == "rf") {
        spec <- rand_forest(
            trees = tune(),
            mtry = tune(),
            min_n = tune(),
        ) %>%
            set_engine(
                "ranger",
                importance = "impurity",
                verbose = TRUE,
                num.threads = 3
            ) %>%
            set_mode("classification")
    }

    # XGB - Classification
    if (model == "xgb") {
        spec <- boost_tree(
            trees = tune(),
            mtry = tune(),
            min_n = tune(),
            sample_size = tune(),
            stop_iter = tune(),
            tree_depth = tune(),
            learn_rate = tune(),
            loss_reduction = tune()
        ) %>%
            set_engine("xgboost", nthread = 3) %>%
            set_mode("classification")
    }

    # MLP - Classification
    if (model == "mlp") {
        spec <- mlp(
            epochs = tune(),
            hidden_units = tune(),
            penalty = tune(),
            learn_rate = tune()
        ) %>%
            set_engine("brulee") %>%
            set_mode("classification") %>%
            translate()
    }


    spec
}
