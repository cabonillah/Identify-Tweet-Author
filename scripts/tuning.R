library("yardstick")
library("tune")
library("stacks")

ctrl_grid <- stacks::control_stack_grid()
size <- 500

tuning <- function(object,
                   resamples,
                   model,
                   ...) {
    if (model == "ridge") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(),
                size = size
            ),
            metrics = yardstick::metric_set(accuracy, roc_auc),
            resamples = resamples,
            control = ctrl_grid
        )
    }
    if (model == "lasso") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(),
                size = size
            ),
            metrics = yardstick::metric_set(accuracy, roc_auc),
            resamples = resamples,
            control = ctrl_grid
        )
    }
    if (model == "elastic") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(),
                mixture(),
                size = size
            ),
            metrics = yardstick::metric_set(accuracy, roc_auc),
            resamples = resamples,
            control = ctrl_grid
        )
    }
    if (model == "rf") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                mtry(c(6, 12)),
                min_n(c(1, 7)),
                trees(c(550, 1200)),
                size = size
            ),
            metrics = yardstick::metric_set(accuracy, roc_auc),
            resamples = resamples,
            control = ctrl_grid
        )
    }
    if (model == "xgb") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                mtry(c(1, 74)),
                min_n(),
                sample_prop(),
                tree_depth(c(4, 12)),
                learn_rate(c(-2, -1)),
                loss_reduction(),
                trees(),
                stop_iter(),
                size = size
            ),
            metrics = yardstick::metric_set(accuracy, roc_auc),
            resamples = resamples,
            control = ctrl_grid
        )
    }

    if (model == "mlp") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                epochs(),
                penalty(),
                learn_rate(),
                hidden_units(),
                size = size
            ),
            metrics = yardstick::metric_set(accuracy, roc_auc),
            resamples = resamples,
            control = ctrl_grid
        )
    }

    tune
}
