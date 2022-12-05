library("yardstick")
library("tune")
library("stacks")

ctrl_grid <- stacks::control_stack_grid()
size <- 50

tuning <- function(object,
                   resamples,
                   model,
                   ...) {
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
                mtry(c(1, 40)),
                min_n(c(2, 20)),
                trees(c(200, 2000)),
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
                min_n(c(2, 30)),
                sample_prop(),
                tree_depth(c(3, 15)),
                learn_rate(c(-4, -1)),
                loss_reduction(c(-10, 0)),
                trees(c(250, 2000)),
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
                epochs(c(300, 1000)),
                penalty(c(-10, -2)),
                learn_rate(c(-2, -1)),
                hidden_units(c(3, 10)),
                size = size
            ),
            metrics = yardstick::metric_set(accuracy, roc_auc),
            resamples = resamples,
            control = ctrl_grid
        )
    }

    tune
}
