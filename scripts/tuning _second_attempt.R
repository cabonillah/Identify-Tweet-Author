library("yardstick")
library("tune")
library("stacks")

ctrl_grid <- stacks::control_stack_grid()
size <- 5

tuning <- function(object,
                   resamples,
                   model,
                   ...) {
    if (model == "lasso") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(c(0.00155, 0.0017), trans = NULL),
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
                penalty(c(-10, -1.8)),
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
                penalty(c(-2.7, -2.2)),
                mixture(c(0.4, 0.7)),
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
                mtry(c(5, 8)),
                min_n(c(2, 9)),
                trees(c(1250, 1700)),
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
                mtry(c(30, 74)),
                min_n(c(2, 8)),
                sample_prop(c(0.4, 0.9)),
                tree_depth(c(5, 15)),
                learn_rate(c(-2, -1)),
                loss_reduction(c(-10, 0)),
                trees(c(600, 1250)),
                stop_iter(c(3, 15)),
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
                epochs(c(900, 1000)),
                penalty(c(-4.5, -3.6)),
                learn_rate(c(-1.025, -1)),
                hidden_units(c(5, 10)),
                size = size
            ),
            metrics = yardstick::metric_set(accuracy, roc_auc),
            resamples = resamples,
            control = ctrl_grid
        )
    }

    tune
}
