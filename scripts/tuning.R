library("yardstick")
library("tune")
library("stacks")

ctrl_grid <- stacks::control_stack_grid()
size <- 5

tuning <- function(object,
                   resamples,
                   model,
                   ...) {
    if (model == "ridge") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(c(0.0175, 0.0200), trans = NULL),
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
                penalty(c(0.000643, 0.00114), trans = NULL),
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
                penalty(c(-2.93, -2.77)),
                mixture(c(0.5, 0.9)),
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
                mtry(c(6,12)),
                min_n(c(1,4)),
                trees(c(800,1100)),
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
                min_n(c(1, 8)),
                sample_prop(c(0.55, 0.8)),
                tree_depth(c(6, 9)),
                learn_rate(c(-1.8, -1.25)),
                loss_reduction(c(-10, -6)),
                trees(c(1500, 2000)),
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
                epochs(c(900, 1000)),
                penalty(c(-8, -4)),
                learn_rate(c(-1.075, -1)),
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
