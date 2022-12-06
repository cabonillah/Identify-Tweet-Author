library("yardstick")
library("tune")
library("stacks")
library("finetune")

ctrl_grid <- stacks::control_stack_grid()
ctrl_grid_race <- finetune::control_race()

size <- 25
tuning <- function(object,
                   resamples,
                   model,
                   ...) {
    if (model == "lasso") {
        # tune <- finetune::tune_race_anova(
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(c(0.00826, 0.00881), trans = NULL),
                size = size
            ),
            metrics = yardstick::metric_set(accuracy, roc_auc),
            resamples = resamples,
            control = ctrl_grid_race
        )
    }

    if (model == "ridge") {
        # tune <- finetune::tune_race_anova(
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(c(0.01, 0.026), trans = NULL),
                size = size
            ),
            metrics = yardstick::metric_set(accuracy, roc_auc),
            resamples = resamples,
            control = ctrl_grid_race
        )
    }

    if (model == "elastic") {
        # tune <- finetune::tune_race_anova(
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(c(-2, -1.75)),
                mixture(c(0.5, 0.9)),
                size = size
            ),
            metrics = yardstick::metric_set(accuracy, roc_auc),
            resamples = resamples,
            control = ctrl_grid_race
        )
    }
    if (model == "rf") {
        # tune <- finetune::tune_race_anova(
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                mtry(c(3, 6)),
                min_n(c(11, 15)),
                trees(c(750, 1000)),
                size = size
            ),
            metrics = yardstick::metric_set(accuracy, roc_auc),
            resamples = resamples,
            control = ctrl_grid_race
        )
    }
    if (model == "xgb") {
        tune <- finetune::tune_race_anova(
            # tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                mtry(c(1, 95)),
                min_n(c(2, 20)),
                sample_prop(c(0.4, 0.9)),
                tree_depth(c(4, 10)),
                learn_rate(c(-2, -1)),
                loss_reduction(c(-10, -5)),
                trees(),
                stop_iter(),
                size = size
            ),
            metrics = yardstick::metric_set(accuracy, roc_auc),
            resamples = resamples,
            control = ctrl_grid_race
        )
    }

    if (model == "mlp") {
        # tune <- finetune::tune_race_anova(
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                epochs(),
                penalty(),
                learn_rate(c(-2, -1)),
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
