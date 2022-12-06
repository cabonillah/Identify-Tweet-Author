library("yardstick")
library("tune")
library("stacks")
library("finetune")

ctrl_grid <- stacks::control_stack_grid()
ctrl_grid_race <- finetune::control_race()

size <- 200

tuning <- function(object,
                   resamples,
                   model,
                   ...) {
    if (model == "lasso") {
        tune <- finetune::tune_race_anova(
            # tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(),
                size = size
            ),
            metrics = yardstick::metric_set(accuracy, roc_auc),
            resamples = resamples,
            control = ctrl_grid_race
        )
    }

    if (model == "ridge") {
        tune <- finetune::tune_race_anova(
            # tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(),
                size = size
            ),
            metrics = yardstick::metric_set(accuracy, roc_auc),
            resamples = resamples,
            control = ctrl_grid_race
        )
    }

    if (model == "elastic") {
        tune <- finetune::tune_race_anova(
            # tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(),
                mixture(),
                size = size
            ),
            metrics = yardstick::metric_set(accuracy, roc_auc),
            resamples = resamples,
            control = ctrl_grid_race
        )
    }
    if (model == "rf") {
        tune <- finetune::tune_race_anova(
            # tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                mtry(c(1, 95)),
                min_n(),
                trees(),
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
                min_n(),
                sample_prop(),
                tree_depth(),
                learn_rate(),
                loss_reduction(),
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
                learn_rate(),
                hidden_units(),
                size = 100
            ),
            metrics = yardstick::metric_set(accuracy, roc_auc),
            resamples = resamples,
            control = ctrl_grid
        )
    }

    tune
}
