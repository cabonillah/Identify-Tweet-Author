source("../scripts/recipes.R")
source("../scripts/specs.R")

workflows <- function(model) {
    spec <- specs(model)

    workflow <- workflow() %>%
        add_recipe(rec) %>%
        add_model(spec)

    workflow
}
