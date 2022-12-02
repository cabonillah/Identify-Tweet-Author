system.time({
    source("../scripts/lasso_clas.R")
    autoplot(result, metric = "accuracy")
    ggsave("../stores/lasso_clas.png")

    source("../scripts/ridge_clas.R")
    autoplot(result, metric = "accuracy")
    ggsave("../stores/ridge_clas.png")

    source("../scripts/elastic_clas.R")
    autoplot(result, metric = "accuracy")
    ggsave("../stores/elastic_clas.png")

    source("../scripts/rf_clas.R")
    autoplot(result, metric = "accuracy")
    ggsave("../stores/rf_clas.png")

    source("../scripts/xgb_clas.R")
    autoplot(result, metric = "accuracy")
    ggsave("../stores/xgb_clas.png")

    source("../scripts/mlp_clas.R")
    autoplot(result, metric = "accuracy")
    ggsave("../stores/mlp_clas.png")
})
