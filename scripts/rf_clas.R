tictoc::tic("Random Forest")
source("../scripts/recipes.R")
source("../scripts/workflows.R")
source("../scripts/tuning.R")

wf <- workflows("rf")

cl <- parallel::makeCluster(3)
result <- wf %>% tuning(
    resamples = validation_split,
    model = "rf"
)
parallel::stopCluster(cl)
cat("------------------------\n---------- RF ----------\n------------------------\n")
result %>%
    collect_metrics() %>%
    filter(.metric == "accuracy") %>%
    arrange(desc(mean)) %>%
    print()

# Select best model
best <- select_best(result, metric = "accuracy")
# Finalize the workflow with those parameter values
final_wf <- wf %>% finalize_workflow(best)


# See variables arranged by importance
# final_wf %>%
#     fit(train) %>%
#     extract_fit_parsnip() %>%
#     vip::vi() %>%
#     dplyr::arrange(desc(Importance)) %>%
#     print(n = Inf)

# Save workflow
saveRDS(final_wf, "../stores/bestwf_rf.Rds")

# Save result
saveRDS(result, "../stores/result_rf.Rds")


# # Fit on training, predict on test, and report performance
# lf <- last_fit(final_wf, data_split)
# # Performance metric on test set
# metric <- rmse(
#     data.frame(
#         test["Ingpcug"],
#         lf %>% extract_workflow() %>% predict(test)
#     ),
#     Ingpcug,
#     .pred
# )$.estimate

# # Final report for this model
# report <- data.frame(
#     Problema = "Reg.", Modelo = "Random Forest",
#     result %>% show_best(n = 1) %>% mutate(mean = metric)
# )

# saveRDS(report, file = "../stores/rf_reg.rds")
tictoc::toc()
