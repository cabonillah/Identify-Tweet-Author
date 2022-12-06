final_st <- readRDS("../stores/final_st_4.Rds")

target <- readRDS("../stores/target.Rds")


target <- target %>% select(-text)

to_normalize <- target %>%
    select(
        total_non_stop,
        total_stop,
        mean_sentence_length,
        commas,
        line_breaks,
        dots
    ) %>%
    colnames()

target <- target %>% mutate(across(all_of(to_normalize), as.numeric, scale))

test <- recipe(~., data = target) %>%
    step_rm(doc_id) %>%
    prep() %>%
    bake(new_data = NULL)

final_pred <- predict(final_st, test) %>%
    bind_cols(target) %>%
    select(doc_id, .pred_class) %>%
    rename(name = .pred_class, id = doc_id)

form <- read_csv("../stores/test.csv")
form <- form %>%
    select(-text) %>%
    left_join(final_pred, by = "id")

write.csv(form, "../stores/bonilla_santofimio_velasquez.csv", row.names = FALSE)
