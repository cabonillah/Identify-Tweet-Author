library("tidyverse")
library("tidymodels")

data <- readRDS("../stores/data.Rds")

data <- data %>% select(-c(doc_id, text))
data$name <- as.factor(data$name)

to_normalize <- data %>%
    select(
        total_non_stop,
        total_stop,
        mean_sentence_length,
        commas,
        line_breaks,
        dots
    ) %>%
    colnames()

data <- data %>% mutate(across(all_of(to_normalize), as.numeric))


# Create train and test samples
set.seed(10)
data_split <- data %>% initial_split(prop = 0.85, strata = name)
train <- data_split %>% training()
test <- data_split %>% testing()

# # Distuinguish between validation and training sets
# train$id <- seq(nrow(train))
# set.seed(10)
# train_temp <- train %>%
#     group_by(name) %>%
#     dplyr::sample_frac(0.70)
# validation <- dplyr::anti_join(train, train_temp, by = "id") %>% select(-id)
# train <- train_temp %>% select(-id)

# rm(train_temp)


set.seed(10)
validation_split <- vfold_cv(train, v = 5, strata = name)

# Recipe to prepare data for classification

rec <- recipe(name ~ ., data = train) %>%
    step_normalize(all_of(to_normalize))
