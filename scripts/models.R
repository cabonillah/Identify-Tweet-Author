library("tidyverse")
library("ranger")
library("xgboost")
library("deepnet")
library("caret")
library("doParallel")
library("caretEnsemble")
library("BradleyTerry2")

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
    )

normalizer <- preProcess(
    to_normalize,
    method = c("center", "scale")
)

normalized <- predict(normalizer, to_normalize)

data <- data %>%
    select(-c(colnames(to_normalize))) %>%
    bind_cols(normalized)

set.seed(10)
train_index <- createDataPartition(data$name,
    p = .85,
    list = FALSE,
    times = 1
)
train <- data[train_index, ]
test <- data[-train_index, ]

set.seed(10)
multifold_index <- createMultiFolds(train$name, 5, 5)
rm(.Random.seed, envir = globalenv())


train_control <- trainControl(
    method = "adaptive_cv",
    index = multifold_index,
    number = 5,
    repeats = 5,
    savePredictions = "final",
    allowParallel = TRUE,
    classProbs = TRUE,
    verboseIter = TRUE,
    search = "random",
    adaptive = list(
        min = 5,
        alpha = 0.05,
        method = "BT",
        complete = TRUE
    )
)



cl <- makePSOCKcluster(3)
registerDoParallel(cl)

caret_list_res <- caretList(
    x = as.matrix(train %>% select(-name)),
    y = train$name,
    methodList = c("ranger", "xgbTree", "dnn"),
    metric = "Accuracy",
    trControl = train_control,
    tuneLength = 100
)

stopCluster(cl)

saveRDS(caret_list_res, "../stores/caret_list_res.Rds")
