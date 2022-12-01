library("tidyverse")
library("tidytext")
library("tm")
library("udpipe")
library("doc2vec")

data <- read_csv("../stores/train.csv")
target <- read_csv("../stores/test.csv")


counts <- function(tweets) {
  # Detect emojis
  tweets$has_emoji <- tweets$text %>%
    str_detect("\\p{Extended_Pictographic}") %>%
    if_else(1, 0)

  # Detect "¡" or "¿"
  tweets$has_opening_sym <- tweets$text %>%
    str_detect("[¡¿]") %>%
    if_else(1, 0)

  # Uses double quotation symbol
  tweets$has_double_quot <- tweets$text %>%
    str_detect('"') %>%
    if_else(1, 0)

  # Uses single quotation symbol
  tweets$has_single_quot <- tweets$text %>%
    str_detect("'") %>%
    if_else(1, 0)

  # Uses url's
  tweets$has_url <- tweets$text %>%
    str_detect("https?\\S+\\s*") %>%
    if_else(1, 0)

  # Count commas
  tweets$commas <- tweets$text %>% str_count(",")

  # Count line breaks
  tweets$line_breaks <- tweets$text %>% str_count("\n")

  # Delete urls
  tweets$text <- tweets$text %>% str_remove_all("http[s]?\\S+\\s*")

  # Add filler when urls are removed
  tweets <- tweets %>% mutate(text = if_else(text == "", "enlace", text))

  # Count dots without urls
  tweets$dots <- tweets$text %>% str_count("\\.")

  # Count colons without urls
  tweets$colons <- tweets$text %>%
    str_detect(":") %>%
    if_else(1, 0)

  # Count # and @ without urls
  tweets$hash_at <- tweets$text %>%
    str_detect("[@#]") %>%
    if_else(1, 0)

  # Uses numbers outside urls
  tweets$has_numbers <- tweets$text %>%
    str_replace_all("https?\\S+\\s*", "") %>%
    str_detect("[0-9]") %>%
    if_else(1, 0)

  # Sentence lengths
  tweets <- tweets %>%
    unnest_tokens(sentence, text, token = "sentences") %>%
    mutate(sentence_length = str_count(sentence, boundary("word"))) %>%
    group_by(id) %>%
    summarise(mean_sentence_length = mean(sentence_length)) %>%
    right_join(tweets, by = "id")

  # Count stop words
  tweets <- tweets %>%
    unnest_tokens(word, text, token = "words") %>%
    filter(word %in% tm::stopwords("spanish")) %>%
    group_by(id) %>%
    count(word) %>%
    summarize(total_stop = sum(n, na.rm = TRUE)) %>%
    right_join(tweets, by = "id") %>%
    mutate(total_stop = replace_na(total_stop, 0))

  # Count non-stop words
  tweets <- tweets %>%
    unnest_tokens(word, text, token = "words") %>%
    filter(!word %in% tm::stopwords("spanish")) %>%
    group_by(id) %>%
    count(word) %>%
    summarize(total_non_stop = sum(n, na.rm = TRUE)) %>%
    right_join(tweets, by = "id") %>%
    mutate(total_non_stop = replace_na(total_non_stop, 0))

  # Delete punctuation, emojis and numbers
  tweets$text <- tweets$text %>% str_remove_all("[[:punct:]¿¡0-9\\p{Extended_Pictographic}]")


  # Lower case
  tweets$text <- tolower(tweets$text)

  # Replace non-alphabetic characters
  tweets$text <- str_replace_all(tweets$text, "[^[:alpha:]]", " ")

  # Replace double spaces
  tweets$text <- str_replace_all(tweets$text, "[[:space:]]+", " ")

  # Trim spaces
  tweets$text <- trimws(tweets$text)

  # Add filler when emojis (and url's) are removed
  tweets <- tweets %>% mutate(text = if_else(text == "", "enlaceemoji", text))

  # Change variable name for use in the training of language models
  tweets <- tweets %>% rename(doc_id = id)

  # Create doc2vec model using train data
  model <- paragraph2vec(
    x = tweets %>% select(doc_id, text),
    iter = 20,
    min_count = 5,
    threads = 3
  )

  # Assign variable names to the result of doc2vec and convert to tibble
  embedding <- as.matrix(model, which = "docs")
  colnames(embedding) <- paste0("var_", 1:50, "_embed")
  embedding <- as_tibble(embedding)

  # Bind embeddings to the rest of the data
  tweets <- bind_cols(tweets, embedding)

  # Delete stopwords
  # tweets <- tweets %>%
  #   unnest_tokens(word, text, token = "words") %>%
  #   filter(!word %in% tm::stopwords("spanish")) %>%
  #   group_by(doc_id) %>%
  #   summarize(text = str_c(str_replace_na(word, "~NA~"), collapse = " ")) %>%
  #   mutate(text = str_remove_all(text, "~NA~")) %>%
  #   right_join(tweets, by = "doc_id", suffix = c("_new", "_old")) %>%
  #   select(-text_old) %>%
  #   rename(text = text_new)

  # Pre-trained language model
  model <- udpipe_load_model(file = "../stores/spanish-gsd-ud-2.5-191206.udpipe")

  # Lemmatize
  analysis <- udpipe_annotate(model, x = tweets$text, doc_id = tweets$doc_id) %>% as_tibble()

  # Count instances of verbs in indicative mood
  analysis$mood_ind <- analysis$feats %>%
    str_count("Mood=Ind") %>%
    replace_na(0)

  # Count instances of verbs in subjunctive mood
  analysis$mood_sub <- analysis$feats %>%
    str_count("Mood=Sub") %>%
    replace_na(0)

  # Count instances of verbs in first person
  analysis$per_1 <- analysis$feats %>%
    str_count("Person=1\\|Tense") %>%
    replace_na(0)

  # Count instances of verbs in third person
  analysis$per_3 <- analysis$feats %>%
    str_count("Person=3\\|Tense") %>%
    replace_na(0)

  # Count instances of verbs in past tense
  analysis$ten_past <- analysis$feats %>%
    str_count("Tense=Past") %>%
    replace_na(0)

  # Count instances of verbs in present tense
  analysis$ten_pres <- analysis$feats %>%
    str_count("Tense=Pres") %>%
    replace_na(0)

  # Count instances of verbs in future tense
  analysis$ten_fut <- analysis$feats %>%
    str_count("Tense=Fut") %>%
    replace_na(0)

  # Count instances of verbs in infinitive
  analysis$infitive <- analysis$feats %>%
    str_count("VerbForm=Inf") %>%
    replace_na(0)

  # Count instances of reflexive pronouns
  analysis$reflexive <- analysis$feats %>%
    str_count("Reflex=Yes") %>%
    replace_na(0)

  # Count number of verbs
  analysis$verb <- analysis$upos %>%
    str_count("VERB|AUX") %>%
    replace_na(0)

  # Count number of nouns
  analysis$noun <- analysis$upos %>%
    str_count("NOUN") %>%
    replace_na(0)

  # Count number of adjectives
  analysis$adjective <- analysis$upos %>%
    str_count("ADJ") %>%
    replace_na(0)

  # Count number of pronouns
  analysis$pronoun <- analysis$upos %>%
    str_count("PRON") %>%
    replace_na(0)

  # Sum ocurrences of the variables just calculated in the context of each tweet
  analysis <- analysis %>%
    group_by(doc_id) %>%
    summarize(
      across(
        c(
          mood_ind,
          mood_sub,
          per_1,
          per_3,
          ten_past,
          ten_pres,
          ten_fut,
          infitive,
          reflexive,
          verb,
          noun,
          adjective,
          pronoun
        ),
        sum
      )
    ) %>% # Create new variables based on the variables above.
    mutate( # The variables are proportions (e.g. verbs in indicative
      across( # as a proportion of all verbs, or number of adjectives vs. number
        c( # of nouns found in each tweet)
          mood_ind,
          mood_sub,
          per_1,
          per_3,
          ten_past,
          ten_pres,
          ten_fut,
          infitive
        ),
        ~ .x / verb,
        .names = "{.col}_by_verb"
      ),
      adj_by_noun = adjective / noun,
      reflexive_by_pron = reflexive / pronoun
    ) %>% # Replace NA's by zeroes
    mutate(
      across(
        .cols = everything(),
        ~ replace(.x, is.na(.), 0)
      )
    ) %>% # Replace NAN's by zeroes
    mutate(
      across(
        .cols = everything(),
        ~ replace(.x, is.nan(.), 0)
      )
    ) %>% # Replace infinite values (division by zero) by zeroes
    mutate(
      across(
        .cols = everything(),
        ~ replace(.x, is.infinite(.), 0)
      )
    ) %>% # Most of the above variables are proportions with 1 as upper limit,
    mutate( # so all values that exceed 1 (because of an error in the syntactic
      across( # parser) were forced to be 0
        .cols = c(-doc_id, -adj_by_noun),
        ~ replace(.x, . > 1, 0)
      )
    ) %>% # Keep the variables that represent proportions and drop simple counts
    select(doc_id, contains("_by_"))

  tweets <- tweets %>% left_join(analysis, by = "doc_id")

  tweets
}

# Transform and save train data
data <- counts(data)
saveRDS(data, "../stores/data.Rds")


# Transform and save test data
target <- counts(target)
saveRDS(target, "../stores/target.Rds")
