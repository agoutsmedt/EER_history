#' ---
#' title: "Script for choosing the preprocessing steps and number of topics"
#' author: "Aurélien Goutsmedt and Alexandre Truc"
#' date: "/ Last compiled on `r format(Sys.Date())`"
#' output: 
#'   github_document:
#'     toc: true
#'     number_sections: true
#' ---
#' 
#' # What is this script for?
#' 
#' Here, we prepare the text of our corpus, we clean the tokens, and then we run different
#' topic models depending of the preprocessing steps and the number of topics. At the end
#' we are able to choose which preprocessing steps and which number of topics we will use
#' in the next script.
#' 
#' > WARNING: This script still needs a lot of cleaning
#' 
#' 
#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

################# Loading packages, paths and data ################### ------
#' # Loading packages, paths and data
#' 
#' 
source("Script_paths_and_basic_objects_EER.R")
source(here("functions", 
            "functions_for_topic_modelling.R"))

# importing corpus
Corpus_text <- readRDS(here(eer_data, 
                       "1_Corpus_Prepped_and_Merged",
                       "Corpus_TM_cleaned.rds"))

#' # Topic modelling on titles and abstracts
#' 
#' 
#' ## Choosing the preprocessing steps and the number of topics
#' 
#' We will run the topic model analysis only on the articles with an abstract. As a significant
#' proportion of macro articles in the late 1970s, early 1980s lack of an abstract, we will be forced
#' in a second step to run the same analysis with the articles without abstracts.

remove_words <- data.table(word = c("paper",
                                    "article",
                                    "datum",
                                    "contribution",
                                    "study",
                                    "show",
                                    "find",
                                    "imply",
                                    "analyze",
                                    "compare",
                                    "literature",
                                    "discuss",
                                    "focus",
                                    "consider",
                                    "characterize",
                                    "conclusion",
                                    "demonstrate",
                                    "finally",
                                    "significantly",
                                    "explore",
                                    "ii",
                                    "iii",
                                    "iv",
                                    "examine",
                                    "author",
                                    "section"),
                           lexicon = "own_built") # We remove typical abstract words and figures
stop_words<- rbind(stop_words, remove_words) %>% 
  distinct(word)
remove_expressions <- c("'s", "\\.")

get_stopwords(source = "smart")

term_list <- Corpus_text %>% 
  unnest_tokens(word, All_text, token = "ngrams", n_min = 1, n = 3, drop = FALSE) %>% 
  separate(word, into = c("word_1", "word_2", "word_3"), sep = " ") %>% 
  mutate(unigram = is.na(word_2) & is.na(word_3),
         bigram = !is.na(word_2) & is.na(word_3),
         ngram = ifelse(unigram == TRUE, "unigram", NA),
         ngram = ifelse(bigram == TRUE, "bigram", ngram),
         ngram = ifelse(is.na(ngram), "trigram", ngram),
         word_2 = ifelse(is.na(word_2), "", word_2),
         word_3 = ifelse(is.na(word_3), "", word_3),
         word_1 = str_remove_all(word_1, paste0(remove_expressions, collapse = "|")),
         word_2 = str_remove_all(word_2, paste0(remove_expressions, collapse = "|")),
         word_3 = str_remove_all(word_3, paste0(remove_expressions, collapse = "|")),
         word_1 = textstem::lemmatize_words(word_1),
         word_2 = textstem::lemmatize_words(word_2),
         word_3 = textstem::lemmatize_words(word_3)) %>% 
  filter(! str_detect(word_1, "[:digit:]"),
         ! str_detect(word_2, "[:digit:]"),
         ! str_detect(word_3, "[:digit:]"),
         str_count(word_1) != 1,
         str_count(word_2) != 1,
         str_count(word_3) != 1) %>%
  anti_join(stop_words, by = c("word_1" = "word")) %>% 
  anti_join(stop_words, by = c("word_2" = "word")) %>% 
  anti_join(stop_words, by = c("word_3" = "word")) %>% 
  unite(term, word_1, word_2, word_3, sep = " ") %>% 
  mutate(term = str_trim(term, "both")) %>% 
  select(-unigram, -bigram) %>% 
  data.table()

saveRDS(term_list, here(eer_data,
                        "3_Topic_modelling", 
                        "TM_term_list.rds"))

#' We will now produce different set of data depending on different filtering parameters:
#' 
hyper_grid <- expand.grid(
  upper_share = c(0.35), # remove a word if it is appearing in more than upper_share% of the docs
  lower_share = c(0.005, 0.01, 0.02), # remove a word if it is appearing in less than lower_share% of the docs
  min_word = c(6, 12), # the min number of words in a doc
  max_word = Inf, # the max number of words in a doc
  prop_word = 1) # keep the top prop_word% of the words (in terms of occurrence)

#' The first step is to use a function to create a list of words data depending on different
#' feature selection criteria (listed in `hyper_grid`).

data_set_trigram <- create_topicmodels_dataset(hyper_grid, 
                                       term_list, 
                                       document_name = "ID_Art") %>% 
  mutate(trigram = TRUE) 

data_set_bigram <- create_topicmodels_dataset(hyper_grid, 
                                              term_list[ngram != "trigram"], 
                                              document_name = "ID_Art") %>% 
  mutate(trigram = FALSE)

data_set <- rbind(data_set_trigram, data_set_bigram) %>% 
  relocate(trigram, .after = prop_word)

saveRDS(data_set, here(eer_data, 
                       "3_Topic_modelling",
                       "TM_data_set.rds"))

#' The second step is to use the different data sets to create stm objects and them to fit 
#' topic models for different number of topics.
#' 

# setting up parallel process
nb_cores <- availableCores()/2
plan(multisession, workers = nb_cores)

data_set <- create_stm(data_set)
topic_number <- seq(40, 100, 5) 
many_models <- create_many_models(data_set, topic_number, max.em.its = 800, seed = 1989)

#' The third step is to calculate different statistics for each model and produce 
#' different plots summarising these statistics.

tuning_results <- stm_results(many_models)
#' If needed, we can save the result: 
#' `saveRDS(tuning_results, here(eer_data, "3_Topic_modelling", "topic_models.rds"))`.
#' 
#' And reload them at the beginning of a new session: 
#' `tuning_results <- readRDS(here(eer_data, "3_Topic_modelling", "topic_models.rds"))`.
#'
#' We can now project the different statistics to choose the best model(s).
plot_topic_models  <- plot_topicmodels_stat(tuning_results, nb_terms = 300)

plot_topic_models$summary %>%
  ggplotly() %>% 
  htmlwidgets::saveWidget(here(picture_path, "topic_modelling", "tuning_topicmodels_summary.html"))

ragg::agg_png(here(picture_path, "topic_modelling", "tuning_topicmodels_coherence_vs_exclusivity.png"),
        width = 20, height = 15, units = "cm", res = 300)
plot_topic_models$exclusivity_coherence
invisible(dev.off())

plot_topic_models$exclusivity_coherence_mean %>%
  ggplotly() %>% 
  htmlwidgets::saveWidget(here(picture_path, "topic_modelling", "tuning_topicmodels_frex_general.html"))

#' For remembering the different preprocessing steps: 
#' `tuning_results %>% select(lower_share, min_word, trigram, preprocessing_id) %>% unique`

#' If we want to look at the stat in an interactive framework, we can do:
#' 
#' - `plot_topic_models$summary %>% ggplotly()`;
#' - `plot_topic_models$exclusivity_coherence_mean %>% ggplotly()`

#' For `r Sys.date()`, we rely on exclusivity and coherence and we see that when we 
#' ponderate the two indicators, we have no gain after 50 topics for most of the 
#' preprocessing steps. We choose the first preprocessing step because that the one who
#' gave the best equilibrium between the two indicators.
#' 

saveRDS(filter(tuning_results, preprocessing_id == 6, K == 70), 
        here(eer_data, "3_Topic_modelling", "chosen_topic_models.rds"))
