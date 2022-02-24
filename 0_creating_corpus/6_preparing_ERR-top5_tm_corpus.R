#' ---
#' title: "Script for creating the EER + Top5 corpus"
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
#' This script takes the corpus, remove some articles that we don't want in the Corpus for
#' the topic modelling (like reply/comments). Then we clean the content of abstracts to
#' remove useless information that will biased the results.
#'
#'
#' > WARNING: This script still needs a lot of cleaning
#'
#'
#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

#' # Loading packages, paths and data
#'
#'

source("Script_paths_and_basic_objects_EER.R")

Corpus <- readRDS(here(eer_data,
                       "1_Corpus_Prepped_and_Merged",
                       "Corpus_Top5_EER.rds")) %>% 
  filter(jel_id == 1)

#' # Creating the corpus for topic-modelling
#'
#' ## Excluding non relevant articles
#'
#' We remove all articles which are comments/reply thanks to their titles.
#'
#' > Outside of the presence of abstracts before 1973, there is also the issue that
#' > we don't have JEL code for EER before 1973 and so we cannot classify these papers as
#' > macroeconomics.

bad_title <- c("- COMMENT$",
               "- COMMENTS$",
               "- REPLY$",
               ": COMMENT$",
               ": COMMENT \\[",
               "- REPLY$")

Corpus_reduced <- Corpus %>%
  filter(!str_detect(Titre, paste0(bad_title, collapse = "|")))

# nettoyage des doublons possibles
remove_doublons <- FALSE
if(remove_doublons == TRUE){
  Corpus_topic <- Corpus_reduced %>%
    mutate(test_doublon = paste(Titre, Annee_Bibliographique, Revue)) %>%
    mutate(doublon = duplicated(test_doublon)) %>%
    filter(doublon == TRUE) %>%
    select(-contains("doublon"))
}

#' ## Cleaning Abstract text with non information
#' 

#' This is to be removed before passing everything to upper cases. 
to_remove_first <- c("© .*",
                   "\\*( )?[A-Z].*") # if not followed by an upper case, it means that it is really the abstract

to_remove_second <- c("THIS ARTICLE ORIGINALLY APPEARED IN THE AMERICAN.*",
                      "PREPARED FOR THE ANNUAL MEETINGS OF THE AMERICAN ECONOMIC ASSOCIATION",
                      "\\(THIS ABSTRACT WAS BORROWED FROM ANOTHER VERSION OF THIS ITEM\\.\\)",
                      "JEL CLASSIFICATION: [A-Z]\\d{1,2}",
                      ".*KEY WORDS:.*", # the abstract containing this is not an abstract
                      "RESEARCH FOR THIS PAPER .*",
                      "-FROM AUTHOR(S)?",
                      "LECTURE TO THE MEMORY.*",
                      "DISCUSSANTS:.*",
                      "COPYRIGHT .*",
                      "N\\.B\\. .*")

Corpus_abstract_cleaned <- Corpus_reduced %>% 
  mutate(Abstract = str_remove(Abstract, paste0(to_remove_first, collapse = "|")),
         Abstract = toupper(Abstract),
         Abstract = str_replace_all(Abstract, "EURO-", "EURO"),
         Abstract = str_remove(Abstract, paste0(to_remove_second, collapse = "|")),
         Abstract = ifelse(Abstract == "", NA, Abstract),
         have_abstract = ! is.na(Abstract)) %>% 
  unite("All_text", Titre, Abstract, sep = " ") %>% 
  mutate(All_text = str_remove(All_text, " NA$"),
         id = row_number())

saveRDS(select(Corpus_abstract_cleaned, ID_Art, All_text), here(eer_data,
                                                                "1_Corpus_Prepped_and_Merged",
                                                                "Corpus_TM_cleaned.RDS"))
