#' ---
#' title: "Script for building the EER corpus"
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
#' This script aims at merging the abstracts extracted from Microsoft Science Academic and 
#' from Scopus to our corpus coming from the OST database.
#' 
#' 
#' > WARNING: This script still needs a lot of cleaning
#' 
#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

#' # Loading packages, paths and data
#'
source("Script_paths_and_basic_objects_EER.R")
 
if(str_detect(here(), "home")){
source("~/macro_AA/logins_DO_NOT_UPLOAD.R")
ESH <- dbConnect(MySQL(),
                 user = usr, password = pswd, dbname = "OST_Expanded_SciHum",
                 host = "127.0.0.1"
)
}

#' # Extracting Microsoft Academic data
#' 
path_microsoft <- here(eer_data,
                       "Corpus_Top5",
                       "MS_Academics_AB")
MS_files <- list.files(path_microsoft)[str_which(list.files(path_microsoft), "^MS")]
Top_5_AB <- map(MS_files, ~ bib2df(here(path_microsoft, .x))) %>% 
  bind_rows() %>% 
  mutate(PAGES_START = str_extract(PAGES, "^\\d+"),
         PAGES_END = str_extract(PAGES, "\\d+$"),
         AUTHOR = as.character(AUTHOR)) %>% 
  filter(between(YEAR, 1973, 2002)) %>% # We don't need abstract before as we don't have the jel id (and it's very messy before 1973)
  select(BIBTEXKEY, AUTHOR, TITLE, JOURNAL, VOLUME, YEAR, PAGES_START, PAGES_END, ABSTRACT) %>% 
  as.data.table() %>% 
  unique

#' ## Looking at the distribution of abstracts


look_plots <- FALSE
if(look_plots == TRUE){
  # histogram
  ggplot(Top_5_AB, aes(x=as.character(YEAR))) + geom_bar() +  facet_wrap(~JOURNAL, ncol = 2, scales = "free") 
  
  # % of missing AB
  Top_5_AB %>% 
    as.data.table() %>% 
    .[,n_journals:=.N,JOURNAL] %>% 
    .[is.na(ABSTRACT)] %>%
    .[,.N,.(JOURNAL,n_journals)] %>%
    .[,N/n_journals,JOURNAL]
  
  # histogram of missing abstracts
  ggplot(Top_5_AB[is.na(ABSTRACT)], aes(x=as.character(YEAR))) + geom_bar() +  facet_wrap(~JOURNAL, ncol = 2, scales = "free") 
}


Corpus <- readRDS(here(eer_data,
                       "0_To_Be_Cleaned",
                       "Corpus_EER_Top5_No_Abstract.rds")) %>% 
  data.table %>% 
  mutate(matching_column = paste(Annee_Bibliographique, Revue, Page_Debut, Page_Fin, Volume))

#' We will first match by the information on journal issue
Top_5_AB_prepared <- Top_5_AB %>% 
  mutate(JOURNAL = toupper(JOURNAL),
         JOURNAL = ifelse(JOURNAL == "THE AMERICAN ECONOMIC REVIEW", "AMERICAN ECONOMIC REVIEW", JOURNAL),
         JOURNAL = ifelse(JOURNAL=="THE REVIEW OF ECONOMIC STUDIES", "REVIEW OF ECONOMIC STUDIES", JOURNAL),
         matching_column = paste(YEAR, JOURNAL, PAGES_START, PAGES_END, VOLUME)) %>% 
  filter(!is.na(ABSTRACT) & !str_detect(ABSTRACT, "Ñ")) %>%  # Removing unreadable abstract
  arrange(matching_column, ABSTRACT) # To be sure not to lost abstract when removing duplicates

Corpus_with_AB <- Corpus %>% 
  left_join(distinct(Top_5_AB_prepared, matching_column, .keep_all = TRUE) %>% # we want to avoid matching the abstract twice
              select(matching_column, ABSTRACT)) 

glue("On { Top_5_AB %>% filter(!is.na(ABSTRACT)) %>% nrow } abstracts for Top 5 in Microsoft
     Academic for our period, we have matched 
     { Corpus_with_AB %>% filter(!is.na(ABSTRACT)) %>% nrow} abstracts")

#' We can isolate what has not been matched and try again but with the title now:
#' we remove the first article of the title (as in WoS) and we remove punctuation to avoid
#' difference.
MS_Non_matched <- Top_5_AB_prepared %>% 
  filter(! matching_column %in% Corpus_with_AB$matching_column) %>% 
  mutate(TITLE_modify = str_remove(TITLE, "^The |^On |^A(n)? ") %>% toupper,
         TITLE_modify = str_squish(str_replace_all(TITLE_modify, "[:punct:]", " ")),
         new_matching_column = paste(YEAR, JOURNAL, TITLE_modify),
         ABSTRACT_second_matching = ABSTRACT)

Corpus_with_AB <- Corpus_with_AB %>% 
  mutate(Title_modify = str_squish(str_replace_all(Titre, "[:punct:]", " ")),
         new_matching_column = paste(Annee_Bibliographique, Revue, Title_modify)) %>% 
  left_join(select(MS_Non_matched, new_matching_column, ABSTRACT_second_matching)) %>% 
  mutate(ABSTRACT = ifelse(!is.na(ABSTRACT_second_matching), ABSTRACT_second_matching, ABSTRACT)) %>% 
  select(-c(Title_modify, ABSTRACT_second_matching))

glue("On { Top_5_AB %>% filter(!is.na(ABSTRACT)) %>% nrow } abstracts for Top 5 in Microsoft
     Academic for our period, we have matched 
     { Corpus_with_AB %>% filter(!is.na(ABSTRACT)) %>% nrow} abstracts")

#' We isolate again what is lefting, just to 
MS_Non_matched_bis <- MS_Non_matched %>% 
  filter(! new_matching_column %in% Corpus_with_AB$new_matching_column)


#' We will now match the abstracts identified with Scopus. It will allow us to add
#' abstract for the EER, but also to add abstracts for the Top 5 when not matched with
#' MS academic.
#' 
#' We first need to clean the journals as they are several different names in scopus.
#' 
#' As above, we create the same types of matching column.

journals <- c("European Economic Review",
              "American Economic Review",
              "The American Economic Review",
              "American Economic Review, Papers and Proceedings",
              "Review of Economic Studies",
              "The Review of economic studies",
              "Quarterly Journal of Economics",
              "Journal of Political Economy",
              "The journal of political economy",
              "Econometrica",
              "Econometrica : journal of the Econometric Society")

scopus_corpus <- readRDS(here(eer_data,
                              "scopus",
                              "scopus_EER-Top5_art.rds")) %>% 
  filter(prism_publication_name %in% journals,
         ! is.na(dc_description)) %>% 
  mutate(Revue = str_remove_all(prism_publication_name, "^The | :.*|,.*") %>% toupper,
         first_page = str_extract(prism_page_range, "^\\d+"),
         last_page = str_extract(prism_page_range, "\\d+$"),
         year = str_extract(prism_cover_date, "^\\d{4}"),
         matching_column = paste(year, Revue, first_page, last_page, prism_volume)) %>% 
  rename(abstract_scopus = dc_description) %>% 
  arrange(matching_column, abstract_scopus) %>% 
  filter(between(year, 1973, 2002))

Corpus_with_scopus_AB <- Corpus_with_AB %>% 
  left_join(distinct(scopus_corpus, matching_column, .keep_all = TRUE) %>% 
              select(matching_column, abstract_scopus)) 

glue("On { scopus_corpus %>% nrow } abstracts for Top 5 in Scopus for our period, 
     we have matched 
     { Corpus_with_scopus_AB %>% filter(!is.na(abstract_scopus)) %>% nrow} abstracts")

#' We look at what is lefting and try again with titles now

Scopus_Non_matched <- scopus_corpus %>% 
  filter(! matching_column %in% Corpus_with_scopus_AB$matching_column) %>% 
  mutate(title_modify = str_remove(dc_title, "^The |^On |^A(n)? ") %>% toupper,
         title_modify = str_squish(str_replace_all(title_modify, "[:punct:]", " ")),
         new_matching_column = paste(year, Revue, title_modify),
         abstract_second_matching = abstract_scopus)

Corpus_with_scopus_AB <- Corpus_with_scopus_AB %>% 
  left_join(select(Scopus_Non_matched, new_matching_column, abstract_second_matching)) %>% 
  mutate(abstract_scopus = ifelse(!is.na(abstract_second_matching), abstract_second_matching, abstract_scopus)) %>% 
  select(-abstract_second_matching)

glue("On { scopus_corpus %>% nrow } abstracts for Top 5 in Scopus for our period, 
     we have matched 
     { Corpus_with_scopus_AB %>% filter(!is.na(abstract_scopus)) %>% nrow} abstracts")

#' Just keeping here what has not been matched:
Scopus_Non_matched_bis <- Scopus_Non_matched %>% 
  filter(! new_matching_column %in% Corpus_with_scopus_AB$new_matching_column)

#' We have spot to remaining possible match that we are doing manually
manually_spot <- Scopus_Non_matched_bis %>% 
  filter(dc_identifier %in% c("SCOPUS_ID:49449120549",
                              "SCOPUS_ID:0002458040")) %>% 
  arrange(dc_identifier) %>% 
  mutate(ID_Art = c("8379909", "41857869")) %>% 
  select(ID_Art, abstract_scopus) 

Corpus_with_scopus_AB <- Corpus_with_scopus_AB %>% 
  mutate(abstract_scopus = ifelse(ID_Art == "8379909", filter(manually_spot, ID_Art == "8379909")$abstract_scopus, abstract_scopus),
         abstract_scopus = ifelse(ID_Art == "41857869", filter(manually_spot, ID_Art == "41857869")$abstract_scopus, abstract_scopus),
         Abstract = ifelse(!is.na(abstract_scopus), abstract_scopus, ABSTRACT)) %>% 
  select(-c(matching_column, ABSTRACT, new_matching_column, abstract_scopus))

saveRDS(Corpus_with_scopus_AB, here(eer_data,
                                    "1_Corpus_Prepped_and_Merged",
                                    "Corpus_top5_EER.rds"))
