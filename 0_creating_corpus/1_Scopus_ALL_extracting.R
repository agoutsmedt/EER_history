#' ---
#' title: "Script for extracting and cleaning data of EER and Top 5 from Scopus
#' author: "Aurélien Goutsmedt"
#' date: "/ Last compiled on `r format(Sys.Date())`"
#' output: 
#'   github_document:
#'     toc: true
#'     number_sections: true
#' ---
#' 
#' # What is this script for?
#' 
#' This script aims at extracting scopus data from the API:
#' 
#' - We take all articles published in the EER and Top 5 between 1969 and 2002. Here, 
#' we are interested in the abstracts.
#' - For EER articles that are missing in the OST database (1971 to 1973), we also take
#' information on affiliations, authors, and references.
#' 
#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)


#' # Loading packages, paths and data
#' 

source("Script_paths_and_basic_objects_EER.R")

if(str_detect(here(), "goutsmedt")){
path_key <- here(str_remove(here(), "/EER_history"),
                 "tools", 
                 "scopus",
                 "scopus_api_key.txt")
file <- read_lines(path_key)
api_key <- str_extract(file[3], "(?<=Key: ).*")
insttoken <- str_extract(file[4], "(?<=insttoken: ).*")

rscopus::set_api_key(api_key)
insttoken <- rscopus::inst_token_header(insttoken)

#' We can only download 5000 items in one query, so we create a function and split 
#' our query in two (we have around 9000 items in total)

scopus_complex_query <- function(journals, years){
journals_query <- paste0("ExactSRCTitle(\"", journals, "\")", collapse = " OR ")
years_query <- paste0("PUBYEAR = ", years, collapse = " OR ")
scopus_query <- rscopus::scopus_search(paste0("DOCTYPE(ar)", journals_query, years_query, collapse = " AND "), 
                                         view = "COMPLETE",
                                         count = 25,
                                         headers = insttoken)
scopus_data_raw <- rscopus::gen_entries_to_df(scopus_query$entries)
}

journals <- c("American Economic Review",
              "European Economic Review",
              "Quarterly Journal of Economics",
              "Econometrica",
              "Review Of Economic Studies",
              "Journal Of Political Economy")
years_1 <- 1969:1991
years_2 <- 1992:2002
scopus_data_raw_1 <- scopus_complex_query(journals, years_1)
scopus_data_raw_2 <- scopus_complex_query(journals, years_2)

scopus_papers <- scopus_data_raw_1$df %>% 
  bind_rows(scopus_data_raw_2$df) %>% 
  clean_names() %>% # important to clean scopus column names as they are hard to use then.
  data.table()

#' We save these information as we will need it later to match with the OST data: it will
#' give us the abstract for the European Economic Review and for the Top 5 (to complete
#' abstracts from Microsoft Academic)

saveRDS(scopus_papers, 
        here(eer_data, 
             "scopus",
             "scopus_EER-Top5_art.rds"))

#' We extract the EER articles that are missing in OST data. We can then collect affiliations,
#' authors and references

EER_missing_art <- scopus_papers %>% 
  filter(prism_publication_name == "European Economic Review") %>% 
  mutate(year = str_extract(prism_cover_date, "\\d{4}"),
         scopus_id = paste0("S", entry_number)) %>% 
  filter(between(year, 1971, 1973))

EER_affiliations <- scopus_data_raw_1$affiliation %>% 
  clean_names() %>% 
  filter(entry_number %in% EER_missing_art$entry_number) %>% 
  mutate(scopus_id = paste0("S", entry_number)) %>% 
  data.table

EER_authors <- scopus_data_raw_1$author %>% 
    clean_names() %>% 
    filter(entry_number %in% EER_missing_art$entry_number) %>% 
    mutate(scopus_id = paste0("S", entry_number)) %>% 
    data.table

#' We need to do a loop to extract references of EER missing articles one by one.

citing_articles <- EER_missing_art$dc_identifier # extracting the IDs of our articles
citation_list <- list()

for(i in 1:length(citing_articles)){
  citations_query <- abstract_retrieval(citing_articles[i],
                                        identifier = "scopus_id",
                                        view = "REF",
                                        headers = insttoken)
  citations <- gen_entries_to_df(citations_query$content$`abstracts-retrieval-response`$references$reference)
  
  message(i)
  if(length(citations$df) > 0){
    message(paste0(citing_articles[i], " is not empty."))
    citations <- citations$df %>% 
      as_tibble(.name_repair = "unique") %>%
      select_if(~!all(is.na(.)))
    
    citation_list[[citing_articles[i]]] <- citations
  }
}

EER_references <- bind_rows(citation_list, .id = "citing_art") %>% 
  clean_names() %>% 
  data.table()

#' We can finally bind all these information in a list, to have them at the same place
  
scopus_EER_missing <- list("articles" = EER_missing_art,
                           "institutions" = EER_affiliations,
                           "authors" = EER_authors,
                           "references" = EER_references)
saveRDS(scopus_EER_missing, 
        here(eer_data, 
             "scopus",
             "scopus_EER_missing_years.rds"))
}
