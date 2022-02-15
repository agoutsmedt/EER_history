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
#' This script aims at extracting all the articles of the EER, the references, the list of authors
#' and their affiliations. It also extracts the same data for the four missing years (1971-1973).
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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Basic Corpus ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' Importing Top 5 and EER from OST
#' 
if(str_detect(here(), "home")){
  message("Need an SQL request")
} else {
  Corpus_ost <- arrow::read_parquet(here(macro_AA_data,
                                         "OST_generic_data",
                                         "all_art.parquet"),
                                    as_data_frame = FALSE) %>% 
    filter(Code_Revue %in% c(758, 4695, 5200, 9662, 13694, 13992),
           Code_Document %in% c(1:3, 6),
           between(Annee_Bibliographique, 1969, 2002)) %>% 
    collect() %>% 
    data.table
}

#' Importing information on journals issue, to get volume, issue, and pages (Useful
#' later for matching with other databases).

issueID <- readRDS(here(macro_AA_data, 
                        "OST_generic_data",
                        "revueID.rds")) %>% 
  data.table()
Revues <- readRDS(here(macro_AA_data, 
                        "OST_generic_data",
                        "revues.RDS")) %>% 
  data.table()
Revues[,Revue:=sub("\r", "", Revue)] # remove some weird characters in journal name


Corpus_ost <- Corpus_ost %>% 
  left_join(select(issueID, Code_Revue, IssueID, Revue, Volume, Numero)) %>% 
  select(ID_Art, ItemID_Ref, Titre, Annee_Bibliographique, Revue, Volume, Numero, Page_Debut, Page_Fin) %>% 
  as.data.table()
  
# Scopus and bind
Corpus_scopus <- readRDS(here(eer_data,
                              "scopus",
                              "scopus_EER_missing_years.rds")) %>% 
  .[["articles"]] %>% 
  rename(Titre = dc_title,
         Volume = prism_volume,
         Numero = prism_issue_identifier) %>% 
  mutate(ID_Art = scopus_id,
         ItemID_Ref = ID_Art,
         Annee_Bibliographique = as.integer(year),
         Revue = toupper(prism_publication_name),
         Page_Debut = str_extract(prism_page_range, "^\\d+") %>% as.integer(),
         Page_Fin = str_extract(prism_page_range, "\\d+$") %>% as.integer()) %>% 
  select(ID_Art, ItemID_Ref, Titre, Annee_Bibliographique, Revue, Volume, Numero, Page_Debut, Page_Fin)

Corpus <- Corpus_ost %>% 
  mutate(ID_Art = as.character(ID_Art),
         ItemID_Ref = as.character(ItemID_Ref)) %>% # for binding
  bind_rows(Corpus_scopus) %>% 
  mutate(Id = ID_Art,
         Titre = toupper(Titre))

#' Identifying macro JEL or not with macro_AA data 

macro_id_art <- readRDS(here(macro_AA_data, 
                        "2_Matched_data",
                        "Econlit_matched_ID_Art.rds"))

#' Adding Scopus articles with a macro Jel
#' 

scopus_macro <- tribble(
  ~dc_identifier, ~ID_Art,
  "SCOPUS_ID:0041619337", "S3755",
  "SCOPUS_ID:0003565397", "S3777",
  "SCOPUS_ID:0002059851", "S3782",
  "SCOPUS_ID:49549169830", "S3728",
  "SCOPUS_ID:49549169639", "S3729",
  "SCOPUS_ID:49549165211", "S3732",
  "SCOPUS_ID:49549164396", "S3734"
)

macro_id_art <- macro_id_art %>% 
  mutate(ID_Art = as.character(ID_Art)) %>% 
  bind_rows(select(scopus_macro, ID_Art))

Corpus <- Corpus %>% 
  mutate(jel_id = ifelse(ID_Art %in% macro_id_art$ID_Art, 1, 0))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Authors ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
if(str_detect(here(), "home")){
  message("Need an SQL request")
} else {
  Authors_ost <- readRDS(here(macro_AA_data,
                            "OST_generic_data",
                            "all_aut.rds")) %>% 
    mutate(ID_Art = as.character(ID_Art)) %>% # for matching later
    filter(ID_Art %in% Corpus$ID_Art) %>% 
    select(ID_Art, Nom, Ordre) %>% 
    data.table
}

# Scopus and bind
Authors_scopus <- readRDS(here(eer_data,
                               "scopus",
                               "scopus_EER_missing_years.rds")) %>% 
  .[["authors"]] %>% 
  rename(ID_Art = scopus_id) %>% 
  mutate(Ordre = as.integer(seq),
         Nom = toupper(authname) %>% 
           str_replace_all(" (?=[A-Z]\\.)", "-") %>% # replace space before initial by a dash to match OST format
           str_remove_all("\\.| ")) %>% 
  select(ID_Art, Nom, Ordre)

Authors <- Authors_ost %>% 
  bind_rows(Authors_scopus) %>% 
  left_join(select(Corpus, ID_Art, Annee_Bibliographique)) %>% 
  mutate(Nom = toupper(Nom))

#' Thanks to scopus, we have spotted wrong author names in WoS, that we can now correct
#' 
correct_name <- data.table(wrong_name = c("CONSTANT-M",
                                          "GEORGAKO-T",
                                          "HJALMARS-L",
                                          "MUELLBAU-J",
                                          "TINBERGE-J",
                                          "VANDENNO-P",
                                          "VANDERLO-S",
                                          "SCHIOPPA-F",
                                          "BROWN-P",
                                          "HAGEN-O",
                                          "STPAUL-G"),
                           right_name = c("CONSTANTOPOULOS-M",
                                          "GEORGAKOPOULOS-T",
                                          "HJALMARSSON-L",
                                          "MUELLBAUER-J",
                                          "TINBERGEN-J",
                                          "VANDENNOORT-P",
                                          "VANDERLOEFF-S",
                                          "PADOASCHIOPPA-F",
                                          "CLARKBROWN-P",
                                          "VONDEMHAGEN-O",
                                          "SAINTPAUL-G"))

for(i in seq_along(correct_name$wrong_name)){
  Authors <- Authors %>% 
    mutate(Nom = str_replace_all(Nom, correct_name$wrong_name[i], correct_name$right_name[i]))
}

saveRDS(Authors, here(eer_data,
                     "1_Corpus_Prepped_and_Merged",
                     "Authors_EER_Top5.rds"))

#' Adding first author name in Corpus
#' 

Corpus <- Corpus %>% 
  left_join(filter(Authors, Ordre == 1)) %>% 
  select(-Ordre) %>% 
  relocate(Nom, .before = Titre)

saveRDS(Corpus, here(eer_data,
                     "0_To_Be_Cleaned",
                     "Corpus_EER_Top5_No_Abstract.rds"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Institutions ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
Institutions_ost <- readRDS(here(macro_AA_data,
                               "3_Corpus_WoS",
                               "MACRO_AA_INSTITUTIONS.rds")) %>% 
  mutate(ID_Art = as.character(ID_Art)) %>% 
  select(ID_Art, Institution, Pays) %>%
  filter(Institution != "NULL",
         ID_Art %in% Corpus$ID_Art) %>% # No Country is null but if it was the case we could have add the country
  data.table %>% 
  unique

# Scopus and bind
Institutions_scopus <- readRDS(here(eer_data,
                                    "scopus",
                                    "scopus_EER_missing_years.rds")) %>% 
  .[["institutions"]] %>% 
  rename(ID_Art = scopus_id) %>% 
  mutate(Pays = toupper(affiliation_country),
         Institution = toupper(affilname)) %>% 
  filter(! is.na(Institution)) %>% 
  select(ID_Art, Institution, Pays) %>% 
  mutate(Pays = ifelse(Institution == "INSTITUTE OF ECONOMIC STUDIES", "SWEDEN", Pays)) # missing country for the IES

Institutions <- Institutions_ost %>% 
  bind_rows(Institutions_scopus) %>% 
  filter(ID_Art %in% Corpus$ID_Art) %>% 
  left_join(select(Corpus, ID_Art, Annee_Bibliographique))

saveRDS(Institutions, here(eer_data,
                           "0_To_Be_Cleaned",
                           "Institutions_EER_Top5_To_Clean.rds"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### References ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

if(str_detect(here(), "home")){
  message("Need an SQL request")
} else {
  id_art <- filter(Corpus, ! str_detect(ID_Art, "^S"))$ID_Art
  references_ost <- arrow::read_parquet(here(macro_AA_data,
                                             "OST_generic_data",
                                             "all_ref.parquet"),
                                        as_data_frame = FALSE) %>% 
    filter(ID_Art %in% id_art) %>%  # can cause problem with filtering with Arrow
    collect() %>% 
    data.table

#' We add available information by joining with articles in the OST database
  itemid_ref <- filter(references_ost, ItemID_Ref != 0)$ItemID_Ref %>% 
    unique %>% 
    sort # Perhas it could accelerate the matching with arrow
  references_info <- arrow::read_parquet(here(macro_AA_data,
                                             "OST_generic_data",
                                             "all_art.parquet"),
                                        as_data_frame = FALSE) %>% 
    filter(ItemID_Ref %in% itemid_ref) %>%  # can cause problem with filtering with Arrow
    collect() %>% 
    data.table
}

references_info_unique <- references_info[,.N,.(ID_Art,ItemID_Ref)][,.N,ItemID_Ref][N==1]
references_ost_dt <- merge(references_ost, 
                           references_info[ItemID_Ref %in% references_info_unique$ItemID_Ref,.(ItemID_Ref,Titre,Code_Revue)], 
                           by="ItemID_Ref",all.x = TRUE)
references_ost_dt <- merge(references_ost_dt, 
                           Revues, 
                           by="Code_Revue",all.x = TRUE)

references_ost <- references_ost %>% 
  left_join(select(references_info, ItemID_Ref, Titre, Code_Revue) %>% unique()) %>% 
  left_join(select(issueID, Code_Revue, Revue) %>% unique()) %>% 
  select(ID_Art, ItemID_Ref, New_id2, Annee, Nom, Titre, Revue, Revue_Abbrege) %>% 
  mutate(across(contains("id"), ~ as.character(.))) %>% 
  data.table

#' We need to match the references of scopus with the original scopus id
scopus_id <- readRDS(here(eer_data,
             "scopus",
             "scopus_EER_missing_years.rds")) %>% 
  .[["articles"]] %>% 
  select(dc_identifier, scopus_id) %>% 
  rename(citing_art = dc_identifier,
         ID_Art = scopus_id)

references_scopus <- readRDS(here(eer_data,
                               "scopus",
                               "scopus_EER_missing_years.rds")) %>% 
  .[["references"]] %>% 
  left_join(scopus_id) %>% 
  mutate(ItemID_Ref = paste0("S", scopus_id), # 
         New_id2 = ItemID_Ref,
         Annee = str_extract(prism_cover_date, "^\\d+") %>% as.integer(),
         Nom = toupper(author_list_author_ce_indexed_name) %>% 
           str_replace_all(" (?=[A-Z]\\.)", "-") %>% # replace space before initial by a dash to match OST format
           str_remove_all("\\.| ")) %>%
  rename(Revue = sourcetitle,
         Titre = title) %>% 
  select(ID_Art, ItemID_Ref, New_id2, Annee, Nom, Titre, Revue) %>% 
  data.table

#' We bind the two lists of references:
references <- references_ost %>% 
  bind_rows(references_scopus) %>% 
  mutate(across(where(is.character), ~ toupper(.))) %>% 
  data.table()

saveRDS(references, here(eer_data,
                           "0_To_Be_Cleaned",
                           "References_EER_Top5_To_Clean.rds"))
