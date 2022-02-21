#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### References ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#' ---
#' title: "Script for building the EER references"
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
#' This script aims at cleaning all the refefrences of the EER and top 5 Macro
#' 
#' 
#'
#' 
#+ r setup, include = FALSE
# knitr::opts_chunk$set(eval = FALSE)

#' # Loading packages, paths and data
#'

source("Script_paths_and_basic_objects_EER.R")
to_clean <- FALSE

Refs <- readRDS(here(eer_data, 
                     "0_To_Be_Cleaned",
                     "References_EER_Top5_To_Clean.rds"))

Corpus <- readRDS(here(eer_data,
                       "0_To_Be_Cleaned",
                       "Corpus_EER_Top5_No_Abstract.rds"))

Refs_macro <- Refs %>% 
  filter(ID_Art %in% filter(Corpus, jel_id == 1)$ID_Art)

#' We put a small test here to check that the `New_id2` are different from the 
#' `ItemID_Ref` as it would be a problem when merging both in a new id column.
test <-  Refs %>% 
  filter(ItemID_Ref == New_id2,
         ItemID_Ref != 0,
         New_id2 != 0,
         ! str_detect(ItemID_Ref, "^S"))
if(nrow(test) > 0){
  message("We have a problem: some New_id2 are the same than ItemID_Ref")
} else {
  message("No similar New_id2 and ItemID_Ref")
}

#' # Checking  `New_id2`
#' 
#' We remove doublons of Item (`ItemID_Ref`)/id2 (`New_id2`) couple. We have different cases
#' we want to manage:
#' 
#' - We have id2 that split (wrongly) non-null Item. We want to keep the Item and do not care of these
#' id2;
#' - We have multiple non-null Item associated to one id2. We want to check if these unique
#' id2 are right in merging the Item. 
#' - We have id2 associated to null Item. We want to check if the id2 is secure or if it
#' risks to associate together different references. By removing doublons, we are leaving
#' only one occurence 
#' 
#' ## Merging `ItemID_Ref` together if true `New_id2`
#' 
#' 

if(isTRUE(to_clean)){
cleaning_item <- Refs_macro %>% 
  select(ItemID_Ref, New_id2, Nom, Annee, Revue_Abbrege, Titre) %>% 
  filter(ItemID_Ref != 0,
         New_id2 != 0,
         ! str_detect(ItemID_Ref, "^S")) %>% 
  distinct(ItemID_Ref, New_id2, .keep_all = TRUE) %>% # remove double occurence of the couple
  add_count(New_id2, name = "id_count") %>% # only new_id that appears at least twice (meaning = that have at least two items)
  filter(id_count > 1) %>% 
  group_by(New_id2) %>% 
  mutate(ItemID_Ref = as.integer(ItemID_Ref), # weird ranking of the minimum item if not
         New_item = min(ItemID_Ref), # by default we give the minimun, and we will correct manually
         good_new_id2 = TRUE) %>%  # if wrong, we put FALSE and do not associate the minimum ItemID_Ref
  arrange(New_id2, Nom, Annee, Revue_Abbrege, Titre, ItemID_Ref)


write_excel_csv2(cleaning_item,
                 here(eer_data,
                      "0_To_Be_Cleaned",
                      "ItemID_Ref_to_clean.csv"))


#' ## Checking  `New_id2` associated with null `ItemID_Ref`
#' 
#' By joining with `cleaned_item`, we give to the New_id2 which are also associated
#' to a positive item, the minimum item identified in the step below.

cleaning_new_id <- Refs_macro %>% 
  select(ItemID_Ref, New_id2, Nom, Annee, Revue_Abbrege) %>% 
  filter(ItemID_Ref == 0,
         New_id2 != 0,
         ! str_detect(ItemID_Ref, "^S")) %>% 
  add_count(New_id2, name = "id_count") %>% 
  distinct(New_id2, .keep_all = TRUE) %>% 
  filter(id_count > 1) %>% 
  mutate(New_id2 = as.integer(New_id2)) %>% # for joining and arranging
  left_join(select(cleaned_item, New_id2, New_item, good_new_id2) %>% unique(), by = c("New_id2" = "New_id2")) %>% 
  mutate(good_new_id2 = ifelse(is.na(good_new_id2), TRUE, good_new_id2)) %>% # it is more likely to have right new_id2 so easier to just put some FALSE when false
  arrange(Nom, Annee, Revue_Abbrege, New_item)

  write_excel_csv2(cleaning_new_id,
                 here(eer_data,
                      "0_To_Be_Cleaned",
                      "New_id2_to_clean.csv"))


#' ## Addin the new id: `id_clean`
#' 
#' We reimport after manual cleaning and we save the association
#' 
cleaned_item <- read_csv2(here(eer_data,
                               "0_To_Be_Cleaned",
                               "ItemID_Ref_cleaned.csv")) %>% 
  mutate(id_clean = ifelse(good_new_id2 == FALSE, ItemID_Ref, New_item)) # We keep the original ItemID_ref if New_id2 is wrong (does not allow to merge items)

#' We reimport after manual cleaning and we save the association
#' 
cleaned_new_id <- read_csv2(here(eer_data,
                               "0_To_Be_Cleaned",
                               "New_id2_cleaned.csv")) %>% 
  mutate(id_clean = ifelse(is.na(New_item) & good_new_id2 == TRUE, New_id2, New_item)) # could do it in `New_item` but that's more than an item as we use new_id2 too

#' We saved a simple list of Item and id2 associated with the cleaned id, as well as if they
#' are right or wrong. We can progressively increase this list
all_cleaned <- cleaned_item %>% 
  select(ItemID_Ref, New_id2, id_clean, good_new_id2) %>% 
  bind_rows(select(cleaned_new_id, ItemID_Ref, New_id2, id_clean, good_new_id2))

write_csv2(all_cleaned, here(eer_data,
                  "0_To_Be_Cleaned",
                  "merged_id.csv"))

saveRDS(all_cleaned, here(eer_data,
                             "0_To_Be_Cleaned",
                             "merged_id.rds"))
}

#' We can now integrate the cleaned id from the list of correction saved
#' 
all_cleaned <- readRDS(here(eer_data,
                            "0_To_Be_Cleaned",
                            "merged_id.rds")) %>% 
  mutate(across(where(is.double), ~ as.character(.)),
         id_clean = ifelse(is.na(id_clean), 0, id_clean)) # as for ItemID_Ref or New_ID2, we put 0 if no ID.

Refs_cleaned <- Refs %>% 
  left_join(all_cleaned) %>% 
  mutate(id_clean = ifelse(is.na(id_clean), ItemID_Ref, id_clean)) %>% 
  select(-good_new_id2) 

#' ## Cleaning Scopus
#' 
#' Here, the goal is to merge Scopus id with WoS id. It will be quicker to do it.
#' 

if(isTRUE(to_clean)){
  Refs_cleaned %>% 
    filter(str_detect(ItemID_Ref, "^S"),
           ID_Art %in% Refs_macro$ID_Art) %>% # we clean only articles with a jel code
    select(ItemID_Ref, Annee, Nom, Titre, Revue) %>% 
    mutate(id_clean = NA) %>% 
    arrange(Nom, Annee, Titre, ItemID_Ref) %>% 
    distinct(ItemID_Ref, .keep_all = TRUE) %>% 
    write_excel_csv2(here(eer_data,
                          "0_To_Be_Cleaned",
                          "scopus_refs_to_merge.csv"))
}

#' We reimport the merged file and merged with the whole ref file
#' 

scopus_refs_cleaned <- read_csv2(here(eer_data,
                                      "0_To_Be_Cleaned",
                                      "scopus_refs_merged.csv")) %>% 
  filter(! is.na(id_clean)) %>% 
  rename(new_scopus_id = id_clean)

direct_citations <- Refs_cleaned %>% 
  left_join(select(scopus_refs_cleaned, ItemID_Ref, new_scopus_id)) %>% 
  mutate(id_clean = ifelse(! is.na(new_scopus_id), new_scopus_id, id_clean)) %>% 
  select(-c(new_scopus_id, New_id2)) %>% 
  as.data.table()

saveRDS(select(direct_citations, ID_Art, ItemID_Ref, id_clean), 
               here(eer_data,
                    "1_Corpus_Prepped_and_Merged",
                    "Direct_citations_top5_EER.rds"))

#' We save a different file with just the list of references with the more information
#' we can collect. We will remove doublons by keeping the row with more information.
#' 

important_info <- c("Annee",
                    "Nom",
                    "Titre",
                    "Revue")
references <- direct_citations %>% 
  filter(id_clean != 0) %>% 
  mutate(nb_na = rowSums(!is.na(select(., all_of(important_info)))),
         length_revue_abbrege = str_count(Revue_Abbrege)) %>% 
  group_by(id_clean) %>% 
  slice_max(order_by = c(nb_na, -length_revue_abbrege), n = 1, with_ties = FALSE) %>% # That's a way to keep the ref with the more information
  select(-c(nb_na, length_revue_abbrege, ID_Art, ItemID_Ref)) %>% 
  relocate(id_clean, .before = everything()) %>% 
  as.data.table

saveRDS(references, here(eer_data,
                               "1_Corpus_Prepped_and_Merged",
                               "References_top5_EER.rds"))
