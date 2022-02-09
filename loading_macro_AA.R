# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## LOADING Macro_AA ####################################--------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

source("Script_paths_and_basic_objects_EER.R")

######################### data ##########################################------------

# Loading Macro articles
nodes_JEL <- readRDS(here(macro_data, 
                          "JEL_matched_corpus_nodes.rds"))
nodes_old_JEL <- readRDS(here(macro_data, 
                              "Old_JEL_matched_corpus_nodes.rds"))
nodes_JEL <- rbind(nodes_JEL, nodes_old_JEL)
rm("nodes_old_JEL")

edges_JEL <- readRDS(here(macro_data, 
                          "JEL_matched_corpus_edges.rds"))
edges_old_JEL <- readRDS(here(macro_data, 
                              "Old_JEL_matched_corpus_edges.rds"))
edges_JEL <- rbind(edges_JEL, edges_old_JEL)
rm("edges_old_JEL")

authors_JEL <- readRDS(here(macro_data, 
                            "JEL_matched_corpus_authors.rds"))
authors_old_JEL <- readRDS(here(macro_data, 
                                "Old_JEL_matched_corpus_authors.rds"))
authors_JEL <- rbind(authors_JEL, authors_old_JEL)

# Cleaning ID of edges:
# We check for doublons in both new_id and ItemID_Ref

check_id <- edges_JEL[
  ItemID_Ref != 0 & New_id2 != 0,
  c("ItemID_Ref", "New_id2")
] %>%
  unique() %>%
  as.data.table()

null_ItemID_Ref <- edges_JEL[
  ItemID_Ref == 0 & New_id2 != 0,
  c("ItemID_Ref", "New_id2")
] %>%
  unique() %>%
  select(New_id2) %>%
  as.data.table()

test_1 <- check_id[New_id2 %in% null_ItemID_Ref]
if (nrow(test_1) == 0) {
  message("New_id2 associated with a null ItemID_Ref are not associated to any other positive ItemID_Ref")
} else {
  message("Warning: New_id2 associated with a null ItemID_Ref are also associated to positive ItemID_Ref")
}

check_id <- check_id %>%
  mutate(doublons = duplicated(ItemID_Ref)) %>%
  filter(doublons == FALSE) %>%
  mutate(new_id = New_id2)

edges_JEL <- edges_JEL %>%
  left_join(check_id[, c("ItemID_Ref", "new_id")]) %>%
  mutate(new_id = ifelse(is.na(new_id), New_id2, new_id))

institutions_info_JEL <- read_csv2(here(macro_data, 
                                        "Macro_AA_Institutions_Cleaned.csv")) %>% 
  data.table()

ref_info_JEL <- readRDS(here(macro_data, 
                             "JEL_matched_corpus_references_info.rds"))
ref_info_old_JEL <- readRDS(here(macro_data, 
                                 "Old_JEL_matched_corpus_references_info.rds"))
ref_info_JEL <- unique(rbind(ref_info_JEL, ref_info_old_JEL))

# keeping only refs with a title and a ESpecialite, then removing doublons
ref_info_JEL <- unique(ref_info_JEL[Titre != "NA" & ESpecialite != "NA", c("New_id2", "Titre", "ESpecialite")])
doublons <- which(duplicated(ref_info_JEL$New_id2))

if (length(doublons) != 0) {
  ref_info_JEL <- ref_info_JEL[-doublons]
}


# Adding info to references
edges_JEL <- merge(unique(edges_JEL), 
                   unique(ref_info_JEL[Titre != "NA" & ESpecialite != "NA", c("New_id2", "Titre", "ESpecialite")]), 
                   by = "New_id2", 
                   all.x = TRUE)
edges_JEL <- merge(edges_JEL, 
                   unique(nodes_JEL[, c("ID_Art", "Annee_Bibliographique")]), 
                   by = "ID_Art", 
                   all.x = TRUE)

# Adding institutions to Articles
institutions_info_JEL$ID_Art <- as.integer(institutions_info_JEL$ID_Art)
institutions_info_JEL$Ordre <- as.integer(institutions_info_JEL$Ordre)
authors_JEL <- merge(authors_JEL, institutions_info_JEL, by = c("ID_Art", "Ordre"), all.x = TRUE)

# removing useless files

rm(ref_info_JEL)
rm(institutions_info_JEL)

# passing name column to upper letters
edges_JEL <- edges_JEL[, Nom := toupper(Nom)]
authors_JEL <- authors_JEL[, Nom := toupper(Nom)]
nodes_JEL <- nodes_JEL[, Nom := toupper(Nom)]