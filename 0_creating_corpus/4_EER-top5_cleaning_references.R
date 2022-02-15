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


Refs <- readRDS(here(eer_data, 
                     "0_To_Be_Cleaned",
                     "References_EER_Top5_To_Clean.rds"))
Corpus <- readRDS(here(eer_data,
                       "0_To_Be_Cleaned",
                       "Corpus_EER_Top5_No_Abstract.rds"))

# Keeping only refs from macro articles
Refs_macro <- Refs[ID_Art %in% Corpus[jel_id==1]$ID_Art]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Cleaning between ItemID_Ref and New_id2 ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Take unique observation when multiple refs for one article
Refs[ItemID_Ref!=0,.N,.(ID_Art,ItemID_Ref)][order(-N)]

Corpus[ID_Art==5797613] %>% as.tibble()

Refs_macro <- Refs[ID_Art %in% Corpus[jel_id==1]$ID_Art]



# Take unique observation when multiple refs for one article
refs_Claveau_unique <- refs_Claveau[ItemID_Ref_old_claveau!=0, head(.SD, 1), .(ID_Art_Source,ItemID_Ref_old_claveau)]
refs_Claveau <- rbind(refs_Claveau_unique,refs_Claveau[ItemID_Ref_old_claveau==0])
if(refs_Claveau[ItemID_Ref_old_claveau!="0",.N,.(ID_Art_Source,ItemID_Ref_old_claveau)][N>1][,.N]>1){print("ALERTE")}

# ItemID_Ref as new_id2, excepted when there is no New_id2 but an ItemID_Ref
refs_Claveau[,ItemID_Ref_Target:=as.character(ItemID_Ref_old_claveau)]
refs_Claveau[ItemID_Ref_old_claveau==0 & New_id2!=0,ItemID_Ref_Target:=paste0("cl",New_id2)]

# ################  Main_BD %%%%%%%%%%%%
# test <- fread("EER/Corpus_EER/EER_REFS_XP.csv", quote="") %>% data.table
# # Remove stupid doubles from stupid database having multiple ItemID_Ref for same ID_Art
# list_error_merge_title <- refs[ItemID_Ref_Target!="NULL",.N,.(ID_Art_Source,ItemID_Ref_Target)][N>1]
# refs[ItemID_Ref_Target %in% list_error_merge_title$ItemID_Ref_Target, Code_Discipline:="NULL"]
# refs[ItemID_Ref_Target %in% list_error_merge_title$ItemID_Ref_Target, Code_Revue:="NULL"]
# refs[ItemID_Ref_Target %in% list_error_merge_title$ItemID_Ref_Target, Titre:="NULL"]
# # Take unique observation when multiple refs for one article
# refs_unique <- refs[ItemID_Ref_Target!="NULL", head(.SD, 1), .(ID_Art_Source,ItemID_Ref_Target)] 
# refs <- rbind(refs_unique,refs[ItemID_Ref_Target=="NULL"])
# 
# refs[ItemID_Ref_Target!="NULL",.N,.(ID_Art_Source,ItemID_Ref_Target)][N>1]
# 
# #test to compare both ref tables
# refs_Claveau[ID_Art_Source %in% Corpus[Annee_Bibliographique<=2015]$ID_Art, .N]
# refs[ID_Art_Source %in% Corpus[Annee_Bibliographique<=2015]$ID_Art, .N]
# 
# ################  Bind all %%%%%%%%%%%%
# #Bind both
# refs <- rbind(refs[ItemID_Ref_Target!="NULL"], 
#               refs_Claveau[ItemID_Ref_old_claveau==0], 
#               fill=TRUE)

refs <- refs_Claveau[ID_Art_Source %in% Corpus$ID_Art]

# put everything in place for cleaning, we now have 83 153 refs with 58 646 unique refs
cleaning <- refs[ItemID_Ref_Target != 0, n_aut_year_couple:=.N,.(Nom,Annee)]
cleaning <- refs[ItemID_Ref_Target != 0, n_cit:=.N,ItemID_Ref_Target]

cleaning <- cleaning[ItemID_Ref_Target != 0][order(-n_aut_year_couple, Nom, Annee, ItemID_Ref_Target),.(n_aut_year_couple,n_cit, ItemID_Ref_Target, Nom, Annee, Titre, Revue_Abbrege)]
cleaning <- cleaning[!duplicated(cleaning)]
cleaning[,ItemID_Ref_Target:=as.character(ItemID_Ref_Target)]

#export and import the cleaned version for [ItemID_Ref_Target != 0]
write.csv(cleaning, "EER/Corpus_EER/cleaning_within_newid2.csv")
cleaned_refs <- fread("EER/Corpus_EER/cleaning_within_newid2_cleaned.csv") %>% data.table
cleaned_refs<- cleaned_refs[,.(New_ItemID_Ref_Target,ItemID_Ref_Target,New_Titre)][New_ItemID_Ref_Target!="" | New_Titre!=""]
cleaned_refs<- cleaned_refs[!duplicated(cleaned_refs)]

refs_final <- merge(refs, cleaned_refs, by = "ItemID_Ref_Target", all.x = TRUE, all.y = FALSE)
refs_final[New_ItemID_Ref_Target!="", ItemID_Ref_Target:=New_ItemID_Ref_Target]
refs_final[New_Titre!="", Titre:=New_Titre]
refs <- copy(refs_final)

#export and import the cleaned version for [ItemID_Ref_old_claveau==0 & New_id2!=0 & n_aut_year_couple>5]
write.csv(refs[ItemID_Ref_old_claveau==0 & New_id2!=0 & n_aut_year_couple>5][order(Nom,Annee,ItemID_Ref_Target)], "EER/Corpus_EER/cleaning_within_newid2_step2.csv")
cleaned_refs <- fread("EER/Corpus_EER/cleaning_within_newid2_step2_cleaned.csv") %>% data.table
cleaned_refs<- cleaned_refs[,.(New_ItemID_Ref_Target,ItemID_Ref_Target)][New_ItemID_Ref_Target!=""]
cleaned_refs<- cleaned_refs[!duplicated(cleaned_refs)]
refs[,New_ItemID_Ref_Target:=NULL]
refs_final <- merge(refs, cleaned_refs, by = "ItemID_Ref_Target", all.x = TRUE, all.y = FALSE)
refs_final[is.na(New_ItemID_Ref_Target)==FALSE, ItemID_Ref_Target:=New_ItemID_Ref_Target]
# we cleaned the corpus twice, we now have 83 153 refs 57 451 unique refs
refs <- copy(refs_final)
# remove institutionnal papers we now have 81 083 and 55 583 unique
refs <- refs[str_detect(Nom,"\\*")==FALSE]

refs[Revue_Abbrege %like% "HDB EC GROW%"]
write.csv(refs[ID_Art_Source %in% Corpus[Annee_Bibliographique<=1980]$ID_Art][ItemID_Ref_Target!=0,.N,.(ItemID_Ref_Target,Nom,Annee, Titre, Revue_Abbrege)][order(Nom,Annee,Revue_Abbrege,Titre)], "EER/Corpus_EER/explore_refs_last_check.csv")


#last cleaning
cleaned_refs <- fread("EER/Corpus_EER/explore_refs_last_check_cleaned.csv") %>% data.table
cleaned_refs<- cleaned_refs[,.(New_ItemID_Ref_Target,ItemID_Ref_Target,New_Titre, New_Nom)][New_ItemID_Ref_Target!="" | New_Titre!="" | New_Nom!=""]
cleaned_refs<- cleaned_refs[!duplicated(cleaned_refs)]

refs[,New_ItemID_Ref_Target:=NULL]
refs[,New_Titre :=NULL]

refs_final <- merge(refs, cleaned_refs, by = "ItemID_Ref_Target", all.x = TRUE, all.y = FALSE)
refs_final[New_ItemID_Ref_Target!="", ItemID_Ref_Target:=New_ItemID_Ref_Target]
refs_final[New_Titre!="", Titre:=New_Titre]
refs_final[New_Nom!="", Nom:=New_Nom]
# We now have 81 083 and 55 404 unique
refs <- copy(refs_final)




# refs[,n_aut_year:=NULL]
refs_Claveau[ItemID_Ref_Target=="46322181"][,.N]
refs_Claveau[ItemID_Ref_Target=="36302916"][,.N]
refs[ItemID_Ref_Target=="975989"][,.N]
################  Scopus merging %%%%%%%%%%%%
# Scopus normalization: there are 665 references in the scopus bd
refs_scopus <- readRDS("EER/Corpus_EER/scopus_references.RDS")
refs_scopus <- refs_scopus %>% rename(ID_Art = temp_id)
refs_scopus[,ID_Art:=paste0("S",ID_Art)]
refs_scopus[,temp_idref:=paste0("SR",temp_idref)]
refs_scopus <- refs_scopus %>% rename(Nom = author)
refs_scopus[,Nom:=toupper(Nom)]
refs_scopus <- refs_scopus %>% rename(Annee = Year)
refs_scopus <- refs_scopus %>% rename(Volume = volume)
refs_scopus <- refs_scopus %>% rename(Page = pages)
refs_scopus[, first_page:= str_replace(Page, "\\-.*","")]

# WoS normalization
# id_ref <- fread("EER/Corpus_EER/EER_refs_identifiers2.csv", quote="") %>% data.table
id_ref <- copy(refs)
# id_ref <- refs #29943
id_ref[, names_scopuslike:= str_replace(Nom, "\\-.*","")]


# # Match on author.year.volume
# id_ref_match <- id_ref[names_scopuslike!="NULL" & Annee!="NULL" & Volume!="NULL" & Page!="NULL"]
# id_ref_match <- id_ref_match[,matching_col:=paste0(names_scopuslike,Annee,Volume,Page)]
# refs_scopus_match <- refs_scopus[Nom!="<NA>" & Annee!="<NA>" & Volume!="<NA>" & first_page!="<NA>"]
# refs_scopus_match <- refs_scopus_match[,matching_col:=paste0(Nom,Annee,Volume,first_page)] 
# # Match and get the temp_idref/ItemID_Ref relationship
# scopus_ItemID_Ref <- merge(refs_scopus_match[,.(matching_col,temp_idref)], id_ref_match[,.(matching_col,ItemID_Ref_Target)], by = "matching_col")
# scopus_ItemID_Ref <- scopus_ItemID_Ref[,head(.SD, 1),matching_col]




# #### Give uniques IDs to the sames references that are not in WoS %%%
# refs_scopus <- merge(refs_scopus[,.(ID_Art, temp_idref, Nom, Annee, journal_scopus=journal, Titre_scopus=title)], scopus_ItemID_Ref[,.(temp_idref,ItemID_Ref_Target)], by = "temp_idref", all.x = TRUE)
# refs_to_give_unique_Ids <- refs_scopus[,find_scopus_ids:=.N,.(Nom,Annee)][order(find_scopus_ids)]
# write.csv(refs_to_give_unique_Ids[order(Nom,Annee)], "EER/Corpus_EER/refs_to_give_unique_Ids.csv")

# Manually give them Ids
# match_list_manual <- id_ref[names_scopuslike %in% refs_to_give_unique_Ids$Nom & Annee %in% refs_to_give_unique_Ids$Annee][order(Nom, Annee)]
# write.csv(match_list_manual, "EER/Corpus_EER/manual_check.csv")
refs_scopus <- refs_scopus %>% rename(journal_scopus = journal)
refs_scopus <- refs_scopus %>% rename(Titre_scopus = title)

refs_to_give_unique_Ids <- fread("EER/Corpus_EER/refs_to_give_unique_Ids_cleaned.csv", quote="") %>% data.table
refs_to_give_unique_Ids <- refs_to_give_unique_Ids %>% rename(manual_ids = manual_id)

refs_to_give_unique_Ids <- refs_to_give_unique_Ids[manual_ids!=""]

#### Scopus and bind %%%
refs_scopus <- merge(refs_scopus, refs_to_give_unique_Ids[,.(temp_idref,manual_ids)], by="temp_idref", all.x = TRUE)
refs_scopus[, ItemID_Ref_Target:=manual_ids]
refs_scopus[is.na(ItemID_Ref_Target)==TRUE, ItemID_Ref_Target:=temp_idref]
write.csv(refs_scopus, "EER/Corpus_EER/explore_refs_scopus_last_check.csv")

#### FINAL SCOPUS MANUAL CLEANING %%%
refs_scopus <- fread("EER/Corpus_EER/explore_refs_scopus_last_check_cleaned.csv") %>% data.table
refs_scopus[is.na(manual_ids)==FALSE, ItemID_Ref_Target:=manual_ids]
refs_scopus <- refs_scopus %>% rename(journal_scopus = journal)
refs_scopus <- refs_scopus %>% rename(Titre_scopus = title)
refs_scopus <- merge(refs_scopus, cleaned_refs, by = "ItemID_Ref_Target", all.x = TRUE, all.y = FALSE)
refs_scopus[New_ItemID_Ref_Target!="", ItemID_Ref_Target:=New_ItemID_Ref_Target]
refs_scopus[,New_ItemID_Ref_Target:=NULL]
refs_scopus[,New_Titre :=NULL]
refs_scopus[,New_Nom :=NULL]


refs <- rbind(refs, refs_scopus[,.(ID_Art_Source=ID_Art,ItemID_Ref_Target, Nom, Annee, journal_scopus,Titre_scopus)], fill=TRUE)
refs[ItemID_Ref_Target=="SR10",Titre_scopus:="L'Equilibre economique en 1961"]
refs[is.na(Titre) & is.na(Titre_scopus)==FALSE, Titre:=toupper(Titre_scopus)]
refs[, c("Titre_scopus"):=NULL]
