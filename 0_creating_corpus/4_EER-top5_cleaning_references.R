################  Claveau BD %%%%%%%%%%%%

if(str_detect(here(), "home")){
  all_ref <-  dbGetQuery(ESH, paste0("SELECT ID_Art, ItemID_Ref, New_id2, Annee, Nom, Revue_Abbrege, Volume, Page 
                                   FROM OST_Expanded_SciHum.References7 as ref 
                                   WHERE ID_Art IN (SELECT ID_Art FROM OST_Expanded_SciHum.Articles WHERE Code_Revue=5200);")) %>%
    data.table
  all_art <-  dbGetQuery(ESH, paste0("SELECT * 
                                   FROM OST_Expanded_SciHum.Articles 
                                   WHERE ItemID_Ref IN (SELECT ItemID_Ref FROM OST_Expanded_SciHum.References7 WHERE ID_Art IN (SELECT ID_Art FROM OST_Expanded_SciHum.Articles WHERE Code_Revue=5200));")) %>%
    data.table
} else {
  id_art <- Corpus %>% 
    filter(! str_detect(ID_Art, "S")) %>% 
    select(ID_Art)
  all_ref <- arrow::read_parquet(here(macro_AA_data,
                                      "OST_generic_data",
                                      "all_ref.parquet"),
                                 as_data_frame = FALSE) %>% 
    filter(ID_Art %in% id_art$ID_Art) %>% 
    collect() %>% 
    data.table
  all_art <- arrow::read_parquet(here(macro_AA_data,
                                      "OST_generic_data",
                                      "all_art.parquet"),
                                 as_data_frame = FALSE) %>% 
    filter(ItemID_Ref %in% all_ref$ItemID_Ref & ItemID_Ref != 0) %>% 
    collect() %>% 
    data.table
}

# Infos about refs (REF_RAW=83 174 with 59 286 unique id2 et 34 062 unique ItemID_Ref)
refs_Claveau <- merge(all_ref, all_art[ItemID_Ref!=0], by="ItemID_Ref", all.x = TRUE) 

# Infos about revues
refs_Claveau <- merge(refs_Claveau, revues[,Code_Revue:=as.integer(Code_Revue)][,.(Code_Revue,Code_Discipline)], by="Code_Revue", all.x = TRUE)

# Noramlize Claveau's BD
refs_Claveau <- refs_Claveau[,.(ID_Art_Source=ID_Art.x, ItemID_Ref_old_claveau=ItemID_Ref, New_id2, Nom, Annee, Code_Revue, Code_Discipline, Titre, Revue_Abbrege, Volume, Page)]
refs_Claveau <- refs_Claveau[ID_Art_Source %in% Corpus$ID_Art]
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

################ Completing Refs Informations %%%%%%%%%%%%
refs[,ID_Art_Source:=as.character(ID_Art_Source)]
refs[,Id:=as.character(ID_Art_Source)]
refs[,Annee_Bibliographique_Target:=Annee]
refs[,Nom_Target:=Nom]
refs[,Code_Revue:=as.character(Code_Revue)]
refs[,Code_Discipline:=as.character(Code_Discipline)]

# Label column
refs <- refs[, name_short:=  gsub("-.*","",Nom)]
refs$name_short <- toupper(refs$name_short)
refs <- refs[,Label_Target:=paste0(name_short,",",Annee_Bibliographique_Target)]
refs[, c("name_short"):=NULL]

# Disciplines and journals
refs <- merge(refs, revues[,.(Code_Revue=as.character(Code_Revue), Revue)], by="Code_Revue", all.x = TRUE)
refs[,Revue := sub("\r","", Revue)]
refs <- merge(refs, disciplines, by="Code_Discipline", all.x = TRUE)
refs[is.na(Revue) & is.na(journal_scopus)==FALSE, Revue:=toupper(journal_scopus)]
refs[, c("journal_scopus"):=NULL]

# Info about Sources
refs[,nb_cit_tot:=.N,ItemID_Ref_Target]

# Info about Sources
refs <- merge(refs, Corpus[,.(ID_Art, Annee_Bibliographique)], by.x = "ID_Art_Source", by.y = "ID_Art", all.x = TRUE)
setnames(refs, "Annee_Bibliographique", "Annee_Bibliographique_Source")
refs[,ID_Art:=ID_Art_Source]
refs[,ItemID_Ref:=ItemID_Ref_Target]