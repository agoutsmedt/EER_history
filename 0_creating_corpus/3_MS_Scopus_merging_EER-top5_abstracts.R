
source("Script_paths_and_basic_objects_EER.R")
source("~/macro_AA/logins_DO_NOT_UPLOAD.R") 

if(str_detect(here(), "home")){
ESH <- dbConnect(MySQL(),
                 user = usr, password = pswd, dbname = "OST_Expanded_SciHum",
                 host = "127.0.0.1"
)
}

`%notin%` <- Negate(`%in%`)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Making the corpus ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
path_microsoft <- here(eer_data,
                       "Corpus_Top5",
                       "MS_Academics_AB")
MS_files <- list.files(path_microsoft)[str_which(list.files(path_microsoft), "^MS")]
top_5_AB <- map(MS_files, ~ bib2df(here(path_microsoft, .x))) %>% 
  bind_rows() %>% 
  mutate(PAGES_START = str_extract(PAGES, "^\\d+"),
         PAGES_END = str_extract(PAGES, "\\d+$")) %>% 
  as.data.table()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Exploration####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# histogram
ggplot(top_5_AB, aes(x=as.character(YEAR))) + geom_bar() +  facet_wrap(~JOURNAL, ncol = 2, scales = "free") 

# % of missing AB
top_5_AB %>% as.data.table() %>% .[,n_journals:=.N,JOURNAL] %>%  .[is.na(ABSTRACT)] %>% .[,.N,.(JOURNAL,n_journals)] %>% .[,N/n_journals,JOURNAL]

# histogram of missing abstracts
ggplot(top_5_AB[is.na(ABSTRACT)], aes(x=as.character(YEAR))) + geom_bar() +  facet_wrap(~JOURNAL, ncol = 2, scales = "free") 
top_5_AB

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Matching with WoS ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# all art
if(str_detect(here(), "home")){
  all_art <- dbGetQuery(ESH, paste0("SELECT * FROM OST_Expanded_SciHum.Articles WHERE ItemID_Ref != 0;")) %>% 
    data.table()
  revues <- dbGetQuery(ESH, "SELECT Code_Revue, Revue, Code_Discipline FROM OST_Expanded_SciHum.Revues;") %>% 
    data.table()
} else {
  all_art <- arrow::read_parquet(here(macro_AA_data, 
                                      "OST_generic_data",
                                      "all_art.parquet"),
                                 as_data_frame = FALSE) %>% 
    filter(ItemID_Ref != 0) %>% 
    collect() %>% 
    data.table()
  
  revues <- readRDS(here(macro_AA_data, 
                         "OST_generic_data",
                         "revues.rds")) %>% 
    data.table()
}

issueID <- readRDS(here(macro_AA_data, 
                      "OST_generic_data",
                      "revueID.rds")) %>% 
                   data.table()

# Get IssueID for matching
all_art <- merge(all_art, issueID[, .(IssueID, Volume)], by = "IssueID")


######################### Getting all_art running smoothly ***************************
all_art <- all_art[, .(ID_Art, Annee_Bibliographique, Title = Titre, Code_Revue, Page_Debut, Page_Fin, Nb_Page, ItemID_Ref, Volume)]
all_art <- merge(all_art, revues, by = "Code_Revue")
all_art[, Revue := sub("\r", "", Revue)]
all_art <- all_art[, Pages := paste0(all_art$Page_Debut, "-", all_art$Page_Fin)] # formatting pages number
all_art <- all_art[, ID_by_pub := paste(all_art$Annee_Bibliographique, all_art$Revue, all_art$Pages, all_art$Volume)] # One variable for matching

######################### Do the same for dt of JEL ***************************
top_5_AB[, JOURNAL := toupper(JOURNAL)]
top_5_AB[JOURNAL=="THE AMERICAN ECONOMIC REVIEW", JOURNAL:="AMERICAN ECONOMIC REVIEW"]
top_5_AB[JOURNAL=="THE REVIEW OF ECONOMIC STUDIES", JOURNAL:="REVIEW OF ECONOMIC STUDIES"]
top_5_AB <- top_5_AB[, Pages := paste0(top_5_AB$PAGES_START, "-", top_5_AB$PAGES_END)] # formatting pages number
dt_MS_art <- top_5_AB[, ID_by_pub := paste(top_5_AB$YEAR, top_5_AB$JOURNAL, top_5_AB$Pages, top_5_AB$VOLUME)]
dt_MS_art <- dt_MS_art[PAGES_START!=""]
dt_MS_art <- dt_MS_art[PAGES_END!=""]
dt_MS_art <- dt_MS_art[VOLUME!=""]

#cleaninr double
dt_MS_art[,art_in_double:=.N,ID_by_pub] # some articles (~30) are in double because they have multiple pdf sources
dt_MS_art[art_in_double>1][order(ID_by_pub, NOTE)]
dt_MS_art[art_in_double>1][NOTE %like% "*Query*"]$BIBTEXKEY
dt_MS_art <- dt_MS_art[BIBTEXKEY %notin% dt_MS_art[art_in_double>1][NOTE %like% "*Query*"]$BIBTEXKEY] # remove artilces with "query dates" in note
dt_MS_art[, NOTE := sub(" cites:", "", NOTE)]
dt_MS_art[, NOTE := as.numeric(NOTE)]
dt_MS_art <- dt_MS_art[order(ID_by_pub, -NOTE)][,head(.SD,1),ID_by_pub] #keep only articles that are the most cited when same IDs


######################### Merging everything ***************************
# JEL with bd to find article by their volume, pages, year, and journal
dt_MS_art_ID_ART <- merge(dt_MS_art, all_art[,.(ID_Art, ID_by_pub)], by="ID_by_pub", all.x = TRUE)
saveRDS(dt_MS_art_ID_ART, 
        here(eer_data,
             "1_Corpus_Prepped_and_Merged",
             "abstracts_MS_with_ID_Art.RDS"))
saveRDS(top_5_AB, 
        here(eer_data,
             "1_Corpus_Prepped_and_Merged",
             "abstracts_MS_RAW.RDS"))

missing_ID_Art <- readRDS("EER/1_Corpus_Prepped_and_Merged/abstracts_MS_with_ID_Art.RDS")
ggplot(missing_ID_Art[is.na(ID_Art),.N,YEAR], aes(x=as.character(YEAR),y=N)) + geom_bar(stat="identity")
ggplot(missing_ID_Art[is.na(ID_Art)], aes(x=as.character(YEAR))) + geom_bar() +  facet_wrap(~JOURNAL, ncol = 2, scales = "fixed")



#' We now need to add the abstracts identified with scopus. The difficulty
#' is to match the two corpora together. The strategy is to match on author-date
#' when there is a unique author-date per year, then to match on title, then
#' to match what is lefting for multiple author-date for a year, and then to finish
#' manually.
#' 

begin_words <- c("^A ","^THE ","^AN ", "^ON THE ")
Corpus <- Corpus[order(Annee_Bibliographique,Nom_ISI,Titre)][Annee_Bibliographique < 2019]
Corpus <- Corpus %>% 
  mutate(author_date = paste0(Nom_ISI,"-",Annee_Bibliographique),
         Titre = toupper(Titre),
         Titre_alt = str_squish(str_replace_all(Titre, "[:punct:]", " ")),
         Titre_alt = str_remove(Titre_alt, paste0(begin_words, collapse = "|"))) %>% 
  group_by(author_date) %>% 
  mutate(count_authordate = n()) %>% 
  as.data.table()

#' Loading and preparing abstracts data
scopus_abstract <- readRDS(paste0(eer_data,"scopus_abstract.RDS"))
scopus_abstract[Nom_ISI == "THELL-H"]$Nom_ISI <- "THEIL-H"
scopus_abstract[Nom_ISI == "RECONCILIATION-A"]$Nom_ISI <- "BRADA-J"
scopus_abstract[Nom_ISI == "STEIGUMJR-E"]$Nom_ISI <- "STEIGUM-E"
scopus_abstract[Nom_ISI == "ROSEN-NA"]$Nom_ISI <- "ROSEN-A"

scopus_abstract <- scopus_abstract[order(Annee_Bibliographique,Nom_ISI,Titre)][Annee_Bibliographique <= max(Corpus$Annee_Bibliographique) &
                                                                                 !is.na(abstract)]
scopus_abstract <- scopus_abstract %>% 
  mutate(author_date = paste0(Nom_ISI,"-",Annee_Bibliographique),
         Titre_alt = str_squish(str_replace_all(Titre, "[:punct:]", " ")),
         Titre_alt = str_remove(Titre_alt, paste0(begin_words, collapse = "|"))) %>% 
  group_by(author_date) %>% 
  mutate(count_authordate = n()) %>% 
  as.data.table()

merge_corpus_authordate <- merge(scopus_abstract[count_authordate == 1, c("temp_id","abstract","author_date")], 
                                 Corpus[count_authordate  == 1], 
                                 by = "author_date")

merge_corpus_title <- merge(scopus_abstract[,c("temp_id","abstract","Titre_alt")], 
                            Corpus, 
                            by = "Titre_alt")

#' There is a "normal" doublon in `merge_corpus_title` that we can remove (an article
#' is an answer to another article, with the same title)
#' 
merge_corpus_title <- merge_corpus_title[-which(duplicated(merge_corpus_title$abstract))]

#' We now work on the cases with two author-dates. 
#' 

#merge_corpus_alt <- merge(scopus_abstract[count_authordate > 1, c("temp_id","abstract","Titre_alt")], 
#                          Corpus[count_authordate  > 1], 
#                          by = "Titre_alt") %>% 
#  unique()

#merge_corpus_alt <- merge_corpus_alt[-which(duplicated(merge_corpus_alt$abstract))]

#' We bind the two merges and remove doublons

merge_corpus <- rbind(merge_corpus_authordate, merge_corpus_title) %>% 
  unique()


#' We can now finish manually by comparing the lefting content of the two data-frames:
#' - `view(scopus_abstract[order(Annee_Bibliographique, Nom_ISI)][! temp_id %in% merge_corpus$temp_id])`
#' - `view(Corpus[! ID_Art %in% merge_corpus$ID_Art, -c("Code_Revue","Code_Document","Id","JEL_id")])`
#' We build a dataframe with the missing abstracts that we can bind with what we already have, and 
#' then merge to the Corpus.

link_id <- data.table(ID_Art = c("41014562","42281165","42035495","7143275",
                                 "7917371","11258778","10716665","12147841",
                                 "12668535","14139264","19354563","61558363",
                                 "61558365","61558360"),
                      temp_id = as.integer(c("650","552","554","2784",
                                             "2612","2199","2288","2153",
                                             "1983","1859","1398","3603",
                                             "3602","3614")))

manual_merge <- merge(link_id, scopus_abstract, by = "temp_id")
merge_corpus <- rbind(merge_corpus[,c("ID_Art","abstract")],manual_merge[,c("ID_Art","abstract")])
Corpus <- merge(Corpus, merge_corpus, by = "ID_Art", all.x = TRUE)
Corpus[, Label := paste0(str_remove(Nom_ISI, "-.*"), ",",Annee_Bibliographique)]
                                                        