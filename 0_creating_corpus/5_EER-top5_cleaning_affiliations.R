#' ---
#' title: "Script for cleaning the affiliations data"
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
#' This script aims at cleaning scopus data for affiliations. It mainly aims at
#' avoiding doublons with different names.
#' 
#' In the last part of the script, we categorize institutions (Academic, International Organization,
#' Private, _etc._)
#'
#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

#' # Loading packages, paths and data
#'

source("Script_paths_and_basic_objects_EER.R")

Institutions <- readRDS(here(eer_data,
                             "0_To_Be_Cleaned",
                             "Institutions_EER_Top5_To_Clean.rds")) %>% 
  rename(Institution_to_clean = Institution)

#' # Cleaning Scopus institutions data
#' 
#' Our goal is to make them similar to WoS institutions name. It involves a lot of manual cleaning
#' 

Institutions_scopus <- Institutions %>% 
  filter(str_detect(ID_Art, "^S")) %>% 
  distinct(Institution_to_clean, .keep_all = TRUE) %>% 
  mutate(clean_name = str_replace_all(Institution_to_clean, " ","-"),
         clean_name = str_replace_all(clean_name, "UNIVERSITY|UNIVERSITÉ|UNIVERSITE(I)?T","UNIV"),
         clean_name = str_replace_all(clean_name, "-OF-|-DE-|-VAN-","-"),
         clean_name = str_remove(clean_name, "^THE-"))

Institutions_scopus_clean <- tribble(
  ~ to_correct, ~ correction,
  "TEL-AVIV", "TEL-AVIV-UNIV",
  "CENTER-PLANNING-AND-ECONOMIC-RESEARCH", "CTR-PLANNING-&-ECON-RES", 
  "CENTRAL-PLANNING-BUREAU", "CENT-PLANNING-BUR",
  "BOWLING-GREEN-", "BOWLING-GREEN-STATE-UNIV",
  "ECONOMIC-AND-SOCIAL-RESEARCH-INSTITUTE", "ECON-&-SOCIAL-RES-INST",
  "HUNGARIAN-ACADEMY-SCIENCES-BUDAPEST", "HUNGARIAN-ACAD-SCI",
  "INDUSTRIAL-INSTITUTE-ECONOMIC-AND-SOCIAL-RESEARCH", "IND-INST-ECON-&-SOCIAL-RES",
  "RESOURCES-FOR-THE-FUTURE,-INC", "RESOURCES-FUTURE-INC",
  "INSTITUTE-ECONOMIC-STUDIES", "INST-INT-ECON-STUDIES",
  "ASPIRANT-F.N.R.S.", "FNRS",
  "SCHOOL-ECONOMICS", "ERASMUS-UNIV",
  "CALIFORNIA$", "UNIV-CALIF-BERKELEY",
  "DAVIS$", "UNIV-CALIF-DAVIS",
  "WORLD-BANK", "WORLD-BANK",
  "LOUVAIN", "UNIV-LOUVAIN",
  "NAPOLI", "UNIV-NAPLES",
  "BOLOGNA", "UNIV-BOLOGNA",
  "^EUROPEAN-INSTITUTE","EUROPEAN-INST-ADV-STUDY-MANAGEMENT",
  "^FREE-UNIV", "FREE-UNIV-BRUSSELS",
  "INTERNATIONAL-MON", "INT-MONETARY-FUND",
  "STOCKHOLMS", "STOCKHOLM-UNIV",
  "GENÈVE", "UNIV-GENEVA"
)

for(i in 1:nrow(Institutions_clean)){
  Institutions_scopus <- Institutions_scopus %>% 
    mutate(clean_name = ifelse(str_detect(clean_name, Institutions_scopus_clean$to_correct[i]),
                               Institutions_scopus_clean$correction[i],
                               clean_name)) 
}

for(i in 1:nrow(Institutions_scopus)){
  Institutions <- Institutions %>% 
    mutate(Institution_to_clean = ifelse(str_detect(Institution_to_clean, Institutions_scopus$Institution_to_clean[i]),
                                         Institutions_scopus$clean_name[i],
                                         Institution_to_clean))
}

#' # Cleaning Institutions
#' 
#' Here, we are trying to uniformise the names of institutions. We try to reduce the possibility
#' of different writings for the same institutions and we also merge different departments/laboratories
#' when in the same university (Europe institutions have more different names than US ones)

Institutions_clean <- tribble(
  ~ to_correct, ~ correction,
  "LOUVAIN|^CORE$|CTR-OPERAT", "UNIV-LOUVAIN",
  "INST-INT-ECON-STUDIES", "UNIV-STOCKHOLM",
  "-ROTTERDAM$", "UNIV-ROTTERDAM",
  "LEUVEN$", "UNIV-LEUVEN",
  "LIBRE-BRUXELLES$|^ECARE", "FREE-UNIV-BRUSSELS",
  "UNIV-AMSTERDAM", "UNIV-AMSTERDAM",
  "^NATL-BUR", "NBER",
  "^CTR-ECON-POLICY", "CEPR",
  "LONDON-SCH-ECON", "LSE",
  "^INST-NATL-STAT", "INSEE",
  "ECOLE-NATL-ST", "ENSAE",
  "EHESS|^DELTA$|^ECOLE-HAUTES-ETUD|CTR-ECON-QUANTITAT|MAISON-SCI-HOMME", "EHESS",
  "ECOLE-NORM", "ENS",
  "CTR-ETUD-PROSPECT-ECON-", "CEPREMAP",
  "^UNIV-OXFORD", "UNIV-OXFORD",
  "UNIV-CAMBRIDGE|CAMBRIDGE-UNIV", "UNIV-CAMBRIDGE",
  "BIRKBECK", "UNIV-LONDON-BIRKBECK",
  "^INT-MONETARY", "IMF",
  "NETHERLANDS-BUR", "CENT-PLANNING-BUR",
  "BOCCONI", "UNIV-BOCCONI",
  "ATHENS(-GRAD)?-SCH-ECON", "ATHENS-SCH-ECON-BUSINESS-SCI"
)

cb_clean <- tribble(
  ~ to_correct, ~ correction,
  "BANQUE-FRANCE|BANK-FRANCE", "BANQUE-FRANCE",
  "BANCO-ESPANA|BANK-SPAIN", "BANCO-ESPANA", 
  "BANCA-ITALIA|BANK-ITALY", "BANCA-ITALIA", 
  "BANCO-PORTUGAL|BANK-PORTUGAL", "BANCO-PORTUGAL",
  "NEDERLANDSCHE-BANK", "NEDERLANDSCHE-BANK",
  "FED-RESERVE-SYST|FED-RESERVE-BOARD", "FED-RESERVE-BOARD",
  "SWISS-NATL-BANK|SUFTUNG-SCHWEIZERISCHEN-NATL-BANK", "SWISS-NATL-BANK",
  "-BUNDESBANK|BUDESBANK", "DEUTSCHE-BUNDESBANK",
  "BANK-BELGIUM", "BANK-BELGIUM",
  "STERIGES-RIKSBANK", "SVERIGES-RIKSBANK",
  "BANK-CHILE", "CENT-BANK-CHILE",
  "EUROPEAN-CENT-BANK", "EUROPEAN-CENT-BANK",
  "BANK-NORWAY", "NORGES-BANK",
  "OESTERREICH-NAT", "OESTERREICH-NATIONALBANK",
  "DANMARKS-NATL-BANK", "DANMARKS-NATIONALBANK"
)

all_cleaning <- bind_rows(Institutions_clean, cb_clean)
Institutions <- Institutions %>% 
  mutate(Institution = Institution_to_clean) # the column where we will put the final name

for(i in 1:nrow(all_cleaning)){
  Institutions <- Institutions %>% 
    mutate(Institution = ifelse(str_detect(Institution, all_cleaning$to_correct[i]),
                                 all_cleaning$correction[i],
                               Institution)) 
}

#' The only conflict is with the two university of York, and we distinguish the one in Canada.
Institutions <- Institutions %>% 
  mutate(Institution = ifelse(Institution == "YORK-UNIV" & Pays == "CANADA", "YORK-UNIV-TORONTO", Institution))

#' We have spotted that sometimes "UNIV" is at the beginning or at the end. The strategy is:
#' 
#' - listing all the university with the pattern "UNIV-NAME" or "NAME-UNIV"
#' - extracting the NAME and rewriting as "UNIV-NAME"
#' - checking when the original name is different from the one we have rebuilt.
#' - checking for potential conflicts (like YORK-UNIV for Canada and UNIV-YORK for England, already
#' problematic in the database)
#' - changing names in the data table.
correct_univ <- data.table("Institution" = filter(Institutions, 
                                                  str_detect(Institution, "^UNIV-[A-z]{1,}$|^[A-z]{1,}-UNIV$"))$Institution %>% 
                             unique) %>% 
  mutate(correct_name = paste0("UNIV-",str_remove(Institution, "UNIV-|-UNIV"))) %>% 
  filter(Institution != correct_name) %>% 
  unique

for(i in 1:nrow(correct_univ)){
  Institutions <- Institutions %>% 
    mutate(Institution = ifelse(str_detect(Institution, correct_univ$Institution[i]),
                                correct_univ$correct_name[i],
                                Institution)) 
}

#' In case we want to save the cleaned names for other projects:
#' `Institutions %>% select(Institution_to_clean, Institution) %>% unique %>% writeRDS()`

#' # Cleaning countries
#' 

country_clean <- tribble(
  ~ to_correct, ~ correction,
  "FED-REP-GER", "GERMANY",
  "WEST-GERMANY", "GERMANY",
  "UNITED STATES", "USA",
  "ENGLAND", "UNITED KINGDOM",
  "SCOTLAND", "UNITED KINGDOM",
  "NORTH-IRELAND", "UNITED KINGDOM",
  "WALES", "UNITED KINGDOM",
  "CZECHOSLOVAKIA|CZECH-REPUBLIC", "CZECH REPUBLIC",
  "YOGUSLAVIA", "YUGOSLAVIA",
  "AUSTL\\.", "AUSTRALIA"
)

for(i in 1:nrow(country_clean)){
  Institutions <- Institutions %>% 
    mutate(Pays = ifelse(str_detect(Pays, country_clean$to_correct[i]),
                         country_clean$correction[i],
                         Pays))
}

#' # Creating the collabs variable
#' 
#' We import a `.csv` with the list of European countries (Europe as a continent). We will
#' first identify if an affiliation is from USA, Europe or Other countries.
Europe <- read_csv(here(eer_data, 
                 "Europe_continent.csv")) %>% 
  bind_rows(data.frame(Countries = c("UNITED KINGDOM", "CYPRUS"))) %>% 
  mutate(Countries = toupper(Countries),
         Countries_grouped = "EUROPE") %>% 
  rename(Pays = Countries)

Institutions <- Institutions %>% 
  left_join(Europe) %>% 
  mutate(Countries_grouped = ifelse(Pays == "USA", Pays, Countries_grouped),
         Countries_grouped = ifelse(is.na(Countries_grouped), "OTHER", Countries_grouped))

saveRDS(Institutions, here(eer_data,
                        "1_Corpus_Prepped_and_Merged",
                        "Institutions_top5_EER.rds"))

#' Just checking if everything is ok:
#' `Institutions %>% select(Pays, Countries_grouped) %>% unique`
#' We can now build the Collabs variable depending if an article has been written only by
#' US affiliated authors, Europe affiliated authors, if authors from the US and Europe have
#' collaborated (without other countries), of if authors are affiliated to other countries
#' (even if writing with European or US affiliated authors).

Collabs <- Institutions %>% 
  group_by(ID_Art) %>% 
  mutate(type = paste0(Countries_grouped, collapse = ";"), # we put all the grouped countries information in the same column
         USA = str_detect(type, "USA"),
         EU = str_detect(type, "EUROPE"),
         OTHER = str_detect(type, "OTHER"),
         EU_US_collab = ifelse(OTHER == TRUE, "Neither", NA), # If any other country, we are not in collaboration or in "only" cases
         EU_US_collab = ifelse(USA == TRUE & EU == FALSE & OTHER == FALSE, "USA Only", EU_US_collab), # Just USA affiliations
         EU_US_collab = ifelse(USA == FALSE & EU == TRUE & OTHER == FALSE, "Europe Only", EU_US_collab), # Just European affiliations
         EU_US_collab = ifelse(USA == TRUE & EU == TRUE & OTHER == FALSE, "Collaboration", EU_US_collab)) %>% 
  select(ID_Art, EU_US_collab) %>% 
  unique

#' We merge with the general corpus.
#' 

readRDS(here(eer_data,
             "0_To_Be_Cleaned",
             "Corpus_EER_Top5_No_Abstract.rds")) %>% 
  left_join(Collabs) %>% 
  saveRDS(here(eer_data,
               "0_To_Be_Cleaned",
               "Corpus_EER_Top5_Collabs_No_Abstract.rds"))
