# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## LOADING PACKAGES, PATHS ####################################--------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

##################### Packages ############################################--------------

package_list <- c(
  "here", "fs", "biblionetwork",
  "data.table", "tidyverse", "furrr", "ggnewscale", "igraph",
  "quanteda", "tm", "tidytext", "ggraph", "tidygraph",
  "leidenAlg", "reshape2", "scales", "RMySQL", "stringi",
  "ggforce", "directlabels", "patchwork", "DescTools", "DT",
  "grid", "ggdendro", "readtext", "pander", "RColorBrewer",
  "scico", "plotly", "crosstalk", "widgetframe", "sigmajs",
  "ggdark", "topicmodels", "ggrepel", "stm", "huge",
  "spacyr", "htmlwidgets", "bib2df","arrow"
)
for (p in package_list) {
  if (p %in% installed.packages() == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

github_list <- c(
  "agoutsmedt/networkflow",
  "mikajoh/tidystm",
  "ParkerICI/vite"
)
for (p in github_list) {
  if (gsub(".*/", "", p) %in% installed.packages() == FALSE) {
    devtools::install_github(p)
  }
  library(gsub(".*/", "", p), character.only = TRUE)
}


######################### Paths ##########################################------------

if (str_detect(getwd(), "goutsmedt")) {
  data_path <- "C:/Users/goutsmedt/Mon Drive/data"
} else {
  if (str_detect(getwd(), "Dropbox")) {
    data_path <- "G:/.shortcut-targets-by-id/1EHqA0pp2uozTykWv0_Stf5xyrvo_dkN5/data"
  } else {
    data_path <- "/projects/data"
  }
}

macro_AA_data <- here(data_path,
                      "macro_AA")
eer_data <- here(data_path, 
                 "EER")

boards_path <- here(eer_data, "editorial_boards")
macro_data <- here(macro_AA_data, "Corpus_Econlit_Matched_WoS")

picture_path <- here("EER_Paper", 
                     "Pictures")