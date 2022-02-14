# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## LOADING PACKAGES, PATHS ####################################--------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

##################### Packages ############################################--------------

package_list <- c("fs",
                  "here",
                  "RMySQL",
                  "bib2df", # for importing Microsoft data
                  "data.table", 
                  "tidyverse", ## Tidy tools
                  "tidygraph",
                  "furrr", 
                  "janitor", # cleaning data
                  "ggnewscale", ## Visualisation tool
                  "ggraph", 
                  "ggforce", 
                  "directlabels", 
                  "patchwork", 
                  # "grid", # Needed?
                  "ggdendro",
                  "scales", 
                  "ggrepel",
                  "RColorBrewer",
                  "scico",
                  "ggdark",
                  # "igraph", # No need?
                  "quanteda", ## Text analysis tools
                  "tm", 
                  "tidytext", 
                  "stm", 
                  "topicmodels",
                  "spacyr", 
                  "stringi",
                  "huge", # Use with stm for spatialisation of networks
                  # "readtext", # What for?
                  "leidenAlg", # Clusterisation but integrated in igraph now? 
                  "biblionetwork",
                  "reshape2",
                  # "DescTools", # what for?
                  "DT", ## Hmtl interactive tools
                  "plotly", 
                  "crosstalk", 
                  "widgetframe", 
                  "sigmajs",
                  "htmlwidgets", 
                  "pander", # For publication?
                  "glue"
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
    data_path <- "/projects/data/macro_AA"
  }
}

# First level path
macro_AA_data <- here(data_path,
                      "macro_AA")
eer_data <- here(data_path, 
                 "EER")


boards_path <- here(eer_data, "editorial_boards")

# Pictures
picture_path <- here(eer_data, 
                     "pictures")
tm_picture_path <- here(picture_path, "topic_modelling")
