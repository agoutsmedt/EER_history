source("Script_paths_and_basic_objects_EER.R")
source(here("functions", "functions_dynamics_networks_alex.R")) 
source(here("functions", "functions_networks_alex.R"))

set.seed(333)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### 1 Setting up the Corpus ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
Corpus <- readRDS(here(eer_data,
                       "1_Corpus_Prepped_and_Merged",
                       "Corpus_top5_EER.RDS"))
Corpus <- Corpus[jel_id==1]
Corpus <- Corpus[Annee_Bibliographique>=1973]
       
Refs <- readRDS(here(eer_data, 
                     "1_Corpus_Prepped_and_Merged",
                     "Direct_citations_top5_EER.RDS"))
Refs[,ItemID_Ref:=id_clean]

# Corpus, smoothing and  keep jel macro
Corpus[,ID_Art:=as.character(ID_Art)]
Corpus[,ItemID_Ref:=as.character(ItemID_Ref)]

# Refs, smoothing and Remove missings refs
Refs[,ID_Art:=as.character(ID_Art)]
Refs[,ItemID_Ref:=as.character(ItemID_Ref)]
Refs <- Refs[ItemID_Ref!=0]
Refs <- Refs[ID_Art %in% Corpus$ID_Art]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### 2 Parameters of networks creation####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Time window
time_window <- 8
first_year <- Corpus[order(Annee_Bibliographique), head(.SD, 1)]$Annee_Bibliographique
last_year <- (as.numeric(Corpus[order(-Annee_Bibliographique), head(.SD, 1)]$Annee_Bibliographique) - time_window + 1) # +1 to get the very last year in the window
last_year <- 1990
all_years <- first_year:last_year
all_years <- c(1975,1980,1985,1990) # do FA2 on some years rather than all

# Treshold
weight_treshold_value <- 2

# Filter for important communities
min_n_years <- 2
min_leiden_max <- 0.04

# Filter for grey
min_n_years_grey <- 1
min_leiden_max_grey <- 0.04

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### 3 Coupling####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

tbl_coup_list <- dynamics_coupling_networks(corpus = Corpus, 
                                            references = Refs, 
                                            source = "ID_Art", 
                                            target = "ItemID_Ref", 
                                            time_variable = Annee_Bibliographique,
                                            time_window = time_window, 
                                            weight_treshold_value = weight_treshold_value)

# We identify articles from the top 5 and from the EER, and do the same for to/from in edges
tbl_coup_list <- lapply(tbl_coup_list, function(tbl){tbl %>% activate(nodes) %>% mutate(Revue_EER_bin = ifelse(Revue=="EUROPEAN ECONOMIC REVIEW","EER","Top5"))})
tbl_coup_list <- lapply(tbl_coup_list, function(tbl){tbl %>% activate(edges) %>% mutate(EER_bin_to = .N()$Revue_EER_bin[to], EER_bin_from = .N()$Revue_EER_bin[from])})
tbl_coup_list <- lapply(tbl_coup_list, function(tbl){tbl %>% activate(edges) %>% mutate(EU_US_collab_to = .N()$EU_US_collab[to], EU_US_collab_from = .N()$EU_US_collab[from])})


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### 4 Community Detection, Main component, intertemporal naming  ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Main components >=5
tbl_coup_list <- lapply(tbl_coup_list, main_components)
tbl_coup_list <- lapply(tbl_coup_list, detect_leiden_igraph)

#' We name communities:
tbl_coup_list <- intertemporal_naming_function(tbl_coup_list, treshold_similarity = 0.55)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### 5 Alluvial ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#################### 5.1 Crossing and meta grouping ####################
alluv_dt <- make_into_alluv_dt(tbl_coup_list)
alluv_dt <- merge(alluv_dt, Corpus[,.N,.(EU_US_collab,ID_Art)], by.x = "Id", by.y = "ID_Art",all.x=TRUE)

alluv_dt <- minimize_crossing(alluv_dt)
alluv_dt <- meta_grouping(alluv_dt, treshold_meta_groups=0.45)
alluv_dt[,meta_group_size:=.N,.(new_Id_com,meta_group)]

#################### 5.2 Label ####################
label <- copy(alluv_dt)
label <- label[,.N,.(new_Id_com, share_leiden_max)][order(share_leiden_max)]

community_name <- tribble(
  ~new_Id_com, ~Label_com,
  "Mgz4J8yL", "International Macroeconomics & Target Zone",
  "CQpUqaZS", "Disequilibrium & Keynesian Macro",
  "Pnz6WX4w", "Modeling Consumption & Production",
  "piySoCVv", "Optimal Taxation \\(1\\)",
  "Ezhslxbw", "Political Economics of Central Banks",
  "JAqUI5vj", "Target Zone & Currency Crises",
  "SMwcTgPW", "Optimal Taxation \\(2\\)",
  "kthrIEeL", "Exchange Rate Dynamics",
  "5LSPOQtk", "Theory of Unemployment & Job Dynamics",
  "RL0j0Wjd", "Capital & Income Taxation",
  "BW7MeofH", "Taxation, Tobin's Q & Monetarism",
  "8ljfcYnr", "Coordination & Sunspots \\(2\\)",
  "mFfXMCSH", "Coordination & Sunspots \\(1\\)",
  "Th6dCLZm", "Monetary Policy, Financial Transmission & Cycles \\(2\\)",
  "vpvjT1UD", "Business Cycles, Cointegration & Trends",
  "rabMUXQL", "Terms of Trade & Devaluation",
  "OwqYJU4A", "Taxation, Debt & Growth",
  "5tcMbr61", "Endogenous Growth",
  "QLir0DCu", "Monetary Policy, Financial Transmission & Cycles \\(1\\)",
  "slbs4yZO", "RBC",
  "oWvS0ZKo", "Exchange Rate Dynamics & Expectations",
  "ZfbnKTMy", "Monetary Approach of Balance of Payments",
  "ghGgIdnw", "Demand for Money",
  "piHtlAjF", "Inflation, Interest Rates & Expectations",
  "VcbF4o2X", "REH, Monetary Policy & Business Cycles",
  "8rBp4n5V", "Credit Rationing, Rational Expectations & Imperfect Information",
  "wQE43lv5", "Inflation & Rigidities",
  "dEgJmubE", "Monetary Policy, Target & Output Gap",
  "ztGPr6dZ", "Permanent Income Hypothesis & Life-Cycle",
  "KkkrUrNU", "Monetary Economics & Demand for Money",
  "fMHDM0xi", "New Theory of Money: Search, Bargaining...",
  "tG4VUh3F", "Marginal Taxation",
  "NTSLryx5", "Intergenerational Model, Savings and Consumption"
)
label <- merge(label, community_name, by="new_Id_com",all.x=TRUE)
# label[,Label_com:=new_Id_com]


#################### 5.3 Coloring ####################
# Table of individual colors for each communities, ordered by order to maximize color diversity inside meta groups
unique_ids_color <- data.table(
  new_Id_com = as.character(alluv_dt[order(order),.N,new_Id_com]$new_Id_com), 
  individuals_colors = rep_len(viridis(10), nrow(alluv_dt[order(order),.N,new_Id_com])))

# Main palette for meta groups, we taka a few qualitative palets, and we order by meta_group size so the biggest groups have the most distinctive colors
color <- brewer.pal(7, name = "Dark2") %>% as.data.table()
color2 <- brewer.pal(7, name = "Set1") %>% as.data.table()
color3 <- brewer.pal(12, name = "Paired") %>% as.data.table()
color4 <- brewer.pal(12, name = "Set3") %>% as.data.table()
main_colors <- rbind(color,color2,color3,color4)
n_colors_metagroups <- alluv_dt[,.N,meta_group][,.N]

main_colors_table <- data.table(
  meta_group = as.character(alluv_dt[order(-meta_group_size),.N,meta_group]$meta_group), 
  main_colors = main_colors$.[1:n_colors_metagroups])

# Add meta groups and meta colors
unique_ids_color <- merge(unique_ids_color, unique(alluv_dt[,.(new_Id_com,meta_group)]), by ="new_Id_com", all.x = TRUE)
unique_ids_color <- merge(unique_ids_color, main_colors_table, by ="meta_group", all.x = TRUE)

# Mix colors
unique_ids_color[,color:=MixColor(individuals_colors, main_colors, amount1 = 0.35)]

# Add grey for smaller community
big_com <- as.character(alluv_dt[n_years>=min_n_years_grey & share_leiden_max>=min_leiden_max_grey,.N,new_Id_com]$new_Id_com) %>% as.data.table()
unique_ids_color[new_Id_com %in% big_com$., color_with_grey:= color]
unique_ids_color[is.na(color_with_grey),color_with_grey:="grey"]

#################### 5.4 Plotting ####################
# Add colors to main dt
alluv_dt_graph <- merge(alluv_dt, unique_ids_color[,.(new_Id_com, color_with_grey)], by="new_Id_com", all.x = TRUE)

# Add Names of communities and keep only one label per community as the average year for plotting
label_mean <- merge(alluv_dt, label[,.(new_Id_com, Label_com)], by="new_Id_com", all.x = TRUE)
label_mean <- copy(label_mean)
label_mean <- label_mean[,Window:=round(mean(as.numeric(Window))),new_Id_com][, head(.SD, 1), .(new_Id_com)]
alluv_dt_graph <- merge(alluv_dt_graph,label_mean[,.(new_Id_com,Window,Label_com)], by = c("new_Id_com","Window"), all.x = TRUE ) # Label_com_unique for plotting
alluv_dt_graph <- alluv_dt_graph %>% rename(Label_com_unique = Label_com)
alluv_dt_graph <- merge(alluv_dt_graph,label_mean[,.(new_Id_com, Label_com)], by = c("new_Id_com"), all.x = TRUE ) # Label_com for names

# Remove label if grey
alluv_dt_graph[color_with_grey=="grey",Label_com_unique:=NA]

# Reorder using the order variable
alluv_dt_graph$new_Id_com <- fct_reorder(alluv_dt_graph$new_Id_com, alluv_dt_graph$order,min, .desc = TRUE)

ggplot(alluv_dt_graph, aes(x = Window, y=share, stratum = new_Id_com, alluvium = Id, fill = color_with_grey, label = new_Id_com)) +
  geom_stratum(alpha =1, size=1/10) +
  geom_flow() +
  theme(legend.position = "none") +
  theme_minimal() +
  scale_fill_identity() +
  ggtitle("") +
  geom_label_repel(stat = "stratum", size = 5, aes(label = Label_com_unique))
ggsave(here(eer_data,"pictures","Graphs","Intertemporal_communities.png"), width=35, height=20, units = "cm")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### 6 tf-idf ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

tf_idf_table <- copy(alluv_dt)
tf_idf_table <- tf_idf_table[,share_leiden:=share_leiden_max]
tf_idf_table <- merge(tf_idf_table, label, by = "new_Id_com", all.x = TRUE) %>% as.data.table()
tf_idf_table[,Leiden1:=Label_com]
tf_idf_table[is.na(Label_com),Leiden1:=new_Id_com]
tf_idf_table <- tbl_graph(tf_idf_table)

tf_idf_color <- copy(unique_ids_color)
tf_idf_color <- merge(tf_idf_color, label, by = "new_Id_com", all.x = TRUE) %>% as.data.table()
tf_idf_color[,Leiden1:=Label_com]
tf_idf_color[is.na(Label_com),Leiden1:=new_Id_com]

tf_idf_results <- tf_idf(tf_idf_table, tf_idf_color, 15, 4, treshold_com = 0.05, size_title_wrap=10, unstemming = TRUE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### 7 EU/US differences ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

nb_cit <- Refs[, .N, ItemID_Ref]
colnames(nb_cit)[colnames(nb_cit) == "N"] <- "size"
nb_cit_all <- merge(Corpus, nb_cit, by.x = "ItemID_Ref", by.y = "ItemID_Ref", all.x = TRUE)
nb_cit_all[is.na(size),size:=0]

alluv_dt_as_nodes <- copy(alluv_dt_graph)
alluv_dt_as_nodes[,nodes_n_time_com := .N, .(new_Id_com,Id)]
alluv_dt_as_nodes <- merge(alluv_dt_as_nodes, nb_cit_all[,.(size, Id, Annee_Bibliographique, Label, Revue)], by="Id", all.x = TRUE)

most_influential_nodes <- copy(alluv_dt_as_nodes)
most_influential_nodes[,weighted_size:=size*nodes_n_time_com]
most_influential_nodes <- most_influential_nodes[, head(.SD, 1), .(new_Id_com,Id)]
most_influential_nodes <- most_influential_nodes[order(-weighted_size)]

com_to_inspect <- alluv_dt_as_nodes[share_leiden_max>=min_leiden_max & n_years>=min_n_years, .N, new_Id_com][order(-N)]$new_Id_com
com_to_inspect <- gsub("([()])","\\\\\\1", com_to_inspect)

euro_com_stat_list <- list()

for (com in com_to_inspect) {
  alluv_com  <- alluv_dt_as_nodes[new_Id_com==paste0(com)]
  # window <- as.integer(c(min(unique(alluv_com$Window)), as.integer(max(unique(alluv_com$Window))) + (time_window - 1)))
  # window <- as.integer(c(min(unique(alluv_com$Window)), as.integer(max(unique(alluv_com$Window)))))
  
  share_europeans_authors <- alluv_dt_as_nodes[Window>=min(alluv_com$Window) & Window<=max(alluv_com$Window)] %>%
    group_by(EU_US_collab) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) %>% as.data.table() %>% 
    .[EU_US_collab=="Europe Only"] %>% 
    .[,freq]
  
  share_europeans_authors_cluster <- alluv_com %>%
    group_by(EU_US_collab) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) %>% as.data.table() %>% 
    .[EU_US_collab=="Europe Only"] %>% 
    .[,freq]
  
  share_US_authors <- alluv_dt_as_nodes[Window>=min(alluv_com$Window) & Window<=max(alluv_com$Window)] %>%
    group_by(EU_US_collab) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) %>% as.data.table() %>% 
    .[EU_US_collab=="USA Only"] %>% 
    .[,freq]
  
  share_US_authors_cluster <- alluv_com %>%
    group_by(EU_US_collab) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) %>% as.data.table() %>% 
    .[EU_US_collab=="USA Only"] %>% 
    .[,freq]
  
  share_EER_articles <- alluv_dt_as_nodes[Window>=min(alluv_com$Window) & Window<=max(alluv_com$Window)] %>%
    group_by(Revue) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) %>% as.data.table() %>% 
    .[Revue =="EUROPEAN ECONOMIC REVIEW"] %>% 
    .[,freq]
  
  share_EER_articles_cluster <- alluv_com %>%
    group_by(Revue) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) %>% as.data.table() %>% 
    .[Revue=="EUROPEAN ECONOMIC REVIEW"] %>% 
    .[,freq]
  
  
  europeans_authors_diff <- share_europeans_authors_cluster/share_US_authors_cluster-share_europeans_authors/share_US_authors
  EER_articles_diff <- share_EER_articles_cluster-share_EER_articles
  EER_articles_diff <- (share_EER_articles_cluster/(1-share_EER_articles_cluster))-(share_EER_articles/(1-share_EER_articles))
  
  
  euro_com_stat <- data.table(
    new_Id_com = com, 
    share_europeans_authors_cluster = share_europeans_authors_cluster,
    share_europeans_authors = share_europeans_authors,
    share_US_authors_cluster = share_US_authors_cluster,
    share_US_authors = share_US_authors,
    share_EER_articles = share_EER_articles,
    share_EER_articles_cluster=share_EER_articles_cluster,
    europeans_authors_diff = europeans_authors_diff,
    EER_articles_diff = EER_articles_diff,
    sum_diff=europeans_authors_diff+EER_articles_diff
  )
  
  euro_com_stat_list[[as.character(com)]] <- euro_com_stat
}

diff_stat_euro_com <- rbindlist(euro_com_stat_list)
diff_stat_euro_com[,com_name:=new_Id_com]
# diff_stat_euro_com[new_Id_com %notin% community_name$community_name,com_name:=NA]
# saveRDS(diff_stat_euro_com,"EER/2_Raw_Networks_and_Alluv/euro_vs_us_communities.rds")

com_to_inspect <- diff_stat_euro_com[order(-sum_diff), .N, new_Id_com]$new_Id_com
com_to_inspect <- gsub("([()])","\\\\\\1", com_to_inspect)


new_Id_com_size <- alluv_dt_as_nodes[,.N,new_Id_com][order(-N)] # size of dots
new_Id_com_size <-  new_Id_com_size %>% rename(new_Id_com_size = N)

diff_stat_euro_com <- merge(diff_stat_euro_com, new_Id_com_size, by="new_Id_com", all.x = TRUE) # size dot
diff_stat_euro_com <- merge(diff_stat_euro_com, community_name, by="new_Id_com",all.x=TRUE)#label name

# diff_stat_euro_com[new_Id_com %notin% community_name$community_name,new_Id_com:=NA]

ggplot(diff_stat_euro_com, aes(x = europeans_authors_diff, y = EER_articles_diff)) +
  geom_vline(xintercept = 0, size = 1, alpha = 0.4) +
  geom_hline(yintercept = 0, size = 1, alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray", alpha = 0.5) +
  ggrepel::geom_text_repel(aes(label = as.character(Label_com)), data = diff_stat_euro_com, min.segment.length = 0, alpha = 0.7) +
  geom_point(aes(size = new_Id_com_size), alpha = 0.8) +
  scale_color_identity() +
  scale_fill_identity() +
  labs(title = "Over/Under representation of the EER and Europeans in Communities",
       x = "US Only (left) vs. European Only (right)",
       y = "Top 5 (down) vs. EER (up)") +
  theme_minimal() 
ggsave(here(eer_data,"pictures","Graphs","Communities_europeanisation_bw.png"), width=35, height=20, units = "cm")

ggplot(diff_stat_euro_com, aes(x = europeans_authors_diff, y = EER_articles_diff)) +
  geom_vline(xintercept = 0, size = 1, alpha = 0.3) +
  geom_hline(yintercept = 0, size = 1, alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_point(aes(color = sum_diff, size = new_Id_com_size), alpha = 0.8) +
  ggrepel::geom_text_repel(data = . %>% filter(sum_diff > 0.01 | sum_diff < -0.012), 
                           aes(label = as.character(Label_com)), size = 2.5, alpha = 1, hjust = 0) +
  scico::scale_color_scico(palette = "roma", breaks = c(-0.35, 0.65), labels = c("Less European", "More European")) +
  labs(title = "",
       color = "",
       x = "US Only (left) vs. European Only (right)",
       y = "Top 5 (down) vs. EER (up)") + 
  theme_classic(base_size = 9) +
  theme(legend.position = "bottom") +
  guides(size = "none") 
ggsave(here(eer_data,"pictures","Graphs","Communities_europeanisation_colored.png"), width=35, height=20, units = "cm")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### 8 Saving ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
saveRDS(tf_idf_results, here(eer_data,"2_Raw_Networks_and_Alluv", "tf_idf.RDS"))
saveRDS(alluv_dt, here(eer_data,"2_Raw_Networks_and_Alluv", "Alluv_dt.RDS"))
saveRDS(alluv_dt_graph, here(eer_data,"2_Raw_Networks_and_Alluv", "Alluv_dt_graph.RDs"))
saveRDS(tbl_coup_list, here(eer_data,"2_Raw_Networks_and_Alluv", "Tbl_coup_list.RDS"))

