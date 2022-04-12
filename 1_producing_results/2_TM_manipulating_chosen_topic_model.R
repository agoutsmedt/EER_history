#' ---
#' title: "Script for manipulating the chosen topic model"
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
#' Now we have chosen the preprocessing steps and the number of topics, we can explore 
#' our topic model by looking at the most important words, the distribution over time, and
#' compute our indicator in function of where it is published and by who.
#' 
#' > WARNING: This script still needs a lot of cleaning
#' 
#' 
#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

################# Loading packages, paths and data ################### ------
#' # Loading packages, paths and data
#' 
#' 
source(here::here("Script_paths_and_basic_objects_EER.R"))
source(here("functions", 
            "functions_for_topic_modelling.R"))

Corpus <- readRDS(here(eer_data, 
                       "1_Corpus_Prepped_and_Merged",
                       "Corpus_top5_EER.rds"))
chosen_topic_model <- read_rds(here(eer_data, 
                                    "3_Topic_modelling", 
                                    "chosen_topic_models.rds"))

#' # Working with the chosen topic model: basic description
#' 
#' We will now integrate the metadata (the year of publication, and also the authors
#' affiliation, in case we want to observe the content of a topic)


id <- chosen_topic_model$preprocessing_id %>% unique
nb_topics <- chosen_topic_model$K %>% unique 

#' Now we can add the covariates. It is not changing the topic
#' model. The topics are the same, just the order of the words can
#' change. Perhaps that is not changing it at all and the small changes
#' are just linked to the random part of the stm function. 
#' 
#' We extract the data of our prepared data, just to keep the ID_Art we need for the
#' metadata 

stm_data <- chosen_topic_model$stm[[1]]
metadata <- data.table("ID_Art" = names(stm_data$documents))

metadata <- metadata %>% 
  left_join(Corpus) %>% 
  mutate(EU_US_collab = ifelse(is.na(EU_US_collab), "Neither", EU_US_collab)) # we need to avoid NA in the variable

stm_data$meta$Year <- as.integer(metadata$Annee_Bibliographique)
stm_data$meta$Origin <- metadata$EU_US_collab # we need this to diffenriate the content of topics

#' We can now fit again the topic model for the same number of topics,
#' but adding the covariates for topic prevalence and topic content.

topic_model <- stm(stm_data$documents, 
                   stm_data$vocab, 
                   prevalence = ~s(Year),
                   content = ~Origin,
                   data = stm_data$meta,
                   K = nb_topics,
                   init.type = "Spectral",
                   seed = 1989)

#' We can use the stm package function to plot some descriptive visualisations.
#' For certain visualisations, we can extract the data to use ggplot/ggraph.
#' 
#' We can also extract the top terms for different measure (not just for beta).
#' This data.frame can also be used to give name to the topics. We will use it
#' for the nodes of the topic correlation network.

top_terms <- extract_top_terms(topic_model,
                               chosen_topic_model$data[[1]],
                               nb_terms = 20,
                               frexweight = 0.3)

#' We will use this table for exploration
#' `saveRDS(top_terms, here(eer_data, "3_Topic_modelling", "TM_top_terms.rds"))`

#' This function will take the four words with the highest frex value to name the 
#' topic.
topics <- name_topics(top_terms, method = "frex", nb_word = 4)

#' ## Correlation network 
#' 
#' We now plot the topic correlation network. The use is double: to see which topics are 
#' the closest, and to use this closeness to build a new order of topics. What we want
#' is to plot summary statistics of topics with more correlated topics being next
#' to each other
set.seed(1989)
topic_corr_network <- ggraph_topic_correlation(topic_model, 
                                               nodes = topics,
                                               method = "huge", 
                                               size_label = 2.5,
                                               nb_topics = nb_topics,
                                               resolution = 1.4) 



#' We observe the results and gives name to the identified communities:
communities <- topic_corr_network$graph %>% 
  activate(nodes) %>% 
  as.data.table %>% 
  select(topic, Com_ID)
topics <- merge(topics, communities, by = "topic") %>% 
  arrange(Com_ID)

#' We can look at the composition of the different community: `View(topics)`

community_name <- tribble(
  ~Com_ID, ~Com_name,
  "02", "Public Finance & Agents Behavior",
  "03", "International Macroeconomics & Growth",
  "04", "Inflation, Expectations & information",
  "05", "Theory, Microfoundations & Econometrics",
  "06", "Monetary & Fiscal Policies",
  "07", "Business Cycles & Production",
  "08", "Schools of Thought Debates",
  "09", "Labor"
)

#' #### Plotting network with communities

community_name$com_color <- c(scico(n = 8, palette = "hawaii"))
topics_with_com <- merge(topics, community_name, by = "Com_ID")
network <- topic_corr_network$graph 
network <- network %>% 
  activate(nodes) %>% 
  left_join(unique(topics_with_com[, c("Com_ID", "Com_name", "com_color")]))
network <- network %>% # mix color
  activate(edges) %>%
  mutate(color_com_ID_to = .N()$com_color[to], color_com_ID_from = .N()$com_color[from]) %>%
  mutate(color_edges = DescTools::MixColor(color_com_ID_to, color_com_ID_from, amount1 = 0.5))

graph_plot <- ggraph(network, layout = "manual", x = x, y = y) +
  geom_edge_arc0(aes(color = color_edges, width = weight), strength = 0.3, alpha = 0.6, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.5,12)) +
  scale_edge_colour_identity() +
  scale_fill_identity() +
  geom_node_label(aes(label = topic_name, fill = com_color), size = 2.5, alpha = 0.7) +
  dark_theme_bw()

ragg::agg_png(here(tm_picture_path, "TM_topic_correlation.png"), 
              width = 40, 
              height = 30, 
              units = "cm", 
              res = 300)
graph_plot
invisible(dev.off())

#' ## Describing topics top terms and frequency
#' 
#' We use the order resulting of correlation to distribute colors for each topic. More generally,
#' we have the choice between giving a unique color per topic (depending of the correlation network),
#' or giving to topics the color of their community (identified in the correlation network with 
#' Leiden algorithm).

topics_with_com <- topics_with_com %>% 
  arrange(Com_ID, id) %>% 
  mutate(new_id = 1:n(),
         color = c(scico(n = (nb_topics)/2 - 1, begin = 0, end = 0.35, palette = "roma"),
                   scico(n = (nb_topics)/2 + 1, begin = 0.55, palette = "roma")))

#' We plot the terms with the highest FREX value for each topic:

top_terms_graph <- top_terms %>%
  filter(measure == "frex") %>% 
  inner_join(topics_with_com[, c("id", "color", "Com_ID", "new_id", "com_color")], by = c("topic" = "id")) %>% 
  mutate(topic = paste0("topic ", topic),
         term = reorder_within(term, value, topic)) %>%
  ggplot(aes(value, term)) +
  scale_fill_identity() +
  geom_col(aes(fill = com_color), show.legend = FALSE) +
  facet_wrap(~ fct_reorder(topic, new_id), scales = "free") +
  scale_y_reordered() +
  coord_cartesian(xlim=c(0.96,1))

ragg::agg_png(here(tm_picture_path, "TM_top_terms.png"),
              width = 50, height = 40, units = "cm", res = 300)
top_terms_graph
invisible(dev.off())

#' We now plot the frequency of each topics:
#' 

plotting_frequency <- plot_frequency(topics_with_com, topic_model, com_color) +
  dark_theme_bw()

ragg::agg_png(here(tm_picture_path, "TM_topic_prevalence.png"), 
              width = 40, 
              height = 40, 
              units = "cm", 
              res = 300)
plotting_frequency
invisible(dev.off())

#' We add the frequency value of each topic:
#' 

topics_complete <- topics_with_com %>% 
  arrange(id) %>% 
  mutate(topic_prevalence = colMeans(topic_model$theta))
saveRDS(topics_complete, here(eer_data,
                             "3_Topic_modelling",
                             "topics_complete.RDS"))


#' We will use this table for exploration
#' `saveRDS(topics_complete, here(eer_data, "3_Topic_modelling", "TM_topics_summary.rds"))`
#' 
#' We also want to save some examples of the topics, by keeping the more representative abstracts.
#' Some topics are most represented just by title

topic_examples <- topics_complete %>% 
  select(id) %>% 
  mutate(examples = map(id, ~findThoughts(topic_model, 
                                          texts = paste0(metadata$Titre, ": ", metadata$Abstract), 
                                                              n = 4, 
                                                              topics = .x)$docs[[1]])) %>% 
  unnest(examples)

saveRDS(topic_examples, here(eer_data,
                             "3_Topic_modelling",
                             "TM_Topic_examples.rds"))

#' # Topics according to our variables of interest
#' 
#' ## Extracting the data from the topic model
#' 
#' We extract the gamma table: the table that associates each document to each topic with
#' a certain gamma value (a kind of rate of belonging).

topic_gamma <- tidy(topic_model, matrix = "gamma") 
topic_gamma <- topic_gamma %>% 
  left_join(select(topics_complete, id, topic_name), by = c("topic" = "id")) %>% 
  select(-topic) %>% 
  mutate(topic_name = str_remove_all(str_replace(topic_name, "\\\n", " "), " \\/"))


topic_gamma_widered <- pivot_wider(topic_gamma,
                           names_from = topic_name, 
                           values_from = gamma) %>% 
  mutate(ID_Art = names(stm_data$documents)) %>% 
  inner_join(Corpus) %>% 
  select(document, ID_Art, Nom, Annee_Bibliographique, Titre, Revue, journal_type, contains("Topic")) %>% 
  as.data.table()

topic_gamma_attributes <- merge(topic_gamma_widered,
                                unique(Corpus[, c("ID_Art", "EU_US_collab")]),
                                by = "ID_Art", all.x = TRUE) %>% 
  mutate(Nom = str_remove_all(as.character(Nom), "c\\(\"|\\)|\""))

topic_gamma_attributes <- pivot_longer(topic_gamma_attributes, 
                                       cols = contains("Topic"),
                                       names_to = "topic_name",
                                       values_to = "gamma") %>% 
  as.data.table() %>% 
  mutate(topic = str_extract(topic_name, "Topic \\d+")) %>% 
  left_join(select(topics_complete, topic, new_id)) # we add the id depending of the correlation and the prevalence

#' We will use this table for exploration
#' `saveRDS(topic_gamma_attributes, here(eer_data, "3_Topic_modelling", "TM_gamma_values.rds"))`
#' `topic_gamma_attributes <- readRDS(here(eer_data, "3_Topic_modelling", "TM_gamma_values.rds"))`

#' ## Differences in terms of journals and affiliations
#' 
#' We will use covariates below to do that, but to have results easier to interpret, we also
#' observe the journal and affiliation effect on topics just by looking at the difference 
#' in the mean of each variable for each topic.
#' 

topic_diff <- copy(topic_gamma_attributes)

topic_diff[, mean_affiliation := mean(gamma), by = c("topic_name", "EU_US_collab")] %>% 
  .[, mean_journal := mean(gamma), by = c("topic_name", "journal_type")] 
topic_diff_summary <- topic_diff %>% 
  filter(EU_US_collab %in% c("Europe Only", "USA Only")) %>% 
  select(topic_name, EU_US_collab, mean_affiliation, journal_type, mean_journal) %>% 
  unique() 

topic_diff_summary <- pivot_wider(topic_diff_summary, 
                                  names_from = "EU_US_collab", 
                                  values_from = "mean_affiliation") %>% 
  pivot_wider(names_from = "journal_type", 
              values_from = "mean_journal") %>% 
  mutate(diff_affiliation = `Europe Only` - `USA Only`,
         diff_journal = EER - TOP5,
         total_diff = diff_affiliation + diff_journal,
         id = as.integer(str_extract(topic_name, "[:digit:]{1,2}")),
         topic_label = str_wrap(str_remove(topic_name, "Topic [:digit:]{1,2} "), 15)) %>% 
  left_join(select(topics_complete, id, new_id, Com_name, com_color, topic_prevalence)) %>% 
  select(id, new_id, topic_label, topic_name, Com_name, com_color, diff_affiliation, diff_journal, total_diff, topic_prevalence) %>% 
  as.data.table()

#' We can now plot the differences in a two-dimensions diagram:
#' 

mean_diff_plot <- ggplot(topic_diff_summary, aes(x = diff_affiliation, y = diff_journal)) +
  geom_vline(xintercept = 0, size = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, size = 1.5, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_label_repel(aes(label = topic_label, 
                       group = factor(Com_name),
                       color = com_color, 
                       fill = com_color), size = 2, alpha = 0.7, hjust = 0) +
  geom_point(aes(group = factor(Com_name),
                 color = com_color, 
                 fill = com_color,
                 size = topic_prevalence), alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.2, 50)) %>% 
  scale_color_identity() +
  scale_fill_identity() +
  labs(title = "Topic Prevalence over journals (Difference of Means method)",
       x = "US Only (left) vs. European Only (right)",
       y = "Top 5 (down) vs. EER (up)") +
  dark_theme_classic()

ragg::agg_png(here(tm_picture_path, "mean_diff_plot.png"), 
              width = 50, 
              height = 40, 
              units = "cm", 
              res = 400)
mean_diff_plot
invisible(dev.off())

#' We test an alternative method for evaluation the relative share of journals and affiliations
#' by mimicking the approach used in the bibliometric coupling analysis. We take only the 
#' articles with a gamma value above 0.1, and we then calculate the share of articles for
#' each variables

topic_diff_logratio_journal <- copy(topic_gamma_attributes) %>% 
  filter(gamma > 0.1) %>% 
  group_by(topic) %>% 
  add_count(journal_type, name = "percent_journal") %>% 
  select(topic_name, topic, journal_type, percent_journal) %>% 
  unique() %>% 
  ungroup() %>% 
  pivot_wider(names_from = journal_type, values_from = percent_journal, values_fill = 0) %>% 
  mutate_if(is.integer, list(~(. + 1) / (sum(.) +1))) %>% 
  mutate(logratio_journal = log(EER/TOP5)) %>% 
  select(topic_name, topic, logratio_journal)

topic_diff_logratio_affiliation <- copy(topic_gamma_attributes) %>% 
  filter(gamma > 0.1) %>% 
  group_by(topic) %>% 
  add_count(EU_US_collab, name = "percent_affiliation") %>% 
  select(topic, EU_US_collab, percent_affiliation) %>% 
  unique() %>% 
  ungroup() %>% 
  pivot_wider(names_from = EU_US_collab, values_from = percent_affiliation, values_fill = 0) %>% 
  mutate_if(is.integer, list(~(. + 1) / (sum(.) +1))) %>% 
  mutate(logratio_affiliation = log(`Europe Only`/`USA Only`)) %>% 
  select(topic, logratio_affiliation)

topic_diff_logratio <- topic_diff_logratio_journal %>% 
  left_join(topic_diff_logratio_affiliation) %>% 
  mutate(id = as.integer(str_extract(topic_name, "[:digit:]{1,2}")),
         topic_label = str_wrap(str_remove(topic_name, "Topic [:digit:]{1,2} "), 15)) %>% 
  left_join(select(topics_complete, id, new_id, Com_name, com_color, topic_prevalence)) %>% 
  select(id, new_id, topic_label, topic_name, Com_name, com_color, logratio_affiliation, logratio_journal, topic_prevalence) %>% 
  as.data.table()
  
topic_diff_summary <- topic_diff_summary %>% 
  left_join(select(topic_diff_logratio, id, logratio_affiliation, logratio_journal))

#' We will use this table for exploration
#' `saveRDS(topic_diff_summary, here(eer_data, "3_Topic_modelling", "TM_topics_diff.rds"))`  
#' We can now plot in two dimensions:

logratio_diff_plot <- ggplot(topic_diff_logratio, aes(x = logratio_affiliation, y = logratio_journal)) +
  geom_vline(xintercept = 0, size = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, size = 1.5, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_label_repel(aes(label = topic_label, 
                       group = factor(Com_name),
                       color = com_color, 
                       fill = com_color), size = 2, alpha = 0.7, hjust = 0) +
  geom_point(aes(group = factor(Com_name),
                 color = com_color, 
                 fill = com_color,
                 size = topic_prevalence), alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.2, 50)) %>% 
  scale_color_identity() +
  scale_fill_identity() +
  labs(title = "Topic Prevalence over journals (Difference of Logratio method)",
       x = "US Only (left) vs. European Only (right)",
       y = "Top 5 (down) vs. EER (up)") +
  dark_theme_classic()

ragg::agg_png(here(tm_picture_path, "logratio_diff_plot.png"), 
              width = 50, 
              height = 40, 
              units = "cm", 
              res = 400)
logratio_diff_plot
invisible(dev.off())
  

#' ## Using covariates for year distribution and topic content per affiliation
#' 
#' ### Looking at year distribution
#' 
#' We fit regressions for our year covariate.
prep <- estimateEffect(~s(Year),
                       topic_model,
                       metadata = stm_data$meta,
                       nsims = 200)

tidyprep_year <- tidystm::extract.estimateEffect(prep, 
                                                 "Year", 
                                                 topic_model, 
                                                 method = "continuous") %>% 
  left_join(select(topics_complete, id, topic_name, color, new_id), by = c("topic" = "id"))

slope <- tidyprep_year %>% 
  filter(covariate.value == max(tidyprep_year$covariate.value) |
           covariate.value == min(tidyprep_year$covariate.value)) %>% 
  select(topic_name, covariate.value, estimate) %>% 
  pivot_wider(values_from = estimate, names_from = covariate.value) %>% 
  mutate(slope = `2002` - `1973`) %>% 
  select(topic_name, slope) %>% 
  arrange(slope)

tidyprep_year <- tidyprep_year %>% 
  arrange(new_id) %>% 
  left_join(slope)

#' We plot the impact for each topics:
topic_per_year <- ggplot(tidyprep_year, aes(x = covariate.value, y = estimate,
                                            ymin = ci.lower, ymax = ci.upper,
                                            group = factor(topic),
                                            fill = color)) +
  scale_fill_identity() +
  facet_wrap(~ fct_reorder(str_wrap(topic_name, 25), new_id), nrow = 6) +
  geom_ribbon(alpha = .5, show.legend = FALSE) +
  geom_line() +
  theme(strip.text = element_text(size = 3)) +
  dark_theme_bw()

ragg::agg_png(here(tm_picture_path, "topic_per_year.png"), 
              width = 50, 
              height = 40, 
              units = "cm", 
              res = 300)
topic_per_year
invisible(dev.off())

#' ### Topic content depending of affiliation
#' 

plot(topic_model, 
     type = "perspectives", 
     topics = 6,
     covarlevels = c("USA Only", "Europe Only"),
     n = 60,
     text.cex = 3.8)


plot(topic_model, 
     type = "perspectives", 
     topics = 30,
     covarlevels = c("USA Only", "Europe Only"),
     n = 60,
     text.cex = 4)


