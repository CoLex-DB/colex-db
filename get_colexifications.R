library(pacman)
p_load(tidyverse, sqldf, DBI, 
       igraph, network, ggnetwork, 
       htmlwidgets, networkD3, tnet, ggnewscale)

clics_db <- dbConnect(SQLite(), dbname = "clics.sqlite")

forms <- dbGetQuery(clics_db, "select Language_ID, Parameter_ID, Value from FormTable where dataset_ID = 'northeuralex'")
# langs <- dbReadTable(clics_db, "LanguageTable")
# cognates <- dbGetQuery(clics_db, "select * from CognateTable where dataset_ID = 'northeuralex'")
langs <- dbGetQuery(clics_db, "select ID, Name from LanguageTable where dataset_ID = 'northeuralex'")
params <- dbReadTable(clics_db, "ParameterTable")
wordnet <- read_tsv("wordnet.tsv")
wn_mappings <- read_tsv("wn-data-eng.tab")

etymologies <- read_tsv("etymwn-20130208/etymwn.tsv", 
                        col_names = c("Target", "Relation", "Source")) %>% 
  filter(str_detect(Relation, "^rel:etymology$")) %>% 
  # head(100) %>% 
  select(-Relation) %>% 
  separate(Target, c("Language_ID", "Value"), sep = ": ") %>% 
  separate(Source, c("Source_ID", "Source_Value"), sep = ": ") %>% 
  filter(str_detect(Value, "^-|-$", negate = T) &
           str_detect(Source_Value, "^-|-$", negate = T)) %>% 
  mutate(Source_Value = str_remove_all(Source_Value, "''|\\[\\[|\\]\\]") %>% 
           str_split(", ")) %>% 
  unnest(Source_Value)

wn_concepticon <- params %>% 
  select(Concepticon_Gloss, Concepticon_ID) %>% 
  distinct() %>% 
  full_join(forms %>% 
              select(ID = Parameter_ID) %>% 
              distinct() %>% 
              left_join(params %>% 
                          select(ID, Concepticon_Gloss))) %>% 
  left_join(wordnet %>% 
              transmute(Concepticon_ID = as.character(CONCEPTICON_ID), 
                        WordNet_ID = OPEN_WORDNET_ID)) %>% 
  left_join(wn_mappings) %>% 
  select(-WordNet_ID) %>% 
  arrange(is.na(ID), !is.na(Synset)) %>% 
  select(NorthEuraLex_ID = ID, everything())

write_tsv(wn_concepticon, "wn_concepticon.tsv")

synchronic_dictionary <- forms %>% 
  left_join(wn_concepticon %>% 
              select(Parameter_ID = NorthEuraLex_ID, Synset)) %>% 
  distinct() %>% 
  group_by(Language_ID, Value) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  arrange(Language_ID, Value) %>% 
  select(Language_ID, Value, everything())

relevant_etymologies <- etymologies %>% 
  right_join(synchronic_dictionary %>% 
               select(Language_ID, Value) %>% 
               distinct()) %>% 
  filter(!(Language_ID == Source_ID & Value == Source_Value) | is.na(Source_ID)) %>% 
  arrange(!is.na(Source_ID))

write_tsv(relevant_etymologies, "northeuralex_etymologies.tsv")

full_dictionary <- synchronic_dictionary %>% 
  full_join(relevant_etymologies %>% 
              select(Language_ID = Source_ID,
                     Value = Source_Value) %>% 
              distinct() %>% 
              filter(!is.na(Language_ID))) %>% 
  arrange(!is.na(Parameter_ID), Language_ID, Value) %>% 
  select(Language_ID, Value, everything())

write_tsv(full_dictionary, "northeuralex_definitions.tsv")

colexifications <- full_dictionary %>% 
  filter(!is.na(Synset)) %>% 
  group_by(Language_ID, Value) %>% 
  summarise(Polysemy = list(unique(c(Synset)))) %>% 
  ungroup() %>% 
  filter(map_dbl(Polysemy, length) > 1) %>% 
  mutate(Polysemy = map(Polysemy, function(x){
    combn(x, 2) %>% 
      apply(2, paste, collapse = " ")
  })) %>% 
  unnest(Polysemy)

colex_etymologies <- colexifications %>% 
  left_join(colexifications %>% 
              inner_join(relevant_etymologies %>% 
                           select(Language_ID = Source_ID,
                                  Value = Source_Value) %>% 
                           distinct()) %>% 
              rename(Source_ID = Language_ID,
                     Source_Value = Value) %>% 
              inner_join(colexifications %>% 
                           left_join(relevant_etymologies))) %>% 
  arrange(is.na(Source_ID))

colex_etymology_graph <- colex_etymologies %>% 
  filter(!is.na(Source_ID)) %>% 
  transmute(Source = paste(Source_ID, Source_Value, Polysemy, sep = ": "),
            Target = paste(Language_ID, Value, Polysemy, sep = ": ")) %>% 
  graph_from_data_frame()

colex_full_etymologies <- colex_etymology_graph %>% 
  distances(.,
            V(.)[degree(., mode = "in") == 0],
            V(.)[degree(., mode = "out") == 0]) %>% 
  as.data.frame() %>% 
  rownames_to_column("Ultimate_Source") %>% 
  pivot_longer(-Ultimate_Source, "Target") %>% 
  filter(!is.infinite(value)) %>% 
  select(-value) %>% 
  separate(Target, c("Language_ID", "Value", "Polysemy"), sep = ": ") %>% 
  separate(Ultimate_Source, c("Ultimate_Source_ID", 
                              "Ultimate_Source_Value"), sep = ": ", 
           extra = "drop") %>% 
  right_join(colex_etymologies) %>% 
  mutate(Ultimate_Source_ID = if_else(is.na(Source_ID),
                                      Language_ID,
                                      Ultimate_Source_ID),
         Ultimate_Source_Value = if_else(is.na(Source_Value),
                                         Value,
                                         Ultimate_Source_Value))

distinct_colexifications <- colex_full_etymologies %>% 
  select(Ultimate_Source_ID, Ultimate_Source_Value, Polysemy) %>% 
  distinct() %>%
  count(Polysemy, name = "weight", sort = T)

colexifications_summary <- distinct_colexifications %>% 
  filter(weight > 1) %>% 
  separate(Polysemy, c("Sense_1", "Sense_2"), " ")

colexifications_graph <- colexifications_summary %>%
  graph_from_data_frame(directed = F)

# plot(colexifications_graph)

betweenness <- betweenness_w(colexifications_summary %>% 
                mutate(Sense_1 = match(Sense_1, V(colexifications_graph)$name),
                       Sense_2 = match(Sense_2, V(colexifications_graph)$name))) %>% 
  as_tibble() %>% 
  mutate(node = V(colexifications_graph)$name[node])

colexifications_graph <- set_vertex_attr(colexifications_graph,
                                         "centrality", 
                                         value = betweenness$betweenness) %>% 
  set_vertex_attr("pos", value = str_match(V(.)$name, ".*\\.([anrsv])\\.\\d+$")[,2])

ggplot(colexifications_graph, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(size = weight), lineend = "round") +
  scale_size_continuous(range = c(0.5, 5)) +
  new_scale("size") +
  geom_nodelabel(aes(label = name, size = centrality, fill = pos), 
                 label.padding = unit(0.15, "lines"), alpha = 0.8, col = "white") +
  scale_size_continuous(range = c(1, 4)) +
  coord_fixed() +
  theme_blank()

ggsave("northeuralex_colex.pdf", width = 20, height = 20)

interactive_network <- forceNetwork(colexifications_summary %>%
                                      mutate(Sense_1 = match(Sense_1, V(colexifications_graph)$name) - 1,
                                             Sense_2 = match(Sense_2, V(colexifications_graph)$name) - 1),
             Nodes = tibble(name = V(colexifications_graph)$name,
                            group = V(colexifications_graph)$pos),
             Source = "Sense_1",
             Target = "Sense_2",
             Value = "weight",
             NodeID = "name",
             Group = "group",
             fontSize = 20,
             opacity = 1,
             zoom = T,
             opacityNoHover = 1)

saveWidget(interactive_network, file = "northeuralex_colex.html")

colexifications_matrix <- colexifications %>% 
  select(Language_ID, Polysemy) %>% 
  distinct() %>% 
  mutate(Value = 1) %>% 
  spread(Polysemy, Value) %>% 
  modify_if(is.numeric, replace_na, 0) %>% 
  column_to_rownames("Language_ID") %>% 
  as.matrix()

dbDisconnect(clics_db)
