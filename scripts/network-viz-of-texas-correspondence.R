# see https://www.jessesadler.com/post/network-analysis-with-r/

# used Open Refine to do another few passes at reconciling and cleaning up names
# saved as 'new-cleaned-correspondence.csv'

library(tidyverse)
letters <- read_csv("new-cleaned-correspondence.csv")

letters

# we want to create a list of *all* nodes
# so we need to grab the names of all individuals,
# senders and recipients. We'll just pull those things out into
# two separate variables, then rejoin into a single list

sources <- letters %>%
  distinct(Sender) %>%
  rename(label = Sender)

destinations <- letters %>%
  distinct(Recipient) %>%
  rename(label = Recipient)

nodes <- full_join(sources, destinations, by = "label")
nodes

nodes <- nodes %>% rowid_to_column("id")
nodes


# create the edgelist and add edges up to make weight.

per_route <- letters %>%  
  group_by(Sender, Recipient) %>%
  summarise(weight = n()) %>% 
  ungroup()

# now we link the ids to these labels

edges <- per_route %>% 
  left_join(nodes, by = c("Sender" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("Recipient" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)
edges

library(tidygraph)
library(ggraph)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
routes_tidy

# graph the result
ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()

# because the graph tibble contains two different kinds of things
# we have to activate either nodes or edges to work with them
#filter weight, filter out any isolates. Sequence matters
filtered <- routes_tidy %>% 
  activate(nodes) %>% 
  mutate(degree = centrality_degree()) %>%
  filter(degree > 1) %>%
#  activate(edges) %>%
#  filter(weight > 1) %>%
  activate(nodes) %>%
  filter(!node_is_isolated()) 

# metrics get calculated on the graph
# and so have to be inside a pipeline
# use ?graph_measures, ?node_measures, ?centrality to find out more about them
# see also https://dgarcia-eu.github.io/SocialDataScience/5_SocialNetworkPhenomena/057_Tidygraph2/tidygraph2.html

pageranks <- as_tibble(routes_tidy %>% 
  activate(nodes) %>% 
  mutate(pgrnk = centrality_pagerank()))

# who has the best pagerank? what does that mean historically?
View(pageranks)

# which correspondence pair is most between?
bwness <- as_tibble(routes_tidy %>% 
                      activate(edges) %>% 
                      mutate(bw = centrality_edge_betweenness()))

View(bwness)

#etc.

# some graphing
ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()
ggraph(filtered) + geom_edge_link() + geom_node_point() + theme_graph()


#plot by betweeness
filtered %>% 
  mutate(centrality = centrality_betweenness()) %>%
  ggraph(layout = 'lgl') + 
  geom_edge_link() + 
  geom_node_point(aes(size = centrality, colour = centrality)) + 
  geom_node_text(aes(label = label)) +
  scale_color_continuous(guide = 'legend') + 
  theme_graph()

#plot by centrality
routes_tidy %>% 
  mutate(centrality = centrality_authority()) %>%
  ggraph(layout = 'graphopt') + 
  geom_edge_link() + 
  geom_node_point(aes(size = centrality, colour = centrality)) + 
  scale_color_continuous(guide = 'legend') + 
  theme_graph()

filtered %>% 
  mutate(centrality = centrality_authority()) %>%
  ggraph(layout = 'graphopt') + 
  geom_edge_link() + 
  geom_node_point(aes(size = centrality, colour = centrality)) + 
  scale_color_continuous(guide = 'legend') + 
  geom_node_text(aes(label = label)) +
  theme_graph()


# circular plot; if `circular = TRUE` is removed, you get an
# arcplot, where direction of arcs above the line are read left to right 
# and direction of arcs below the line are read right to left

ggraph(filtered, layout = "linear", circular = TRUE) + 
  theme(axis.text.x = element_text(angle = 90)) +
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Letters") +
  theme_graph()

# community detection 
# however, should be run on an undirected network;
# this can be set in the tbl_graph command eg 

undirectedroutes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
 
filtered_undirected <- undirectedroutes_tidy %>% 
  activate(nodes) %>% 
  mutate(degree = centrality_degree()) %>%
  filter(degree > 1) %>%
  activate(edges) %>%
  filter(weight > 1) %>%
  activate(nodes) %>%
  filter(!node_is_isolated()) 

#take a quick look at the result
ggraph(filtered_undirected) + geom_edge_link() + geom_node_point() + theme_graph()

#make things a bit nicer
filtered_undirected %>%
  mutate(community = as.factor(group_infomap())) %>% 
  ggraph(layout = 'nicely') + 
  geom_edge_link(aes(alpha = ..index..), show.legend = FALSE) + 
  geom_node_point(aes(colour = community), size = 7) + 
  geom_node_text(aes(label = label)) +
  theme_graph()
