analyze_network <- function(graph) {
  
  nw_params <- list()
  
  # n nodes
  nw_params$n_nodes <- V(graph) %>% length()
  
  # n edges
  nw_params$n_edges <- E(graph) %>% length()
  
  # giant component
  nw_params$n_components <- components(graph)$no
  nw_params$n_giant_component <- components(graph)$csize
  
  # average strength
  nw_params$avg_strength <- strength(graph) %>% mean()
  
  # cc
  nw_params$avg_local_cc <- transitivity(graph, type = "average")
  
  # modularity
  if(!is_directed(graph)) {
    nw_params$modularity <- modularity(graph, membership = cluster_louvain(graph)$membership)
  }
  
  # aspl
  inv <- graph
  weigths <- edge_attr(inv, "weight")
  edge_attr(inv, "weight") <- (max(weigths) + 1) - weigths
  nw_params$aspl <- distances(inv) %>% mean()
  
  # retrun network parameters
  nw_params
  
}