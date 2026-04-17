infer_network <- function(data, directed = TRUE) {
  
  # transform data to long format, and select relevant columns
  data_long <- data %>% 
    select(cue, starts_with("response")) %>% 
    pivot_longer(cols = starts_with("response"), names_to = "position", values_to = "response") %>% 
    select(cue, response)
  
  if(directed) {
    
    # create edge list (directed, weighted)
    edge_list <- data_long %>% 
      filter(response %in% unique(data_long$cue)) %>% 
      mutate(edge_name = paste(cue, response, sep = "->")) %>% 
      group_by(edge_name) %>% 
      count() %>% 
      mutate(word_1 = str_split_i(edge_name, "->", 1),
             word_2 = str_split_i(edge_name, "->", 2))
    
  } else {
    
    # create edge list (undirected, weighted)
    edge_list <- data_long %>% 
      filter(response %in% unique(data_long$cue)) %>% 
      mutate(edge_name = if_else(cue < response, paste(cue, response, sep = "--"), paste(response, cue, sep = "--"))) %>% 
      group_by(edge_name) %>% 
      count() %>% 
      mutate(word_1 = str_split_i(edge_name, "--", 1),
             word_2 = str_split_i(edge_name, "--", 2))
    
  }
  
  # create adjacency matrix
  words <- unique(c(edge_list$word_1, edge_list$word_2))
  adj_mat <- matrix(0, nrow = length(words), ncol = length(words))
  colnames(adj_mat) <- rownames(adj_mat) <- words
  adj_mat[cbind(edge_list$word_1, edge_list$word_2)] <- edge_list$n
  
  # import to igraph
  if(directed) {
    graph <- igraph::graph_from_adjacency_matrix(adj_mat, "directed", weighted = TRUE)
  } else {
    graph <- igraph::graph_from_adjacency_matrix(adj_mat, "max", weighted = TRUE)
  }
  
  # return graph
  graph
  
}