# Small-world-ness S (Humphries & Gurney, 2008)
# type = "global" -> S^Δ (transitivity), "average" -> S^ws (avg local CC)

small_world_hg <- function(graph, iter = 100, type = c("global", "average")) {
  
  type <- match.arg(type)
  
  # H&G is defined for unweighted, undirected, simple graphs
  g <- as_undirected(graph, mode = "collapse", edge.attr.comb = "ignore")
  g <- simplify(g, edge.attr.comb = "ignore")
  
  # restrict to giant component (path length undefined otherwise)
  comp <- components(g)
  g <- induced_subgraph(g, which(comp$membership == which.max(comp$csize)))
  
  n <- vcount(g)
  m <- ecount(g)
  
  C <- transitivity(g, type = type)
  L <- mean_distance(g)
  
  # Erdős–Rényi null with same n and m
  rnd <- vapply(seq_len(iter), function(i) {
    r <- sample_gnm(n, m)
    c(transitivity(r, type = type), mean_distance(r))
  }, numeric(2))
  
  C_rand <- mean(rnd[1, ], na.rm = TRUE)
  L_rand <- mean(rnd[2, ], na.rm = TRUE)
  
  list(
    sw_S = (C / C_rand) / (L / L_rand),
    sw_C = C,
    sw_L = L
  )
}