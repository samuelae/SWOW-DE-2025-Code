# ================================================================
# Spreading Activation via Katz Walks
# ================================================================
# This script implements a spreading activation process based on
# Katz-style random walks (Newman, 2010, "Networks: An Introduction").
# The method incorporates indirect associative structure into
# semantic networks by summing over all possible paths.
#
# ----------------------------
# Overview
# ----------------------------
# The algorithm computes a path-augmented graph by summing over all
# walks between nodes, with longer paths exponentially downweighted.
#
# Parameter:
#   alpha (α): attenuation factor for longer paths
#     - must satisfy: 0 < alpha < 1
#     - higher values increase contribution of indirect paths
#
# ----------------------------
# Inputs
# ----------------------------
# 1. Adjacency file (triplet format):
#       i   j   f
#    where:
#       i = cue index
#       j = response index
#       f = frequency of response j given cue i
#
# 2. Label file:
#    Mapping from indices (i, j) to lexical items
#
# ----------------------------
# Preprocessing
# ----------------------------
# - Restrict graph to the largest strongly connected component
# - Remove self-loops
#
# ----------------------------
# Output
# ----------------------------
# G.rw:
#   Graph incorporating indirect paths, renormalized and weighted
#   using Positive Pointwise Mutual Information (PPMI)
#
# ----------------------------
# Method Details
# ----------------------------
# Spreading Activation:
#   - Short paths contribute more strongly
#   - Longer paths are downweighted by alpha^k (k = path length)
#
# PPMI Weighting:
#   - Emphasizes informative co-occurrences
#   - Known bias toward rare events (Turney & Pantel, 2010)
#   - For large graphs (>12k nodes), consider weighted PPMI
#     (Levy, Goldberg & Dagan, 2015)
#
# Similarity Computation:
#   - Full pairwise cosine similarity is memory intensive
#   - Use only if sufficient RAM is available
#   - Otherwise compute similarities on demand
#
# ----------------------------
# Default Parameters
# ----------------------------
# alpha = 0.75
#   Provides a good balance between local and global structure
#
# ----------------------------
# References
# ----------------------------
# De Deyne, S., Navarro, D., Perfors, A., Storms, G. (2016).
#   Structure at every scale: A semantic network account of the
#   similarities between unrelated concepts. JEP:G.
#
# Levy, O., Goldberg, Y., & Dagan, I. (2015).
#   Improving distributional similarity with lessons learned
#   from word embeddings. TACL, 3, 211-225.
#
# Newman, M. (2010).
#   Networks: An Introduction. Oxford University Press.
#
# Turney, P. D., & Pantel, P. (2010).
#   From frequency to meaning: Vector space models of semantics.
#   JAIR, 37, 141-188.
#
# ----------------------------
# Contact
# ----------------------------
# Simon De Deyne
# simon.dedeyne@unimelb.edu.au
#
# Last updated: 4 April 2026
# ================================================================




# Compute a Positive Pointwise Mutual Information (PPMI) transform.
# Accepts a matrix or tbl_graph with weighted edges. Columns are scaled
# by their average marginal weight, log2 is applied to nonzero entries,
# and negative values are truncated to zero.
PPMI <- function(P) {
  if (inherits(P, "tbl_graph")) {
    P <- igraph::as_adjacency_matrix(P, attr = "weight", names = TRUE)
  }
  
  dn = dimnames(P)
  N <- nrow(P)
  D <- Matrix::Diagonal(x = 1 / (Matrix::colSums(P) / N))
  P <- P %*% D
  
  # log only stored nonzero entries
  P@x <- log2(P@x)
  
  # truncate negative PMI values to zero, preserving sparsity structure
  P@x[P@x < 0] <- 0
  
  # optionally drop explicit zeros from sparse representation
  P <- Matrix::drop0(P)
  dimnames(P) = dn
  P
}


# ------------------------------------------------------------------
# Extract the largest connected component from a graph
#
# This function identifies the largest connected component in an igraph
# object and returns both the induced subgraph and summary information
# about the component structure.
#
# Args:
#   G    : An igraph graph object.
#   mode : Type of component structure to use:
#            - "weak"   : weakly connected components
#            - "strong" : strongly connected components
#          For undirected graphs, this distinction has no effect.
#
# Returns:
#   A named list with the following elements:
#     max_component    : index of the largest component
#     max_size         : number of vertices in the largest component
#     component_sizes  : vector of sizes for all components
#     removed_vertices : names of vertices removed from the graph
#     subgraph         : induced subgraph containing only the largest component
#
# Details:
#   - If multiple components tie for largest size, the first is returned.
#   - Vertices outside the largest component are removed.
#   - Vertex names are returned when present; otherwise the vector of
#     removed vertices may be empty or NULL depending on the graph.
#
# Notes:
#   - In many semantic-network applications, analyses are restricted to
#     the largest connected component so that all retained nodes remain
#     mutually reachable.
#
# Example:
#   comp <- extract_largest_component(G, mode = "strong")
#   G_main <- comp$subgraph
# ------------------------------------------------------------------
extract_largest_component <- function(G, mode = c("weak", "strong")) {
  mode <- match.arg(mode)
  
  if (!inherits(G, "igraph")) {
    stop("`G` must be an igraph graph object.")
  }
  
  comp <- igraph::components(G, mode = mode)
  
  max_component <- which.max(comp$csize)
  max_size <- comp$csize[max_component]
  component_sizes <- comp$csize
  
  keep_vertices <- which(comp$membership == max_component)
  drop_vertices <- which(comp$membership != max_component)
  
  removed_vertices <- igraph::V(G)$name[drop_vertices]
  subgraph <- igraph::induced_subgraph(G, vids = keep_vertices)
  
  result <- list(
    max_component = max_component,
    max_size = max_size,
    component_sizes = component_sizes,
    removed_vertices = removed_vertices,
    subgraph = subgraph
  )
  
  return(result)
}


# ------------------------------------------------------------------
# Compute a Katz walk / spreading-activation matrix
#
# This function computes the Katz transform of an adjacency or transition
# matrix. The Katz transform sums over walks of all lengths, with longer
# walks exponentially downweighted by the attenuation parameter `alpha`.
#
# In matrix form:
#
#   K = (I - alpha * G)^(-1)
#
# where:
#   - G is the input graph matrix
#   - I is the identity matrix
#   - alpha controls the contribution of longer paths
#
# This representation can be interpreted as a form of spreading activation
# or path-augmented connectivity, where both direct and indirect paths
# contribute to the final association strength.
#
# Args:
#   G     : A square numeric matrix or sparse Matrix object representing
#           a weighted graph, adjacency matrix, or transition matrix.
#   alpha : A positive attenuation parameter. Larger values give more
#           weight to longer walks.
#
# Returns:
#   A matrix K of the same dimensions as `G`, containing the Katz walk
#   values. Row and column names are preserved when available.
#
# Details:
#   - The inverse exists only when (I - alpha * G) is nonsingular.
#   - In practice, alpha must be small enough for the series to converge,
#     typically:
#
#         alpha < 1 / lambda_max(G)
#
#     where lambda_max(G) is the largest eigenvalue (spectral radius) of G.
#
# Notes:
#   - Small alpha emphasizes local structure.
#   - Larger alpha incorporates more indirect structure, but may lead
#     to numerical instability if chosen too close to the convergence bound.
#   - For large sparse graphs, solving the system may still be expensive,
#     and explicit inversion can become memory intensive.
#
# Example:
#   K <- katz_walk(G, alpha = 0.75)
# ------------------------------------------------------------------
katz_walk <- function(G, alpha) {
  if (!is.matrix(G) && !inherits(G, "Matrix")) {
    stop("`G` must be a base matrix or a Matrix object.")
  }
  
  if (nrow(G) != ncol(G)) {
    stop("`G` must be square.")
  }
  
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha <= 0) {
    stop("`alpha` must be a single positive number.")
  }
  
  I <- Matrix::Diagonal(n = nrow(G))
  K <- solve(I - alpha * G)
  
  if (!is.null(dimnames(G))) {
    dimnames(K) <- dimnames(G)
  }
  
  return(K)
}


# ------------------------------------------------------------------
# Row-normalize a matrix
#
# This function rescales each row of a matrix to unit length under
# either the L1 norm (row sums equal 1) or the L2 norm (Euclidean
# row norm equal 1). It is mainly intended for sparse association
# or transition matrices, such as those used in random-walk or
# spreading-activation representations.
#
# Args:
#   m    : A numeric matrix or Matrix::sparseMatrix. Rows are treated
#          as observations or node representations to be normalized.
#   norm : Type of normalization:
#            - "l1"   : divide each row by its row sum
#            - "l2"   : divide each row by its Euclidean norm
#            - "none" : return the input unchanged
#
# Returns:
#   A matrix of the same dimensions as `m`, with each row normalized
#   according to `norm`. Zero rows are left unchanged.
#
# Details:
#   - For L1 normalization, each row is divided by sum(row).
#   - For L2 normalization, each row is divided by sqrt(sum(row^2)).
#   - Rows with norm 0 are not rescaled, to avoid division by zero.
#   - Sparse matrices are handled efficiently using diagonal
#     left-multiplication.
#
# Notes:
#   - L1 normalization is typically appropriate when rows should be
#     interpreted as probability distributions or transition weights.
#   - L2 normalization is typically appropriate when cosine similarity
#     will be computed later.
#   - Negative row sums under L1 normalization are allowed by the code,
#     but in most random-walk applications the input should be
#     nonnegative.
#
# Example:
#   M_l1 <- normalize_rows(M, "l1")
#   M_l2 <- normalize_rows(M, "l2")
# ------------------------------------------------------------------
normalize_rows <- function(m, norm = c("l1", "l2", "none")) {
  norm <- match.arg(norm)
  
  if (!is.matrix(m) && !inherits(m, "Matrix")) {
    stop("`m` must be a base matrix or a Matrix object.")
  }
  
  if (norm == "none") {
    return(m)
  }
  
  row_norms <- switch(
    norm,
    l1 = Matrix::rowSums(m),
    l2 = sqrt(Matrix::rowSums(m ^ 2))
  )
  
  # Avoid division by zero: leave zero rows unchanged
  scale_vec <- ifelse(row_norms > 0, 1 / row_norms, 0)
  
  if (inherits(m, "sparseMatrix")) {
    return(Matrix::Diagonal(x = scale_vec) %*% m)
  } else {
    return(m * scale_vec)
  }
}



# ------------------------------------------------------------------
# Compute a cosine similarity matrix
#
# This function computes the full pairwise cosine similarity between
# the rows of a matrix. Each row is first L2-normalized, after which
# cosine similarity is obtained by taking the row cross-product.
#
# Args:
#   G : A numeric matrix or Matrix object. Rows are treated as vectors
#       for which pairwise cosine similarity will be computed.
#
# Returns:
#   A dense numeric matrix S, where:
#     S[i, j] = cosine similarity between row i and row j of G
#
# Details:
#   - Rows are normalized to unit L2 length before similarity is computed.
#   - Cosine similarity is then obtained as:
#
#         S = Gn %*% t(Gn)
#
#     where Gn is the row-normalized version of G.
#   - Zero rows remain zero after normalization and will have cosine
#     similarity 0 with all rows, including themselves.
#
# Notes:
#   - The result is always dense, even if G is sparse.
#   - This can be memory intensive for large matrices, since the output
#     has nrow(G)^2 entries.
#   - For large-scale applications, consider computing similarities only
#     for selected rows or nearest neighbors.
#
# Example:
#   S <- cosine_matrix(G)
# ------------------------------------------------------------------
cosine_matrix <- function(G) {
  if (!is.matrix(G) && !inherits(G, "Matrix")) {
    stop("`G` must be a base matrix or a Matrix object.")
  }
  
  G <- as(G, "Matrix")
  Gn <- normalize_rows(G, norm = "l2")
  
  S <- tcrossprod(Gn)
  S <- as.matrix(S)
  colnames(S) <- colnames(G)
  rownames(S) <- colnames(G)
  
  return(S)
}