l2_normalize_rows <- function(embedding_df) {
  dims <- embedding_df %>% select(-word) %>% as.matrix()
  norms <- sqrt(rowSums(dims^2))
  dims_normalized <- dims / norms
  bind_cols(word = embedding_df$word, as_tibble(dims_normalized))
}