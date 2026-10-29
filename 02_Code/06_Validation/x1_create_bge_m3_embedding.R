# Dependencies -----------------------------------------------------------------

library(tidyverse)
library(httr2)

# Data -------------------------------------------------------------------------

# get words for which embedding is needed
swow_embedding <- readRDS("01_Data/Varia/swow_embedding.rds")

# Get LLM embedding from BAAI/bge-m3 -------------------------------------------

# prepare hf requests
vocabulary <- swow_embedding$word
chunks <- split(vocabulary, ceiling(seq_along(vocabulary) / 50))
embedding <- list()

# run hf requests
for (chunk in 1:length(chunks)) {
  
  resp <- request("https://router.huggingface.co/hf-inference/models/BAAI/bge-m3/pipeline/feature-extraction") |>
    req_method("POST") |>
    req_headers(
      "Authorization" = paste("Bearer", Sys.getenv("hf-llm-fa-spelling")),
      "Content-Type" = "application/json"
    ) |>
    req_body_json(list(inputs = paste("Das Wort ist:", chunks[[chunk]]))) |>
    req_perform()
  
  # Parse response
  embedding[[chunk]] <- resp_body_json(resp) |> map(unlist)
  names(embedding[[chunk]]) <- chunks[[chunk]]
  
}

# clean up result
bge_m3_list <- unlist(embedding, recursive = FALSE)
bge_m3_mat <- do.call(rbind, bge_m3_list)
colnames(bge_m3_mat) <- paste0("dim_", seq_len(ncol(bge_m3_mat)))
bge_m3_embedding <- as_tibble(bge_m3_mat, rownames = "word")

# save to disk
saveRDS(bge_m3_embedding, "01_Data/Varia/bge_m3_embedding.rds")
