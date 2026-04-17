# Dependencies -----------------------------------------------------------------

library(tidyverse)
library(text2vec)
library(RSpectra)
library(Matrix)
library(httr2)

source("02_Code/06_Validation/Functions/extract_fastText_embedding.R")
source("02_Code/06_Validation/Functions/katz_rw_functions.R")

# Data -------------------------------------------------------------------------

# Relatedness judgment data
relatedness_judgments <- read_csv(
    "00_Cold_Storage/German_Norms/relatedness_judgments.csv"
  ) %>% 
  filter(source != "gurevych65")

# SWOW-DE
swow_de <- read_csv("01_Data/Final/SWOW_DE_2025_R55.csv")
swow_de_long <- swow_de %>%
  select(-unknown_word, -starts_with("no_further_response")) %>%
  pivot_longer(
    cols = matches("response_corrected_|response_raw_|spelling_ok_|corrections_"),
    names_to = c(".value", "position"),
    names_pattern = "(.*)_(\\d+)"
  )

# Extract fastText embedding for swow cue words --------------------------------

swow_cues <- swow_de %>% pull(cue) %>% unique()
fT_embedding <- extract_fastText_embedding("00_Cold_Storage/fastText/cc.de.300.vec", swow_cues)

# convert to tibble
colnames(fT_embedding) <- paste0("dim_", 1:300)
fT_embedding <- tibble(word = rownames(fT_embedding)) %>% 
  bind_cols(fT_embedding)
fT_embedding <- fT_embedding %>% filter(word %in% swow_cues) # keep casing as cues

saveRDS(fT_embedding, "01_Data/Varia/fT_embedding.rds")

# Preprare SWOW-DE data --------------------------------------------------------

swow_cue_response_counts <- swow_de_long |> 
  group_by(cue, response_corrected) |> 
  summarize(n = n()) |> 
  drop_na()

# cue-response matrix
m_cr <- matrix(
  0,
  nrow = length(unique(swow_cue_response_counts$cue)),
  ncol = length(unique(swow_cue_response_counts$response_corrected))
)
rownames(m_cr) <- unique(swow_cue_response_counts$cue)
colnames(m_cr) <- unique(swow_cue_response_counts$response_corrected)

m_cr[cbind(swow_cue_response_counts$cue, swow_cue_response_counts$response_corrected)] <- swow_cue_response_counts$n

# SWOW-COUNTS-COS - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# cos
m_cc_counts_cos <- sim2(as(m_cr, "sparseMatrix"), method = "cosine", norm = "l2")

# SWOW-PPMI-COS - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# ppmi
m_cr <- m_cr / sum(m_cr)
norm <- rowSums(m_cr) %*% t(colSums(m_cr))
m_cr <- log2(m_cr / norm); rm(norm); gc()
m_cr_ppmi <- pmax(m_cr, 0); rm(m_cr); gc()

# cos
m_cc_counts_ppmi_cos <- sim2(as(m_cr_ppmi, "sparseMatrix"), method = "cosine", norm = "l2")

# SWOW-PPMI-SVD-COS - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# svd
sm_cr_ppmi <- Matrix(m_cr_ppmi, sparse = TRUE)
svd_result <- svds(sm_cr_ppmi, k = 300) # 2.7 sec
rownames(svd_result$u) = rownames(m_cr_ppmi)
m_cr_ppmi_svd = svd_result$u %*% diag(svd_result$d)

# cos
m_cc_counts_ppmi_svd_cos <- sim2(as(m_cr_ppmi_svd, "sparseMatrix"), method = "cosine", norm = "l2")

# SWOW-RW-COS - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Based on Simon's script

alpha <- 0.75

# Convert to iGraph
G <- igraph::graph_from_data_frame(
    swow_cue_response_counts %>% 
      select(from = cue, to = response_corrected), 
    directed = TRUE
  )
G <- igraph::set_edge_attr(G, 'weight', value = swow_cue_response_counts$n)

# Extract largest connected component
comp.strong  <- extract_largest_component(G, mode = 'strong')
G.strong <- igraph::as_adjacency_matrix(comp.strong$subgraph, attr='weight', names = TRUE)
G.strong <- normalize_rows(G.strong, norm = 'l1')

# Generate the weighted graphs strength and PPMI graphs
G.w <- list()
G.w$strength <- G.strong
G.w$ppmi <- normalize_rows(PPMI(G.strong), 'l1')

# Generate the Katz-walk graph
G.w$rw <- normalize_rows(katz_walk(G.w$ppmi, alpha), 'l1')

# Reweight by PPMI (filter spurious paths)
G.w$rw <- normalize_rows(PPMI(G.w$rw), 'l1')

# Confirm sparsity
density.ppmi <- Matrix::nnzero(G.w$ppmi) / (nrow(G.w$ppmi) * ncol(G.w$ppmi))
density.rw <- Matrix::nnzero(G.w$rw) / (nrow(G.w$rw) * ncol(G.w$rw))

# Calculate a similarity matrix
S <- list()
S$rw <- cosine_matrix(G.w$rw)
#S$ppmi <- cosine_matrix(G.w$ppmi)
#S$strength <- cosine_matrix(G.w$strength)

# translate into own nomenclature
m_cc_counts_rw_ppmi_cos <- S$rw

# Extract relevant data and combine - - - - - - - - - - - - - - - - - - - - - - 

rj_in_swow <- relatedness_judgments %>% 
  filter(word_1 %in% rownames(m_cc_counts_rw_ppmi_cos), word_2 %in% rownames(m_cc_counts_rw_ppmi_cos))

rj_in_swow <- rj_in_swow |> 
  bind_cols(swow_counts = m_cc_counts_cos[cbind(rj_in_swow$word_1, rj_in_swow$word_2)], 
            swow_counts_ppmi = m_cc_counts_ppmi_cos[cbind(rj_in_swow$word_1, rj_in_swow$word_2)], 
            swow_counts_ppmi_svd = m_cc_counts_ppmi_svd_cos[cbind(rj_in_swow$word_1, rj_in_swow$word_2)],
            swow_counts_rw_ppmi = m_cc_counts_rw_ppmi_cos[cbind(rj_in_swow$word_1, rj_in_swow$word_2)])


# Prepare fastText and LLM data ------------------------------------------------ 

# fastText
m_fT_emb <- as.matrix(fT_embedding |> select(-word))
rownames(m_fT_emb) <- fT_embedding$word
m_fT_cos <- sim2(m_fT_emb, method = "cosine", norm = "l2")

rj_in_swow <- rj_in_swow |> 
  bind_cols(fastText = m_fT_cos[cbind(rj_in_swow$word_1, rj_in_swow$word_2)])

# get LLM embedding using BAAI/bge-m3
vocabulary <- unique(c(rj_in_swow$word_1, rj_in_swow$word_2))
chunks <- split(vocabulary, ceiling(seq_along(vocabulary) / 50))
embedding <- list()

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

m_bge_m3_emb <- unlist(embedding, recursive = FALSE) |> 
  map_dfr(~ as.numeric(.x)) |> 
  as.matrix() |> 
  t()
m_bge_m3_cos <- sim2(m_bge_m3_emb, method = "cosine", norm = "l2")

# Add to predictors
rj_in_swow <- rj_in_swow |> 
  bind_cols(bge_m3 = m_bge_m3_cos[cbind(rj_in_swow$word_1, rj_in_swow$word_2)])

# Save all data to disk
rj_in_swow |> write_csv("01_Data/Varia/validation_rj_data.csv")

# Correlate RJs, SWOW, and (L)LM Similarities ----------------------------------

correlation_results <- tibble(predictor = character(0),
                              source = character(0),
                              n = integer(0),
                              r = double(0),
                              p.value = double(0),
                              ci.lower = double(0),
                              ci.upper = double(0))

for(p in c("swow_counts", "swow_counts_ppmi", "swow_counts_ppmi_svd", "swow_counts_rw_ppmi", "fastText", "bge_m3")) {
  
  correlation_results <- correlation_results |> 
    bind_rows(rj_in_swow |>
    group_by(source) |>
    summarize(
      n = n(),
      r = cor.test(rating_normalized, !!sym(p))$estimate,
      p.value = cor.test(rating_normalized, !!sym(p))$p.value,
      ci.lower = cor.test(rating_normalized, !!sym(p))$conf.int[1],
      ci.upper = cor.test(rating_normalized, !!sym(p))$conf.int[2]
    ) |> 
    mutate(predictor = p, .before = 1))
  
}

# Create plot of all correlations ----------------------------------------------
correlation_results |>
  mutate(
    source = fct_relevel(
      source,
      # "gurevych65",
      "gurevych222",
      "gurevych350",
      "wordsim353",
      "simlex999",
      "wulff_younger",
      "wulff_older"
    )
  ) |>
  mutate(
    source = fct_recode(
      source,
      # "Gurevych 65" = "gurevych65",
      "Gurevych 222" = "gurevych222",
      "Gurevych 350" = "gurevych350",
      "Wordsim 353" = "wordsim353",
      "SimLex 999" = "simlex999",
      "Wulff et al. (18-32)" = "wulff_younger",
      "Wulff et al. (65-78)" = "wulff_older"
    )
  ) |>
  mutate(
    predictor = fct_recode(
      predictor,
      "BGE-M3" = "bge_m3",
      "fastText" = "fastText",
      "SWOW" = "swow_counts",
      "SWOW + PPMI" = "swow_counts_ppmi",
      "SWOW + PPMI + SVD" = "swow_counts_ppmi_svd",
      "SWOW + PPMI + RW + PPMI" = "swow_counts_rw_ppmi"
    )
  ) |>
  mutate(predictor = fct_relevel(predictor, "fastText", "BGE-M3")) |>
  ggplot(aes(x = predictor, y = r, fill = predictor)) +
  geom_col() +
  geom_errorbar(
    aes(ymin = ci.lower, ymax = ci.upper),
    width = 0.3,
    color = "grey50"
  ) +
  scale_fill_manual(
    values = c(
      "BGE-M3" = "grey75",
      "fastText" = "grey75",
      "SWOW" = "#357BA2FF",
      "SWOW + PPMI" = "#3B5698FF",
      "SWOW + PPMI + SVD" = "#3E356BFF",
      "SWOW + PPMI + RW + PPMI" = "#0B0405FF"
    ),
    guide = "none"
  ) +
  facet_wrap(~source, nrow = 1) +
  labs(x = "") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(), 
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(color = "black"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    text = element_text(family = "Helvetica")
  )

ggsave("03_Plots/validation_rj.png", width = 18, height = 10, units = "cm", bg = "white", scale = 1.25) 

# Get numbers for paper --------------------------------------------------------

# Using Fisher's Z Transformation to average correlation coefficients

# mean swow-based models
correlation_results %>% 
  filter(str_starts(predictor, "swow_")) %>% 
  summarize(mean(r), tanh(mean(atanh(r))))

# mean for each swow-based model
correlation_results %>% 
  filter(str_starts(predictor, "swow_")) %>% 
  group_by(predictor) %>% 
  summarize(mean(r), tanh(mean(atanh(r))))

# mean for text-based models
correlation_results %>% 
  filter(predictor == "fastText" | predictor == "bge_m3") %>% 
  summarize(mean(r), tanh(mean(atanh(r))))

# mean for each text-based model
correlation_results %>% 
  filter(predictor == "fastText" | predictor == "bge_m3") %>% 
  group_by(predictor) %>% 
  summarize(mean(r), tanh(mean(atanh(r))))




