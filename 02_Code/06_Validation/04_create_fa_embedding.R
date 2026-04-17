# Dependencies -----------------------------------------------------------------

#devtools::install_github("samuelae/associatoR")
library(associatoR)
library(tidyverse)
library(ggrepel)
library(umap)

# Data -------------------------------------------------------------------------

data <- read_csv("01_Data/Final/SWOW_DE_2025_R55.csv")
data_long <- data %>%
  select(-unknown_word, -starts_with("no_further_response")) %>%
  pivot_longer(
    cols = matches("response_|response_corrected_|spelling_ok_|corrections_"),
    names_to = c(".value", "position"),
    names_pattern = "(.*)_(\\d+)"
  )

# Create embedding using associatoR --------------------------------------------

# import using corrected responses
ar_obj <- ar_import(data = data_long, 
                    participant = participant_id, 
                    cue = cue, 
                    response = response_corrected,
                    participant_vars = c(age, gender, education, native_language, 
                                         latitude, longitude), 
                    response_vars = c(position, trial_id))

# create PPMI-SVD embedding with 300 dim
set.seed(1984)
ar_obj <- ar_obj %>% 
  ar_set_targets(targets = "cues") %>% 
  ar_embed_targets(method = "ppmi-svd", min_count = 5, n_dim = 300)

saveRDS(ar_obj$target_embedding %>% rename(word = target), 
        "01_Data/Varia/swow_embedding.rds")

# Create plot of FA embedding --------------------------------------------------

# cluster targets (for plot)
set.seed(1337)
ar_obj <- ar_obj |>
  ar_cluster_targets(method = "louvain", similarity = "cosine") |>
  ar_count_targets()

# show n clusters and n targets
ar_obj$targets |>
  count(cluster)

# project onto 2d (for plot)
set.seed(42)

settings <- umap.defaults
settings$metric <- "cosine"
settings$min_dist <- 0.25

projection <- umap(ar_obj$target_embedding %>% select(-target) %>% as.matrix(),
                   config = settings)

plot_data <- ar_obj$targets |>
  left_join(ar_obj$target_embedding %>% 
              select(target) %>% 
              bind_cols(as_tibble(projection$layout, .name_repair)), 
            by = "target") |>
  rename(dim_1 = V1, dim_2 = V2) |>
  arrange(desc(frequency))

# Translate labels for paper plot
# gl_auth(Sys.getenv("google_cloud")) # costs (almost) nothing
# translation <- gl_translate(str_to_lower(plot_data$target), target = "en", source = "de")
# write_csv(translation, "01_Data/Varia/Plotting/cue_translations.csv")
translation <- read_csv("01_Data/Varia/Plotting/cue_translations.csv")
plot_data <- plot_data %>%
  mutate(target_lower = str_to_lower(target)) %>% 
  left_join(translation, by = c("target_lower" = "text")) %>% 
  select(target, cluster, frequency, dim_1, dim_2, target_en = translatedText)

# Plot for paper
set.seed(1337)
paper_plot <- plot_data |>
  ggplot(aes(x = dim_1, y = dim_2)) +
  geom_point(
    aes(size = frequency, color = cluster),
    alpha = 0.10,
    show.legend = FALSE
  ) +
  geom_text_repel(
    data = plot_data |> slice_head(prop = 0.15),
    mapping = aes(label = target_en, size = frequency, color = cluster),
    max.overlaps = 25,
    force = 0.25,
    family = "Arial",
    fontface = "bold"
  ) +
  guides(size = "none", color = "none") +
  scale_size(range = c(0, 10)) +
  scale_color_viridis_d(option = "mako", begin = 0.2, end = .8) +
  theme_void()

ggsave(
  "03_Plots/swow_embedding_paper.png",
  paper_plot,
  width = 40,
  height = 47.5,
  units = "cm",
  bg = "white",
  dpi = 600
)

# German version
set.seed(1337)
paper_plot <- plot_data |>
  ggplot(aes(x = dim_1, y = dim_2)) +
  geom_point(
    aes(size = frequency, color = cluster),
    alpha = 0.10,
    show.legend = FALSE
  ) +
  geom_text_repel(
    data = plot_data |> slice_head(prop = 0.15),
    mapping = aes(label = target, size = frequency, color = cluster),
    max.overlaps = 25,
    force = 0.5,
    family = "Arial",
    fontface = "bold"
  ) +
  guides(size = "none", color = "none") +
  scale_size(range = c(0, 10)) +
  scale_color_viridis_d(option = "mako", begin = 0.2, end = .8) +
  theme_void()

ggsave(
  "03_Plots/swow_embedding_paper_de.png",
  paper_plot,
  width = 40,
  height = 47.5,
  units = "cm",
  bg = "white",
  dpi = 600
)
