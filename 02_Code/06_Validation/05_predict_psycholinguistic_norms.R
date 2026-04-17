# Dependencies -----------------------------------------------------------------

library(tidyverse)
library(tidymodels)

source("02_Code/06_Validation/Functions/predict_and_evaluate_embedding.R")

# Data -------------------------------------------------------------------------

l2_normalize_rows <- function(embedding_df) {
  dims <- embedding_df %>% select(-word) %>% as.matrix()
  norms <- sqrt(rowSums(dims^2))
  dims_normalized <- dims / norms
  bind_cols(word = embedding_df$word, as_tibble(dims_normalized))
}

swow_embedding <- readRDS("01_Data/Varia/swow_embedding.rds") %>% l2_normalize_rows()
fT_embedding <- readRDS("01_Data/Varia/fT_embedding.rds") %>% l2_normalize_rows()
norms <- read_csv("00_Cold_Storage/German_Norms/psycholinguistic_ratings.csv")

# Combine data (swow and fT embeddings, as well as norms)
data <- swow_embedding %>% 
  left_join(fT_embedding, by = "word", suffix = c(".swow", ".fT")) %>% 
  drop_na() %>% 
  right_join(norms, by = "word")

# One dependent variable (dv) per data set -------------------------------------

rsq_values <- tibble(word_property = character(length = 0),
                     data = character(length = 0),
                     rsq = numeric(length = 0))

set.seed(1984)

for (word_property in names(norms)[-1]) {
  
  # select dv and both embeddings
  dv_data <- data %>% 
    select(dv = all_of(word_property), starts_with("dim_")) %>% 
    filter(!is.na(dv))

  # swow data
  swow_data <- dv_data %>% 
    select(dv, ends_with(".swow")) %>% 
    drop_na()
  
  # fT data
  fT_data <- dv_data %>% 
    select(dv, ends_with(".fT")) %>% 
    drop_na()
  
  # combined data
  combined_data <- dv_data %>% 
    drop_na()
  
  # run ridge regression tuning and evaluation
  results_swow_data <- predict_and_evaluate_embedding(swow_data)
  results_fT_data <- predict_and_evaluate_embedding(fT_data)
  results_combined_data <- predict_and_evaluate_embedding(combined_data)
  
  # arrange results in tibble
  results <- tibble(word_property = word_property,
                    data = c("SWOW", "fastText", "SWOW + fastText"),
                    rsq = c(results_swow_data$rsq,
                            results_fT_data$rsq,
                            results_combined_data$rsq),
                    ci_lower = c(results_swow_data$ci_lower,
                                 results_fT_data$ci_lower,
                                 results_combined_data$ci_lower),
                    ci_upper = c(results_swow_data$ci_upper,
                                 results_fT_data$ci_upper,
                                 results_combined_data$ci_upper))

  rsq_values <- rsq_values %>% 
    bind_rows(results)
  
}

write_csv(rsq_values, "01_Data/Varia/psylin_predictions_l2normalized.csv")

# Plot rsq values for all three prediction bases -------------------------------

rsq_values %>% 
  left_join(rsq_values %>% 
              group_by(word_property, data) %>% 
              summarize(sort_value = mean(rsq)) %>% 
              filter(data == "SWOW") %>% 
              select(-data), 
            by = "word_property") %>% 
  mutate(property_label = case_when(word_property == "arousal" ~ "Arousal",
                                    word_property == "valence_lang" ~ "Valence (LANG)",
                                    word_property == "concreteness" ~ "Concreteness",
                                    word_property == "valence_angst" ~ "Valence (ANGST)",
                                    word_property == "imageability" ~ "Imageability",
                                    word_property == "arousal_anew" ~ "Arousal (ANEW)",
                                    word_property == "arousal_bawl" ~ "Arousal (BAWL)",
                                    word_property == "aoa" ~ "Age of Acquisition",
                                    word_property == "dominance" ~ "Dominance",
                                    word_property == "potency" ~ "Potency")) %>% 
  mutate(property_label = fct_reorder(property_label, -sort_value)) %>% 
  ggplot(aes(x = property_label, y = rsq, group = data, color = data)) +
  # geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = data),
  #             color = NA, alpha = 0.15, position = position_dodge(width = 0.2)) +
  geom_line(linewidth = 1, alpha = 0.5, position = position_dodge(width = 0.2)) +
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper), size = 0.5, position = position_dodge(width = 0.2)) +
  # geom_point(size = 3) +
  scale_color_manual(
    values = c("SWOW" = "#366A9FFF", fastText = "grey50", "SWOW + fastText" = "#3B2F5EFF"), 
    name = "Language Model"
  ) +
  # scale_fill_manual(
  #   values = c("SWOW" = "#366A9FFF", fastText = "grey50", "SWOW + fastText" = "#3B2F5EFF"), 
  #   name = "Embedding"
  # ) +
  coord_cartesian(ylim = c(0, 0.9)) +
  scale_y_continuous(breaks = seq(0, 0.9, 0.1), minor_breaks = NULL) +
  labs(y = expression(R^2)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "right",
        text = element_text(family = "Helvetica"))

ggsave("03_Plots/validation_psylin_l2normalized.png", width = 18, height = 8, 
       bg = "white", units = "cm", scale = 1.25)

# Extract numbers for the paper ------------------------------------------------

# Models
rsq_values %>% 
  summarize(mean(rsq), range(rsq)[1], range(rsq)[2])

rsq_values %>% 
  group_by(data) %>% 
  summarize(mean(rsq), range(rsq)[1], range(rsq)[2])

# Criteria
rsq_values %>% 
  group_by(word_property) %>% 
  summarize(mean(rsq), range(rsq)[1], range(rsq)[2]) %>% 
  arrange(desc(`mean(rsq)`))


