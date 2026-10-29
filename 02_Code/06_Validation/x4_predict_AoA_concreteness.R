# Dependencies -----------------------------------------------------------------

library(tidyverse)
library(tidymodels)

# Functions

source("02_Code/06_Validation/Functions/predict_and_evaluate_embedding.R")
source("02_Code/06_Validation/Functions/l2_normalize_rows.R")

# Data -------------------------------------------------------------------------

# predictors
swow_embedding <- readRDS("01_Data/Varia/swow_embedding.rds") %>% l2_normalize_rows()
fT_embedding <- readRDS("01_Data/Varia/fT_embedding.rds") %>% l2_normalize_rows()

# SWOW-DE
swow_frequency <- read_csv("01_Data/Final/SWOW_DE_2025_R55.csv") %>%
  select(-unknown_word, -starts_with("no_further_response")) %>%
  pivot_longer(
    cols = matches("response_corrected_|spelling_ok_|corrections_"),
    names_to = c(".value", "position"),
    names_pattern = "(.*)_(\\d+)"
  ) %>% 
  group_by(response_corrected) %>%  
  summarize(swow_frequency = n()) %>% 
  rename(word = response_corrected) %>% 
  filter(word %in% swow_embedding$word) %>% 
  mutate(swow_frequency = log10(swow_frequency))

# SUBTLEX-DE
subtlex_frequency <- read_csv2("00_Cold_Storage/SUBTLEX-DE/SUBTLEX-DE.csv") %>% 
  select(word = Word, subtlex_frequency = SUBTLEX) %>% 
  filter(word %in% swow_embedding$word) %>% 
  mutate(subtlex_frequency = log10(subtlex_frequency))
  
# criteria (concreteness and AoA)
norms <- read_csv("00_Cold_Storage/German_Norms/psycholinguistic_ratings.csv") %>% 
  select(word, concreteness, aoa)

# Combine data (embeddings, frequencies, norms)
data <- norms %>% left_join(swow_frequency, by = "word") %>% 
  left_join(subtlex_frequency, by = "word") %>% 
  left_join(swow_embedding, by = "word") %>% 
  left_join(fT_embedding, by = "word", suffix = c(".swow", ".fT")) %>% 
  filter(!(is.na(concreteness) & is.na(aoa)))


# One dependent variable (dv) per data set -------------------------------------

rsq_values <- tibble(word_property = character(length = 0),
                     data = character(length = 0),
                     rsq = numeric(length = 0))

set.seed(1984)

for (word_property in names(norms)[-1]) {

  # select dv and both embeddings
  dv_data <- data %>% 
    select(dv = all_of(word_property), starts_with("dim_"), ends_with("_frequency")) %>% 
    drop_na()

  # swow data
  swow_embedding_data <- dv_data %>% 
    select(dv, ends_with(".swow"))
  
  # fT data
  fT_embedding_data <- dv_data %>% 
    select(dv, ends_with(".fT"))
  
  # swow frequency data
  swow_frequency_data <- dv_data %>% 
    select(dv, all_of("swow_frequency"))
  
  # subtlex frequency data
  subtlex_frequency_data <- dv_data %>% 
    select(dv, all_of("subtlex_frequency"))
  
  # combined data
  swow_swowfreq_combined_data <- dv_data %>% 
    select(dv, all_of("swow_frequency"), ends_with(".swow"))
  swow_fT_combined_data <- dv_data %>% 
    select(dv, ends_with(".swow"), ends_with(".fT"))
  swow_swowfreq_fT_combined_data <- dv_data %>% 
    select(dv, all_of("swow_frequency"), ends_with(".swow"), ends_with(".fT"))
  swow_subtlexfreq_fT_combined_data <- dv_data %>% 
    select(dv, all_of("subtlex_frequency"), ends_with(".swow"), ends_with(".fT"))
  swowfreq_fT_combined_data <- dv_data %>% 
    select(dv, all_of("swow_frequency"), ends_with(".fT"))
  
  # run ridge regression tuning and evaluation
  results_swow_embedding_data <- predict_and_evaluate_embedding(swow_embedding_data)
  results_fT_embedding_data <- predict_and_evaluate_embedding(fT_embedding_data)
  
  results_swow_frequency_data <- predict_and_evaluate_embedding(swow_frequency_data)
  results_subtlex_frequency_data <- predict_and_evaluate_embedding(subtlex_frequency_data)
  
  results_swow_swowfreq_combined_data <- predict_and_evaluate_embedding(swow_swowfreq_combined_data)
  results_swow_fT_combined_data <- predict_and_evaluate_embedding(swow_fT_combined_data)
  results_swow_swowfreq_fT_combined_data <- predict_and_evaluate_embedding(swow_swowfreq_fT_combined_data)
  results_swow_subtlexfreq_fT_combined_data <- predict_and_evaluate_embedding(swow_subtlexfreq_fT_combined_data)
  results_swowfreq_fT_combined_data <- predict_and_evaluate_embedding(swowfreq_fT_combined_data)
  
  # arrange results in tibble
  results <- tibble(
    word_property = word_property,
    data = c(
      "SWOW",
      "fastText",
      "SWOW Freq",
      "SUBTLEX Freq",
      "SWOW + SWOW Freq",
      "SWOW + fastText",
      "SWOW + SWOW Freq + fastText",
      "SWOW + SUBTLEX Freq + fastText",
      "SWOW Freq + fastText"
    ),
    rsq = c(
      results_swow_embedding_data$rsq,
      results_fT_embedding_data$rsq,
      results_swow_frequency_data$rsq,
      results_subtlex_frequency_data$rsq,
      results_swow_swowfreq_combined_data$rsq,
      results_swow_fT_combined_data$rsq,
      results_swow_swowfreq_fT_combined_data$rsq,
      results_swow_subtlexfreq_fT_combined_data$rsq,
      results_swowfreq_fT_combined_data$rsq
    ),
    ci_lower = c(
      results_swow_embedding_data$ci_lower,
      results_fT_embedding_data$ci_lower,
      results_swow_frequency_data$ci_lower,
      results_subtlex_frequency_data$ci_lower,
      results_swow_swowfreq_combined_data$ci_lower,
      results_swow_fT_combined_data$ci_lower,
      results_swow_swowfreq_fT_combined_data$ci_lower,
      results_swow_subtlexfreq_fT_combined_data$ci_lower,
      results_swowfreq_fT_combined_data$ci_lower
      
    ),
    ci_upper = c(
      results_swow_embedding_data$ci_upper,
      results_fT_embedding_data$ci_upper,
      results_swow_frequency_data$ci_upper,
      results_subtlex_frequency_data$ci_upper,
      results_swow_swowfreq_combined_data$ci_upper,
      results_swow_fT_combined_data$ci_upper,
      results_swow_swowfreq_fT_combined_data$ci_upper,
      results_swow_subtlexfreq_fT_combined_data$ci_upper,
      results_swowfreq_fT_combined_data$ci_upper
    )
  )
  
  rsq_values <- rsq_values %>%
    bind_rows(results)
}

write_csv(rsq_values, "01_Data/Varia/concretness_aoa_predictions_wordfreq.csv")

# Plot results

rsq_values %>%
  filter(
    data %in% c(
      "SWOW",
      "fastText",
      "SWOW Freq",
      "SWOW + SWOW Freq",
      "SWOW + fastText",
      "SWOW + SWOW Freq + fastText",
      "SWOW Freq + fastText"
    )
  ) %>% 
  mutate(property_label = case_when(word_property == "concreteness" ~ "Concreteness",
                                    word_property == "aoa" ~ "Age of Acquisition")) %>% 
  mutate(data = fct_reorder(data, rsq)) %>% 
  ggplot(aes(x = data, y = rsq)) +
  geom_col() +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper), 
    width = 0.3, 
    color = "grey50"
  ) +
  coord_cartesian(ylim = c(-.1, 0.9)) +
  scale_y_continuous(breaks = seq(-0.1, 0.9, 0.1), minor_breaks = NULL) +
  labs(y = expression(R^2), x = "") +
  facet_wrap(~property_label) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    strip.text = element_text(size = 12, hjust = 0, face = "bold")
  )
ggsave("03_Plots/aoa_concreteness_freq.png", width = 18, height = 10, units = "cm", bg = "white", scale = 1.25)


# Extract numbers for the paper ------------------------------------------------

# Models
rsq_values %>% 
  group_by(data) %>% 
  summarize(mean(rsq), range(rsq)[1], range(rsq)[2])

# Criteria
rsq_values %>% 
  group_by(word_property) %>% 
  summarize(mean(rsq), range(rsq)[1], range(rsq)[2]) %>% 
  arrange(desc(`mean(rsq)`))


