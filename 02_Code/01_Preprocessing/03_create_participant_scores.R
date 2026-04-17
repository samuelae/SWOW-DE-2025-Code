# Dependencies -----------------------------------------------------------------

library(tidyverse)
library(hunspell)

# download hunspell dictionaries (.aff & .dic) for de_DE, de_CH, and de_AT from
# https://github.com/wooorm/dictionaries/tree/main/dictionaries rename each pair
# to de_DE.aff & de_DE.dic etc. and copy all to ~/Library/Spelling/ 

# Read files -------------------------------------------------------------------

data <- read_csv("01_Data/Raw/02_data.csv")

# Prepare tibbles --------------------------------------------------------------

data_long <- data %>% 
  pivot_longer(cols = starts_with("response_") | starts_with("no_further_response_"),
               names_to = c(".value", "response_position"), names_pattern = "(.*)_(.)") %>% 
  select(id_r, id_p, id_c, cue, response, response_position, no_further_response, unknown_word, section) 
data_long_no_NAs <- data_long %>% 
  filter(!is.na(response))

participant_scores <- data %>% 
  select(id_p, native_language, native_language_label) %>% 
  distinct()

# Create scores ----------------------------------------------------------------

# n-grams (proportion of responses with n-gram > 1)
participant_scores <- participant_scores %>% 
  left_join(data_long_no_NAs %>% 
              bind_cols(n_gram = map_int(data_long_no_NAs$response, 
                                         \(x) (length(unlist(strsplit(x, " ")))))) %>% 
              mutate(n_gram_larger_1 = if_else(n_gram > 1, TRUE, FALSE)) %>% 
              group_by(id_p) %>% 
              summarize(score_n_gram = sum(n_gram_larger_1) / n()),
            by = "id_p")
  
# uniqueness of responses per participant (proportion of unique responses)
participant_scores <- participant_scores %>% 
  left_join(data_long_no_NAs %>% 
              group_by(id_p) %>% 
              summarize(score_unique_responses = length(unique(response)) / n()),
            by = "id_p")

# native speaker (yes or no)
participant_scores <- participant_scores %>% 
  mutate(score_native_speaker = if_else(is.na(native_language), FALSE, TRUE))

# word spelling (proportion of correctly spelled words (case insensitive))
source("02_Code/Functions/spellchecks.R")

participant_scores <- participant_scores %>% 
  left_join(data_long_no_NAs %>% 
              bind_cols(hunspell_de_DE = spellcheck_all_cases(data_long_no_NAs$response)) %>% 
              filter(!is.na(response)) %>% 
              group_by(id_p) %>% 
              summarize(score_spelling = sum(hunspell_de_DE) / n()),
            by = "id_p")

# unknown word or no further responses (proportion of words unknown or not supplied)
participant_scores <- participant_scores |> 
  left_join(data_long |> 
              group_by(id_p) |> 
              summarize(score_unknown_or_no_further_response = sum(unknown_word | no_further_response) / n()),
            by = "id_p")

# unknown word (proportion of words unknown by participant)
participant_scores <- participant_scores %>% 
  left_join(data_long %>% 
              group_by(id_p) %>% 
              summarize(score_unknown_word = sum(unknown_word) / n()),
            by = "id_p")

# no further responses (proportion "of no further response" by participant)
participant_scores <- participant_scores %>% 
  left_join(data_long %>% 
              group_by(id_p) %>% 
              summarize(score_no_further_response = sum(no_further_response) / n()),
            by = "id_p")

# Save participant scores to disk ----------------------------------------------

# select data
participant_scores <- participant_scores %>% 
  select(-native_language, -native_language_label)

# save to disk
write_csv(participant_scores, "01_Data/Raw/03_participant_scores.csv")
