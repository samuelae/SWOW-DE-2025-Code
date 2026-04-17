# Dependencies -----------------------------------------------------------------

library(tidyverse)
library(hunspell)

source("02_Code/Functions/spellchecks.R")
source("02_Code/Functions/wikipedia.R")

# Read files -------------------------------------------------------------------

# data
data <- read_csv("01_Data/Raw/05_data.csv")

# data_long (including spelling details)
data_long <- read_csv("01_Data/Raw/05_data_long.csv")

# Find candidates for Wikipedia check ------------------------------------------

candidates <- data_long %>% 
  filter(!spelling_ok) %>% 
  select(response_corrected) %>% 
  distinct()

candidate_words <- candidates %>% pull(response_corrected)
chunks <- split(candidate_words, ceiling(seq_along(candidate_words) / 50))

# Run Wikipedia query ----------------------------------------------------------

results <- list()
non_json <- 0 # count errors (due to forbidden characters)

for(i in 1:length(chunks)) { # 118 Seconds (700/700 success) -> 0.17 Seconds per Request

  results[[i]] <- check_wikipedia_entry(chunks[[i]])

  if(is.na(results[i])) {
    non_json <- non_json + 1
  }
}

# combine results
wikipedia <- tibble(word = character(), wikipedia_page = logical(), page_id = integer(), title = character())
for(i in 1:length(results)) {
  if(!is.na(results[i])) {
    wikipedia <- wikipedia %>% bind_rows(results[[i]])
  }
}

# Create custom whitelist  -----------------------------------------------------

# extract words
wikipedia_words <- wikipedia %>% 
  filter(wikipedia_page) %>% 
  pull(title)

# save wikipedia whitelist
saveRDS(wikipedia_words, "01_Data/Varia/wikipedia_words_whitelist.rds")

# multi word check (standard)
data_long <- data_long %>% 
  select(-spelling_ok) %>% 
  bind_cols(spelling_ok = spellcheck_multiword(data_long$response_corrected)) # 4 min

mean(data_long$spelling_ok, na.rm = TRUE) # 94.36% correctly spelled

# multi word check (including custom list)
data_long_custom <- data_long %>% 
  filter(!spelling_ok) %>% 
  bind_cols(spelling_ok_custom = spellcheck_multiword(data_long %>% 
                                                        filter(!spelling_ok) %>% 
                                                        pull(response_corrected),
                                                      custom_words = wikipedia_words)) # 17 Min

# add custom whitelisted words to whitelist
data_long <- data_long %>% 
  left_join(data_long_custom %>% select(response_corrected, spelling_ok_custom) %>% distinct(), by = "response_corrected") %>% 
  mutate(spelling_ok_custom = replace_na(spelling_ok_custom, FALSE)) %>% 
  mutate(spelling_ok = (spelling_ok | spelling_ok_custom)) %>% 
  select(-spelling_ok_custom)

mean(data_long$spelling_ok, na.rm = TRUE) # 96.38% correctly spelled (+ 2.02%)

# Improve spelling with whitelist ----------------------------------------------

# cache state for change tracking
pre_wikipedia <- data_long

candidates <- data_long %>% 
  filter(!spelling_ok) %>% 
  select(response_corrected) %>% 
  distinct()

wikipedia_corrections <- candidates %>% 
  left_join(wikipedia, by = c("response_corrected" = "word")) %>% 
  filter(wikipedia_page) %>% 
  filter(response_corrected != title) %>% 
  select(from = response_corrected, to = title) %>% 
  distinct()

data_long <- data_long %>% 
  left_join(wikipedia_corrections, by = c("response_corrected" = "from")) %>% 
  mutate(response_corrected = if_else(is.na(to), response_corrected, to)) %>% 
  select(-to)

# multi word check (including custom list)
data_long_custom <- data_long %>% 
  filter(!spelling_ok) %>% 
  bind_cols(spelling_ok_custom = spellcheck_multiword(data_long %>% 
                                                        filter(!spelling_ok) %>% 
                                                        pull(response_corrected),
                                                      custom_words = wikipedia_words)) # 11 Min

# add custom whitelisted words to whitelist
data_long <- data_long %>% 
  left_join(data_long_custom %>% select(response_corrected, spelling_ok_custom) %>% distinct(), by = "response_corrected") %>% 
  mutate(spelling_ok_custom = replace_na(spelling_ok_custom, FALSE)) %>% 
  mutate(spelling_ok = (spelling_ok | spelling_ok_custom)) %>% 
  select(-spelling_ok_custom)

mean(data_long$spelling_ok, na.rm = TRUE) # 97.17% correctly spelled (+ 0.79%)
# +2.81 precentage points to pre-wikipedia

# mark words corrected in this step
positions_corrected <- pre_wikipedia$response_corrected != data_long$response_corrected
data_long$correction_wikipedia[positions_corrected] <- TRUE
rm(pre_wikipedia)

# Save long data including spelling details ------------------------------------

write_csv(data_long, "01_Data/Raw/06_data_long.csv")

# Apply spelling improvements and save -----------------------------------------

data_wide <- data_long %>% 
  select(-spelling_ok, -response, -starts_with("correction_")) %>%
  rename(response = response_corrected) %>% 
  pivot_wider(names_from = response_position, values_from = c(response, no_further_response))

data <- data_wide %>% 
  left_join(data %>% select(1:3, created_at_r:longitude), by = c("id_r", "id_p", "id_c"))

write_csv(data, "01_Data/Raw/06_data.csv")
