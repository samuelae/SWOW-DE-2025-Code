# Dependencies -----------------------------------------------------------------

library(tidyverse)
library(kableExtra)

# Read files -------------------------------------------------------------------

swow_combined <- read_csv("00_Cold_Storage/SWOW/Unified/swow_combined.csv")

# Find most frequent words in each language ------------------------------------

word_frequencies <- swow_combined %>% 
  pivot_longer(cols = starts_with("response_"), names_to = "response_position", values_to = "response") %>% 
  mutate(respone_position = str_sub(response_position, start = -1)) %>% 
  count(dataset, response) %>% 
  arrange(dataset, desc(n))

top_10 <- word_frequencies %>% 
  group_by(dataset) %>% 
  mutate(response = if_else(response %in% c("x", "#Missing", "#Unknown"), NA, response)) %>% 
  filter(!is.na(response)) %>% 
  slice_max(order_by = n, n = 10) %>% 
  print(n = 100)

# add English translations -----------------------------------------------------

top_10 <- top_10 %>% 
  mutate(english_translation = response)

# de
top_10[top_10$dataset == "de", ]$english_translation <- c("money", "music", "work", "school", "water", "food", "car", "love", "old", "family")

# nl
top_10[top_10$dataset == "nl", ]$english_translation <- c("water", "money", "food", "pain", "car", "tasty", "music", "beautiful", "children", "school")

# rp
top_10[top_10$dataset == "rp", ]$english_translation <- c("water", "food", "love", "work", "pain", "money", "music", "animal", "life", "house")

# zh
top_10[top_10$dataset == "zh", ]$english_translation <- c("person", "money", "water", "work", "cute", "red", "teacher", "game", "time", "friend")

# sl
top_10[top_10$dataset == "sl", ]$english_translation <- c("money", "work", "car", "school", "sport", "job", "book", "time", "person", "love")

# Create table for paper -------------------------------------------------------

top_10 %>% 
  mutate(table = if_else(dataset == "en", response, paste0(response, " (", english_translation, ")"))) %>% 
  select(dataset, table) %>% 
  bind_cols(position = rep(1:10, times = 6)) %>% 
  pivot_wider(names_from = dataset, values_from = table) %>%
  kbl(caption = "Top 10 free association responses in SWOW datasets",
      format = "latex",
      col.names = c("Rank","German","English","Dutch","Rioplatense Spanish", "Slovene", "Mandarin Chinese"),
      align = c("r", "c", "c", "c", "c", "c", "c")) %>%
  kable_classic(full_width = FALSE, html_font = "Times New Roman")

