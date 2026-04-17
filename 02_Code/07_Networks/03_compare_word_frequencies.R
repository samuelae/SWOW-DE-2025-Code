# Dependencies -----------------------------------------------------------------

library(tidyverse)
library(readxl)

source("02_Code/07_Networks/Functions/zipf_value.R")

# Get SUBTLEX data ----------------------------------------------------

subtlex_de <- read_delim("00_Cold_Storage/SUBTLEX-DE/SUBTLEX-DE.csv", 
                         delim = ";", locale = locale(decimal_mark = ",")) %>% 
  select(word = Word, count = WFfreqcount, zipf_provided = ZipfSUBTLEX) %>% 
  mutate(language = "de")

subtlex_en <- read_excel("00_Cold_Storage/SUBTLEX/EN/SUBTLEX-US frequency list with PoS and Zipf information.xlsx") %>% 
  select(word = Word, count = FREQcount, zipf_provided = `Zipf-value`) %>% 
  mutate(language = "en")

subtlex_es_1 <- read_excel("00_Cold_Storage/SUBTLEX/ES/SUBTLEX-ESP.xlsx") %>% 
  select(word = Word...1, count = `Freq. count...2`)
subtlex_es_2 <- read_excel("00_Cold_Storage/SUBTLEX/ES/SUBTLEX-ESP.xlsx") %>% 
  select(word = Word...6, count = `Freq. count...7`)
subtlex_es_3 <- read_excel("00_Cold_Storage/SUBTLEX/ES/SUBTLEX-ESP.xlsx") %>% 
  select(word = Word...11, count = `Freq. count...12`)
subtlex_es <- bind_rows(subtlex_es_1, subtlex_es_2, subtlex_es_3) %>% 
  mutate(language = "es") %>% 
  remove_missing()

subtlex_ch <- read_excel("00_Cold_Storage/SUBTLEX/CH/SUBTLEX-CH-WF.xlsx", skip = 2) %>% 
  select(word = Word, count = WCount) %>% 
  mutate(language = "ch")

# Calculate Zipf-Values for all languages --------------------------------------

subtlex_de <- subtlex_de %>% 
  mutate(zipf_value = zipf_value(count, sum(subtlex_de$count), length(unique(subtlex_de$word))))
subtlex_en <- subtlex_en %>% 
  mutate(zipf_value = zipf_value(count, sum(subtlex_en$count), length(unique(subtlex_en$word))))
subtlex_es <- subtlex_es %>% 
  mutate(zipf_value = zipf_value(count, sum(subtlex_es$count), length(unique(subtlex_es$word))))
subtlex_ch <- subtlex_ch %>% 
  mutate(zipf_value = zipf_value(count, sum(subtlex_ch$count), length(unique(subtlex_ch$word))))

# Get SWOW original samples ----------------------------------------------------

swow_de <- read_csv("01_Data/Final/SWOW_DE_2025_R55.csv") %>% 
  select(cue) %>% 
  distinct() %>% 
  mutate(language = "de", .before = cue)

swow_en <- read_delim("00_Cold_Storage/SWOW/SWOW-EN18/SWOW-EN.R100.20180827.csv") %>% 
  select(cue) %>% 
  distinct() %>% 
  mutate(language = "en", .before = cue)

swow_rp <- read_delim("00_Cold_Storage/SWOW/SWOW-RP22/SWOWRP.R70.20220426.csv") %>% 
  select(cue) %>% 
  distinct() %>% 
  mutate(language = "es", .before = cue)

swow_zh <- read_delim("00_Cold_Storage/SWOW/SWOW-ZH24/SWOWZH.R55.20230424.csv") %>% 
  select(cue) %>% 
  distinct() %>% 
  mutate(language = "ch", .before = cue)

swow <- bind_rows(swow_de, swow_en, swow_rp, swow_zh)

# Add info on SWOW matched samples ---------------------------------------------

swow_matched <- read_csv("00_Cold_Storage/SWOW/Unified_Matched/swow_combined.csv")
swow_matched_cues <- swow_matched %>% 
  select(dataset, cue) %>% 
  distinct() %>% 
  mutate(language = case_when(dataset == "rp" ~ "es",
                              dataset == "zh" ~ "ch",
                              TRUE ~ dataset))

swow_de <- swow_de %>% 
  mutate(in_matched_set = if_else(
    cue %in% (swow_matched_cues %>% filter(language == "de") %>% pull(cue)), 
    TRUE, 
    FALSE
  ))
swow_en <- swow_en %>% 
  mutate(in_matched_set = if_else(
    cue %in% (swow_matched_cues %>% filter(language == "en") %>% pull(cue)), 
    TRUE, 
    FALSE
  ))
swow_rp <- swow_rp %>% 
  mutate(in_matched_set = if_else(
    cue %in% (swow_matched_cues %>% filter(language == "es") %>% pull(cue)), 
    TRUE, 
    FALSE
  ))
swow_zh <- swow_zh %>% 
  mutate(in_matched_set = if_else(
    cue %in% (swow_matched_cues %>% filter(language == "ch") %>% pull(cue)), 
    TRUE, 
    FALSE
  ))

# Add SUBTLEX Zipf Values to SWOW ----------------------------------------------

swow_de <- swow_de %>% 
  left_join(subtlex_de %>% select(word, zipf_value), by = c("cue" = "word"))
swow_en <- swow_en %>% 
  left_join(subtlex_en %>% select(word, zipf_value), by = c("cue" = "word"))
swow_rp <- swow_rp %>% 
  left_join(subtlex_es %>% select(word, zipf_value), by = c("cue" = "word"))
swow_zh <- swow_zh %>% 
  left_join(subtlex_ch %>% select(word, zipf_value), by = c("cue" = "word"))

swow <- bind_rows(swow_de, swow_en, swow_rp, swow_zh)

# Show Zipf-Value distributions of SWOW cues -----------------------------------

plot_data <- swow %>% 
  mutate(language = case_match(language, 
                               "de" ~ "German", 
                               "en" ~ "English",
                               "es" ~ "Rioplatense Spanish",
                               "ch" ~ "Mandarin Chinese")) %>% 
  mutate(in_matched_set = if_else(in_matched_set == TRUE, "matched", "full")) %>% 
  mutate(language = fct_relevel(language, "German", "English", "Rioplatense Spanish", "Mandarin Chinese"))

plot_data %>% 
  ggplot(aes(x = zipf_value, fill = in_matched_set)) +
  geom_histogram(bins = 50) +
  facet_wrap(~language, ncol = 1) +
  scale_fill_manual(values = c("grey70", "#3B5698FF"), name = "Data") +
  labs(x = "Zipf Value (Word Frequency)", y = "Count") +
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  theme_light() +
  theme(legend.position = "top",
        strip.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(color = "black"))

ggsave("03_Plots/dataset_matching_word_frequency.png", units = "cm",
       width = 9, height = 16, dpi = 300, scale = 1.25)

# Version with comparison to DE distribution density ---------------------------

german_rep <- plot_data %>%
  filter(language == "German") %>%
  select(zipf_value)

plot_data %>%
  ggplot(aes(x = zipf_value, fill = in_matched_set)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50) +
  geom_density(
    data = german_rep,
    aes(x = zipf_value),
    inherit.aes = FALSE,
    linewidth = 0.75
  ) +
  facet_wrap(~language, ncol = 1) +
  scale_fill_manual(values = c("grey70", "#3B5698FF"), name = "Data") +
  labs(x = "Zipf Value (Word Frequency)", y = "Density", caption = "Black line shows SWOW-DE Zipf-Value distribution") +
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  theme_light() +
  theme(
    legend.position = "top",
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(color = "black")
  )

ggsave("03_Plots/dataset_matching_word_frequency_comparison2.png", units = "cm",
       width = 9, height = 16, dpi = 300, scale = 1.25)












