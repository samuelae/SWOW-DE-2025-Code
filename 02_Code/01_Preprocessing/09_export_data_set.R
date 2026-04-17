# Dependencies -----------------------------------------------------------------

library(tidyverse)

# latest version of full participant data
data <- read_csv("01_Data/Raw/07_data.csv")

# latest version of preprocessed data
data_long <- read_csv("01_Data/Raw/07_data_long.csv")

# geo information
geo_info <- read_csv("01_Data/Varia/Geocode/geo_data_lookup.csv")

# Handle corrections -----------------------------------------------------------

# collapse correction tracking
changes <- data_long %>%
  select(id_r, response_position, starts_with("correction_")) %>% 
  pivot_longer(
    cols = starts_with("correction_"),
    names_pattern = "(.)_(.*)",
    names_to = c(NA, "correction")
  ) %>% 
  filter(value) %>% 
  group_by(id_r, response_position) %>% 
  summarize(corrections = str_c(correction, collapse = ","))

# replace individual correction cols with collapsed version
data_long <- data_long %>% 
  select(-starts_with("correction_")) %>% 
  left_join(changes, by = c("id_r", "response_position"))

# Combine preprocessed responses with participant data -------------------------

# convert responses to wide format
preprocessed_wide <- data_long %>% 
  pivot_wider(names_from = response_position, 
              values_from = c(response, response_corrected, no_further_response, 
                              spelling_ok, corrections))

# select participant data 
data <- data %>% 
  select(id_r, id_p, id_c, created_at = created_at_r, age, gender, 
         education_label, native_language_label, latitude, longitude) %>% 
  left_join(preprocessed_wide, by = c("id_r", "id_p", "id_c"))

# Organize data set for publication --------------------------------------------

# select and rename cols
data <- data %>% 
  select(trial_id = id_r, participant_id = id_p, created_at, age, gender, education = education_label,
         native_language = native_language_label, latitude, longitude, section, 
         cue, unknown_word, section, starts_with("response_corrected_"), response_raw_1 = response_1, 
         response_raw_2 = response_2, response_raw_3 = response_3, starts_with("no_further_response_"),
         starts_with("spelling_ok_"), starts_with("corrections_"))

# clean up created_at (all < 2016-08-17 21:06:11 are missing)
data <- data %>% 
  mutate(created_at = if_else(as.character(created_at) == "2016-08-17 21:06:11", NA, created_at))

# add fallback date information from participants table for NA datetimes
participants_raw <- read_csv("00_Cold_Storage/Latest/participants.csv") %>% 
  select(id, p_created_at = created_at)
data <- data %>% 
  left_join(participants_raw, by = c("participant_id" = "id")) %>% 
  mutate(created_at = if_else(is.na(created_at), date(p_created_at), created_at))

# make double-sure geographical data is approximate (should be already; ~1km)
data <- data %>% 
  mutate(latitude = round(latitude, digits = 2),
         longitude = round(longitude, digits = 2))

# import human-readable geo information (city, state, country) after lat, lon
data <- data %>% 
  select(trial_id:longitude) %>% 
  left_join(geo_info, by = c("latitude", "longitude")) %>% 
  bind_cols(data %>% select(section:corrections_3))

# rename sections
data <- data %>% 
  mutate(section = case_when(section == "set_2011" ~ "1",
                             section == "set2" ~ "2",
                             section == "set3" ~ "3",
                             section == "set4" ~ "4")) %>% 
  mutate(section = as.integer(section))

# label age values as NA if 99
data <- data %>% 
  mutate(age = na_if(age, 99))

# make sure correct spelling of Luxembourg is used
data <- data %>% 
  mutate(native_language = if_else(native_language == "German (Luxemburg)", 
                                   "German (Luxembourg)",
                                   native_language))

# Create data set with exactly n responses per cue -----------------------------

# count responses per cue
response_counts <- data %>% 
  group_by(cue) %>% 
  summarize(n = n())

# calculate AUCs
sum(response_counts$n >= 57) * 57
sum(response_counts$n >= 56) * 56
sum(response_counts$n >= 55) * 55 # max (2025-08-25)
sum(response_counts$n >= 54) * 54
sum(response_counts$n >= 53) * 53

# sample exactly 55 trials for all cues that have >= 55 trials
cues_55 <- response_counts %>% filter(n >= 55) %>% pull(cue)
set.seed(1984)
data_55 <- data %>% 
  filter(cue %in% cues_55) %>% 
  group_by(cue) %>% 
  slice_sample(n = 55)

# export data set with exactly 55 trials per cue
write_csv(data_55, "01_Data/Final/SWOW_DE_2025_R55.csv") # shall be used as SWOW-DE 2025

# Create data set with all responses for the cues with >= 55 responses ---------

data_55plus <- data %>% 
  filter(cue %in% cues_55) %>% 
  mutate(R55 = trial_id %in% data_55$trial_id, .after = trial_id)

# export data set with exactly 55 trials per cue
write_csv(data_55plus, "01_Data/Final/SWOW_DE_2025_R55plus.csv")

# Export full data set with dummy variable (used in R55?) ----------------------

data_raw <- data %>% 
  mutate(R55 = trial_id %in% data_55$trial_id, .after = trial_id)

# export full data set
write_csv(data_raw, "01_Data/Final/SWOW_DE_2025_RAW.csv")

# Extract data set descriptives for paper (R55) --------------------------------

# n cues
data_55 %>% count(cue) %>% nrow()

# n cues in each section
data_55 %>% 
  select(cue, section) %>% 
  distinct(cue, section) %>% 
  group_by(section) %>% 
  summarize(n_cues = n())
data %>% 
  select(cue, section) %>% 
  distinct(cue, section) %>% 
  group_by(section) %>% 
  summarize(n_cues = n())

# calculate reduction in trials and participants because of 55 criterion
n_trials_data <- data$trial_id %>% unique() %>% length()
n_trials_data_55 <- data_55$trial_id %>% unique() %>% length()
n_trials_data - n_trials_data_55
(n_trials_data - n_trials_data_55) / n_trials_data

n_ppts_data <- data$participant_id %>% unique() %>% length()
n_ppts_data_55 <- data_55$participant_id %>% unique() %>% length()
n_ppts_data - n_ppts_data_55
(n_ppts_data - n_ppts_data_55) / n_ppts_data

# cue part of speech (determined by https://wortarten.info/)
data_55 %>% 
  distinct(cue) %>% 
  write_csv("01_Data/Varia/Cue_PoS/cues.csv", col_names = FALSE)

nouns <- read_delim("01_Data/Varia/Cue_PoS/Nomen.txt", delim = ",", col_names = "cue") %>% 
  mutate(pos = "noun")
adjectives <- read_delim("01_Data/Varia/Cue_PoS/Adjektiv.txt", delim = ",", col_names = "cue") %>% 
  mutate(pos = "adjective")
adverbs <- read_delim("01_Data/Varia/Cue_PoS/Adverb.txt", delim = ",", col_names = "cue") %>% 
  mutate(pos = "adverb")
verbs <- read_delim("01_Data/Varia/Cue_PoS/Verb.txt", delim = ",", col_names = "cue") %>% 
  mutate(pos = "verb")

data_55 %>% 
  distinct(cue) %>% 
  left_join(nouns %>% bind_rows(adjectives) %>% bind_rows(adverbs) %>% bind_rows(verbs)) %>% 
  group_by(pos) %>% 
  summarize(n_pos = n(), prop_pos = n()/length(unique(data_55$cue)))

# n participants in R55
data_55 %>% 
  ungroup() %>% 
  count(participant_id) %>% 
  nrow()

