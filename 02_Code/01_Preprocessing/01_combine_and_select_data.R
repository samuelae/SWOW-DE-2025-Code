# Dependencies -----------------------------------------------------------------

library(tidyverse)

# Read files -------------------------------------------------------------------

cues <- read_csv("00_Cold_Storage/Latest/cues.csv")
participants <- read_csv("00_Cold_Storage/Latest/participants.csv")
responses <- read_csv("00_Cold_Storage/Latest/responses.csv")

# Combine data -----------------------------------------------------------------

full_data <- responses %>% 
  left_join(participants, by = c("participantID" = "id", "section"), suffix = c("_r", "_p")) %>% 
  left_join(cues, by = c("cue", "section"), suffix = c("", "_c"))

# Select data ------------------------------------------------------------------

data <- full_data %>% 
  select(id_r = id, id_p = participantID, id_c, 
         cue, response_1 = response1, response_2 = response2, response_3 = response3,
         section, created_at_r, 
         age, education, gender, native_language = nativeLanguage, latitude, longitude)

# Extract information from response columns ------------------------------------

data <- data %>% 
  pivot_longer(cols = starts_with("response_"), values_to = "response",
               names_sep = "_", names_to = c(NA, "response_position")) %>% 
  mutate(unknown_word = if_else(response %in% c("Unbekanntes Wort", "Unbekannest Wort", 
                                                "Unknown Word"), TRUE, FALSE),
         no_further_response = if_else(response %in% c("Keine Eingaben mehr", 
                                                       "keine Antowrt", "keine antowrt", 
                                                       "keine Anwort", "keine anwort", "?", "-"), TRUE, FALSE)) %>% 
  mutate(response = if_else(unknown_word | no_further_response, NA, response)) %>%
  pivot_wider(values_from = c(response, no_further_response), 
              names_from = response_position) %>% 
  select(id_r:cue, response_1:response_3, unknown_word, 
         no_further_response_1:no_further_response_3, section:longitude)

# Clean demographics columns ---------------------------------------------------

# Gender
data <- data %>% 
  mutate(gender = case_when(gender %in% c("Fe", "v")  ~ "female",
                            gender %in% c("Ma", "m") ~ "male",
                            gender %in% c("X") ~ "other",
                            TRUE ~ NA))

# Education
data <- data %>% 
  mutate(education = case_when(education == 1 ~ 1, # "kein Abschluss"
                               education == 2 ~ 2, # "Grundschule"
                               education == 6 ~ 3, # "Haupt- oder Realschulabschluss"
                               education == 7 ~ 4, # "Abitur"
                               education == 5 ~ 5, # "Hochschulabschluss (Diplom, Bachelor, Master)"
                               TRUE ~ NA)) %>% 
  mutate(education_label = case_when(education == 1 ~ "no degree",
                                     education == 2 ~ "elementary_school",
                                     education == 3 ~ "secondary_school_diploma",
                                     education == 4 ~ "high_school_diploma",
                                     education == 5 ~ "higher_education_degree", 
                                     .default = NA))

# Native Language
data <- data %>% 
  mutate(native_language = if_else(toupper(native_language) %in% c("DEU", "OST", "SCH", "LUX", "BE", "TR"),
                                   toupper(native_language), NA)) %>% 
  mutate(native_language_label = case_when(native_language == "DEU"  ~ "German (Germany)",
                                           native_language == "OST" ~ "German (Austria)",
                                           native_language == "SCH" ~ "German (Switzerland / Lichtenstein)",
                                           native_language == "LUX" ~ "German (Luxembourg)",
                                           native_language == "BE" ~ "German (Belgium)",
                                           native_language == "TR" ~ "German (Italy)",
                                           TRUE ~ NA))

# Order columns and save to disk -----------------------------------------------

# Order
data <- data %>% 
  select(id_r:age, gender, education, education_label, native_language, 
         native_language_label, latitude, longitude)

# Save to disk
write_csv(data, file = "01_Data/Raw/01_data.csv")
