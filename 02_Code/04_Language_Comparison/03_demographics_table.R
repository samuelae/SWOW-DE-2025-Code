# Dependencies -----------------------------------------------------------------

library(tidyverse)

# Read files -------------------------------------------------------------------

# swow-de
swow_de <- read_csv("01_Data/Final/SWOW_DE_2025_R55.csv") %>% 
  select(participant_id, age, gender, datetime = created_at, education) %>% 
  distinct()

# NL
swow_nl <- read_delim("00_Cold_Storage/SWOW/SWOW-NL12/associationData.csv", delim = ";") %>% 
  select(participant_id = recodedPP_ID, trial_id = ctr, cue, response_1 = asso1, response_2 = asso2, response_3 = asso3)
swow_nl_ppt <- read_delim("00_Cold_Storage/SWOW/SWOW-NL12/users.csv", delim = ";") %>% 
  select(participant_id = recodedPP_ID, age, gender = sexe, datetime = logindate)
swow_nl <- swow_nl %>% 
  left_join(swow_nl_ppt, by = "participant_id") %>% 
  mutate(gender = case_when(gender == "F" ~ "female",
                            gender == "M" ~ "male",
                            gender == "X" ~ "other")) %>% 
  mutate(datetime = as_datetime(datetime)) %>% 
  select(-cue, -starts_with("response"), -trial_id) %>% 
  distinct()

# EN
swow_en <- read_delim("00_Cold_Storage/SWOW/SWOW-EN18/SWOW-EN.R100.20180827.csv") %>% 
  select(participant_id = participantID, age, gender, datetime = created_at, education) %>% 
  mutate(gender = case_when(gender == "Fe" ~ "female",
                            gender == "Ma" ~ "male",
                            gender == "X" ~ "other")) %>% 
  distinct()

# RP
swow_rp <- read_delim("00_Cold_Storage/SWOW/SWOW-RP22/SWOWRP.R70.20220426.csv") %>% 
  mutate(datetime = NA) %>% 
  select(participant_id = participantID, age, gender, datetime, education) %>% 
  mutate(gender = case_when(gender == "Fe" ~ "female",
                            gender == "Ma" ~ "male",
                            gender == "X" ~ "other")) %>% 
  distinct()

# ZH
swow_zh <- read_delim("00_Cold_Storage/SWOW/SWOW-ZH24/SWOWZH.R55.20230424.csv") %>% 
  select(participant_id = participantID, age, gender, datetime = created_at, education) %>% 
  mutate(gender = case_when(gender == "Fe" ~ "female",
                            gender == "Ma" ~ "male",
                            gender == "X" ~ "other")) %>% 
  distinct()

# SL
swow_sl <- read_delim("00_Cold_Storage/SWOW/SWOW-SL24/SWOW-SL1.0_responses.tsv",
                      delim = "\t") %>% 
  select(participant_id = participantID, trial_id = id)
swow_sl_ppt <- read_delim("00_Cold_Storage/SWOW/SWOW-SL24/SWOW-SL1.0_participants.tsv",
                          delim = "\t") %>% 
  select(participant_id = participantID, age, gender, education) %>% 
  mutate(gender = case_when(gender == "Fe" ~ "female",
                            gender == "Ma" ~ "male",
                            gender == "X" ~ "other"))
swow_sl <- swow_sl %>% 
  left_join(swow_sl_ppt, by = "participant_id") %>% 
  mutate(datetime = as_datetime(NA)) %>% 
  select(-trial_id) %>% 
  distinct()

# Demographics table -----------------------------------------------------------

options(pillar.sigfig = 4)

# --- DE --- 

# n
n_participants <- swow_de$participant_id %>% unique() %>% length()
n_participants

# gender
n_gender <- sum(!is.na(swow_de$gender))
swow_de %>% 
  filter(!is.na(gender)) %>% 
  group_by(gender) %>% 
  summarize(n = n(),
            percent = n() / n_gender * 100)

# age
swow_de %>% 
  summarize(age_m = mean(age, na.rm = TRUE),
            age_sd = sd(age, na.rm = TRUE),
            age_min = min(age, na.rm = TRUE),
            age_max = max(age, na.rm = TRUE))

# education
n_education <- sum(!is.na(swow_de$education))
swow_de %>% 
  filter(!is.na(education)) %>% 
  group_by(education) %>% 
  summarize(n = n(),
            percent = n() / n_education * 100)
  
# --- NL --- 

# n
n_participants <- swow_nl$participant_id %>% unique() %>% length()
n_participants

# gender
n_gender <- sum(!is.na(swow_nl$gender))
swow_nl %>% 
  filter(!is.na(gender)) %>% 
  group_by(gender) %>% 
  summarize(n = n(),
            percent = n() / n_gender * 100)

# age
swow_nl %>% 
  mutate(age = case_when(age < 7 | age >= 99 ~ NA, .default = age)) %>%
  summarize(age_m = mean(age, na.rm = TRUE),
            age_sd = sd(age, na.rm = TRUE),
            age_min = min(age, na.rm = TRUE),
            age_max = max(age, na.rm = TRUE))


# --- EN --- 

# n
n_participants <- swow_en$participant_id %>% unique() %>% length()
n_participants

# gender
n_gender <- sum(!is.na(swow_en$gender))
swow_en %>% 
  filter(!is.na(gender)) %>% 
  group_by(gender) %>% 
  summarize(n = n(),
            percent = n() / n_gender * 100)

# age
swow_en %>%
  mutate(age = case_when(age >= 99 ~ NA, .default = age)) %>%
  summarize(age_m = mean(age, na.rm = TRUE),
            age_sd = sd(age, na.rm = TRUE),
            age_min = min(age, na.rm = TRUE),
            age_max = max(age, na.rm = TRUE))

# education
n_education <- sum(!is.na(swow_en$education))
swow_en %>% 
  filter(!is.na(education)) %>% 
  group_by(education) %>% 
  summarize(n = n(),
            percent = n() / n_education * 100) %>% 
  filter(education >= 4) %>% 
  summarize_all(sum)


# --- RP --- 

# n
n_participants <- swow_rp$participant_id %>% unique() %>% length()
n_participants

# gender
n_gender <- sum(!is.na(swow_rp$gender))
swow_rp %>% 
  filter(!is.na(gender)) %>% 
  group_by(gender) %>% 
  summarize(n = n(),
            percent = n() / n_gender * 100)

# age
swow_rp %>% 
  mutate(age = case_when(age >= 99 ~ NA, .default = age)) %>%
  summarize(age_m = mean(age, na.rm = TRUE),
            age_sd = sd(age, na.rm = TRUE),
            age_min = min(age, na.rm = TRUE),
            age_max = max(age, na.rm = TRUE))

# education
n_education <- sum(!is.na(swow_rp$education))
swow_rp %>% 
  filter(!is.na(education)) %>% 
  group_by(education) %>% 
  summarize(n = n(),
            percent = n() / n_education * 100) %>% 
  filter(education >= 4) %>% 
  summarize_all(sum)


# --- ZH --- 

# n
n_participants <- swow_zh$participant_id %>% unique() %>% length()
n_participants

# gender
n_gender <- sum(!is.na(swow_zh$gender))
swow_zh %>% 
  filter(!is.na(gender)) %>% 
  group_by(gender) %>% 
  summarize(n = n(),
            percent = n() / n_gender * 100)

# age
swow_zh %>% 
  mutate(age = case_when(age >= 99 ~ NA, .default = age)) %>%
  summarize(age_m = mean(age, na.rm = TRUE),
            age_sd = sd(age, na.rm = TRUE),
            age_min = min(age, na.rm = TRUE),
            age_max = max(age, na.rm = TRUE))

# education
n_education <- sum(!is.na(swow_zh$education))
swow_zh %>% 
  filter(!is.na(education)) %>% 
  group_by(education) %>% 
  summarize(n = n(),
            percent = n() / n_education * 100) %>% 
  filter(education >= 4) %>% 
  summarize_all(sum)


# --- SL --- 

# n
n_participants <- swow_sl$participant_id %>% unique() %>% length()
n_participants

# gender
n_gender <- sum(!is.na(swow_sl$gender))
swow_sl %>% 
  filter(!is.na(gender)) %>% 
  group_by(gender) %>% 
  summarize(n = n(),
            percent = n() / n_gender * 100)

# age
swow_sl %>% 
  summarize(age_m = mean(age, na.rm = TRUE),
            age_sd = sd(age, na.rm = TRUE),
            age_min = min(age, na.rm = TRUE),
            age_max = max(age, na.rm = TRUE))

# education
n_education <- sum(!is.na(swow_sl$education))
swow_sl %>% 
  filter(!is.na(education)) %>% 
  group_by(education) %>% 
  summarize(n = n(),
            percent = n() / n_education * 100) %>% 
  filter(education >= 5) %>% 
  summarize_all(sum)















