# Dependencies -----------------------------------------------------------------

library(tidyverse)

# SWOW-DE data -----------------------------------------------------------------

swow_de <- read_csv("01_Data/Final/SWOW_DE_2025_R55.csv") %>% 
  select(trial_id, participant_id, cue, response_1 = response_corrected_1, 
         response_2 = response_corrected_2, response_3 = response_corrected_3, 
         age, gender, datetime = created_at)
n_cues <- length(unique(swow_de$cue))
n_resp <- 55

# Create SWOW-EN data with highest response-frequency cues ---------------------

swow_en <- read_delim("00_Cold_Storage/SWOW/SWOW-EN18/SWOW-EN.complete.20180827.csv")

# count cues in responses
swow_en_cue_counts <- swow_en %>% 
  select(cue) %>% 
  distinct() %>% 
  left_join(swow_en %>% 
              select(R1, R2, R3) %>% 
              pivot_longer(cols = c(R1, R2, R3), names_to = NULL, values_to = "response") %>% 
              count(response), by = c("cue" = "response")) %>% 
  arrange(desc(n))

# extract most frequent n_cues
swow_en_cues_to_include <- swow_en_cue_counts %>% 
  slice_max(n, n = n_cues, with_ties = FALSE) %>% 
  pull(cue)

# sample n_resp responses per included cue
set.seed(1984)
swow_en_subset <- swow_en %>% 
  filter(cue %in% swow_en_cues_to_include) %>% 
  group_by(cue) %>% 
  slice_sample(n = n_resp)

# unify format
swow_en_subset <- swow_en_subset %>% 
  select(participant_id = participantID, trial_id = id, cue, response_1 = R1, response_2 = R2, response_3 = R3, 
         age, gender, datetime = created_at) %>% 
  mutate(gender = case_when(gender == "Fe" ~ "female",
                            gender == "Ma" ~ "male",
                            gender == "X" ~ "other"))

# Create SWOW-RP data with highest response-frequency cues ---------------------

swow_rp <- read_delim("00_Cold_Storage/SWOW/SWOW-RP22/SWOWRP.R70.20220426.csv")

# count cues in responses
swow_rp_cue_counts <- swow_rp %>% 
  select(cue) %>% 
  distinct() %>% 
  left_join(swow_rp %>% 
              select(R1, R2, R3) %>% 
              pivot_longer(cols = c(R1, R2, R3), names_to = NULL, values_to = "response") %>% 
              count(response), by = c("cue" = "response")) %>% 
  arrange(desc(n))

# extract most frequent n_cues
swow_rp_cues_to_include <- swow_rp_cue_counts %>% 
  slice_max(n, n = n_cues, with_ties = FALSE) %>% 
  pull(cue)

# sample n_resp responses per included cue
set.seed(1984)
swow_rp_subset <- swow_rp %>% 
  filter(cue %in% swow_rp_cues_to_include) %>% 
  group_by(cue) %>% 
  slice_sample(n = n_resp)

# unify format
swow_rp_subset <- swow_rp_subset %>% 
  mutate(datetime = NA) %>% 
  select(participant_id = participantID, trial_id = responseID, cue, response_1 = R1, response_2 = R2,
         response_3 = R3, age, gender, datetime) %>% 
  mutate(gender = case_when(gender == "Fe" ~ "female",
                            gender == "Ma" ~ "male",
                            gender == "X" ~ "other"))

# Create SWOW-ZH data with highest response-frequency cues ---------------------

swow_zh <- read_delim("00_Cold_Storage/SWOW/SWOW-ZH24/SWOWZH.R55.20230424.csv")

# count cues in responses
swow_zh_cue_counts <- swow_zh %>% 
  select(cue) %>% 
  distinct() %>% 
  left_join(swow_zh %>% 
              select(R1, R2, R3) %>% 
              pivot_longer(cols = c(R1, R2, R3), names_to = NULL, values_to = "response") %>% 
              count(response), by = c("cue" = "response")) %>% 
  arrange(desc(n))

# extract most frequent n_cues
swow_zh_cues_to_include <- swow_zh_cue_counts %>% 
  slice_max(n, n = n_cues, with_ties = FALSE) %>% 
  pull(cue)

# sample n_resp responses per included cue
set.seed(1984)
swow_zh_subset <- swow_zh %>% 
  filter(cue %in% swow_zh_cues_to_include) %>% 
  group_by(cue) %>% 
  slice_sample(n = n_resp)

# unify format
swow_zh_subset <- swow_zh_subset %>% 
  select(participant_id = participantID, trial_id = trialsID, cue, response_1 = R1, response_2 = R2,
         response_3 = R3, age, gender, datetime = created_at) %>% 
  mutate(gender = case_when(gender == "Fe" ~ "female",
                            gender == "Ma" ~ "male",
                            gender == "X" ~ "other")) %>% 
  mutate(response_1 = na_if(response_1, "#Missing"),
         response_2 = na_if(response_2, "#Missing"),
         response_3 = na_if(response_3, "#Missing"))

# Create SWOW-NL data with highest response-frequency cues ---------------------

swow_nl <- read_delim("00_Cold_Storage/SWOW/SWOW-NL12/associationData.csv")

# count cues in responses
swow_nl_cue_counts <- swow_nl %>% 
  select(cue) %>% 
  distinct() %>% 
  left_join(swow_nl %>% 
              select(asso1, asso2, asso3) %>% 
              pivot_longer(cols = c(asso1, asso2, asso3), names_to = NULL, values_to = "response") %>% 
              count(response), by = c("cue" = "response")) %>% 
  arrange(desc(n))

# extract most frequent n_cues
swow_nl_cues_to_include <- swow_nl_cue_counts %>% 
  slice_max(n, n = n_cues, with_ties = FALSE) %>% 
  pull(cue)

# sample n_resp responses per included cue
set.seed(1984)
swow_nl_subset <- swow_nl %>% 
  filter(cue %in% swow_nl_cues_to_include) %>% 
  group_by(cue) %>% 
  slice_sample(n = n_resp)

# unify format
swow_nl_subset <- swow_nl_subset %>% 
  select(participant_id = recodedPP_ID, trial_id = ctr, cue, response_1 = asso1, response_2 = asso2,
         response_3 = asso3) %>% 
  mutate(age = NA, gender = NA, datetime = NA)

# Export and combine all datasets ----------------------------------------------

write_csv(swow_de, "00_Cold_Storage/SWOW/Unified_Matched_Highest_Frequency/swow_de.csv")
write_csv(swow_en_subset, "00_Cold_Storage/SWOW/Unified_Matched_Highest_Frequency/swow_en.csv")
write_csv(swow_rp_subset, "00_Cold_Storage/SWOW/Unified_Matched_Highest_Frequency/swow_rp.csv")
write_csv(swow_zh_subset, "00_Cold_Storage/SWOW/Unified_Matched_Highest_Frequency/swow_zh.csv")
write_csv(swow_nl_subset, "00_Cold_Storage/SWOW/Unified_Matched_Highest_Frequency/swow_nl.csv")

# add language variable
swow_de <- swow_de %>% mutate(dataset = "de")
swow_en_subset <- swow_en_subset %>% mutate(dataset = "en")
swow_rp_subset <- swow_rp_subset %>% mutate(dataset = "rp")
swow_zh_subset <- swow_zh_subset %>% mutate(dataset = "zh")
swow_nl_subset <- swow_nl_subset %>% mutate(dataset = "nl")

# save combined file
swow_matched <- swow_de %>% 
  bind_rows(swow_en_subset) %>% 
  bind_rows(swow_rp_subset) %>% 
  bind_rows(swow_zh_subset) %>% 
  bind_rows(swow_nl_subset)

write_csv(swow_matched, "00_Cold_Storage/SWOW/Unified_Matched_Highest_Frequency/swow_combined.csv")


