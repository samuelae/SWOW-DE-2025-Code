# Dependencies -----------------------------------------------------------------

library(tidyverse)

# Read files -------------------------------------------------------------------

# swow-de
swow_de <- read_csv("01_Data/Final/SWOW_DE_2025_R55.csv") %>% 
  select(trial_id, participant_id, cue, response_1 = response_corrected_1, 
         response_2 = response_corrected_2, response_3 = response_corrected_3, 
         age, gender, datetime = created_at)

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
  mutate(datetime = as_datetime(datetime))

# EN
swow_en <- read_delim("00_Cold_Storage/SWOW/SWOW-EN18/SWOW-EN.R100.20180827.csv") %>% 
  select(participant_id = participantID, trial_id = id, cue, response_1 = R1, response_2 = R2, response_3 = R3, 
         age, gender, datetime = created_at) %>% 
  mutate(gender = case_when(gender == "Fe" ~ "female",
                            gender == "Ma" ~ "male",
                            gender == "X" ~ "other"))

# RP
swow_rp <- read_delim("00_Cold_Storage/SWOW/SWOW-RP22/SWOWRP.R70.20220426.csv") %>% 
  mutate(datetime = NA) %>% 
  select(participant_id = participantID, trial_id = responseID, cue, response_1 = R1, response_2 = R2,
         response_3 = R3, age, gender, datetime) %>% 
  mutate(gender = case_when(gender == "Fe" ~ "female",
                            gender == "Ma" ~ "male",
                            gender == "X" ~ "other"))

# ZH
swow_zh <- read_delim("00_Cold_Storage/SWOW/SWOW-ZH24/SWOWZH.R55.20230424.csv") %>% 
  select(participant_id = participantID, trial_id = trialsID, cue, response_1 = R1, response_2 = R2,
         response_3 = R3, age, gender, datetime = created_at) %>% 
  mutate(gender = case_when(gender == "Fe" ~ "female",
                            gender == "Ma" ~ "male",
                            gender == "X" ~ "other")) %>% 
  mutate(response_1 = na_if(response_1, "#Missing"),
         response_2 = na_if(response_2, "#Missing"),
         response_3 = na_if(response_3, "#Missing"))

# SL
swow_sl <- read_delim("00_Cold_Storage/SWOW/SWOW-SL24/SWOW-SL1.0_responses.tsv",
                      delim = "\t") %>% 
  select(participant_id = participantID, trial_id = id, cue, response_1 = response1,
         response_2 = response2, response_3 = response3)
swow_sl_ppt <- read_delim("00_Cold_Storage/SWOW/SWOW-SL24/SWOW-SL1.0_participants.tsv",
                          delim = "\t") %>% 
  select(participant_id = participantID, age, gender) %>% 
  mutate(gender = case_when(gender == "Fe" ~ "female",
                            gender == "Ma" ~ "male",
                            gender == "X" ~ "other"))
swow_sl <- swow_sl %>% 
  left_join(swow_sl_ppt, by = "participant_id") %>% 
  mutate(datetime = as_datetime(NA)) %>% 
  mutate(response_1 = na_if(response_1, "<noMoreReplies>")) %>% 
  mutate(response_2 = na_if(response_2, "<noMoreReplies>")) %>% 
  mutate(response_3 = na_if(response_3, "<noMoreReplies>")) %>% 
  mutate(response_1 = na_if(response_1, "<unknownWord>")) %>% 
  mutate(response_2 = na_if(response_2, "<unknownWord>")) %>% 
  mutate(response_3 = na_if(response_3, "<unknownWord>"))


# Save unified to disk ---------------------------------------------------------

write_csv(swow_de, "00_Cold_Storage/SWOW/Unified/swow_de.csv")
write_csv(swow_nl, "00_Cold_Storage/SWOW/Unified/swow_nl.csv")
write_csv(swow_en, "00_Cold_Storage/SWOW/Unified/swow_en.csv")
write_csv(swow_rp, "00_Cold_Storage/SWOW/Unified/swow_rp.csv")
write_csv(swow_zh, "00_Cold_Storage/SWOW/Unified/swow_zh.csv")
write_csv(swow_sl, "00_Cold_Storage/SWOW/Unified/swow_sl.csv")

# add language variable
swow_de <- swow_de %>% mutate(dataset = "de")
swow_nl <- swow_nl %>% mutate(dataset = "nl")
swow_en <- swow_en %>% mutate(dataset = "en")
swow_rp <- swow_rp %>% mutate(dataset = "rp")
swow_zh <- swow_zh %>% mutate(dataset = "zh")
swow_sl <- swow_sl %>% mutate(dataset = "sl")

# save combined file
swow <- swow_de %>% 
  bind_rows(swow_nl) %>% 
  bind_rows(swow_en) %>% 
  bind_rows(swow_rp) %>% 
  bind_rows(swow_zh) %>% 
  bind_rows(swow_sl)

write_csv(swow, "00_Cold_Storage/SWOW/Unified/swow_combined.csv")

