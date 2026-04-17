# Dependencies -----------------------------------------------------------------

library(tidyverse)

# Read files -------------------------------------------------------------------

# swow-de
data <- read_csv("01_Data/Final/SWOW_DE_2025_R55.csv")

# Extract participants ---------------------------------------------------------

# distinct participants
participants <- data %>% 
  select(participant_id, age, gender, education, native_language, 
         latitude, longitude) %>% 
  distinct()


options(pillar.sigfig = 5)

participants %>% 
  count(education) %>% 
  mutate(available = !is.na(education)) %>% 
  group_by(available) %>% 
  summarize(n = sum(n)) %>% 
  mutate(precent = n / nrow(participants) * 100) %>% 
  arrange(desc(n))

participants %>% 
  filter(!is.na(education)) %>% 
  count(education) %>% 
  mutate(percent = n / nrow(participants %>% filter(!is.na(education))) * 100)
