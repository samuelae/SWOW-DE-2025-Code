# Dependencies -----------------------------------------------------------------

library(tidyverse)

# Read files -------------------------------------------------------------------

data <- read_csv("01_Data/Raw/01_data.csv")
cues <- read_csv("00_Cold_Storage/Latest/cues.csv")
set4 <- read_csv("01_Data/Varia/set_2022.csv")

# Find originals for previously corrected cues ---------------------------------

set4_typo_fixes <- set4 %>% 
  filter(source == "typo_fix") %>% 
  pull(cue)

# calculate string distances
swow_cues <- cues %>% pull(cue)
swow_cues <- swow_cues[!(swow_cues %in% set4_typo_fixes)]

potential_matches <- map(set4_typo_fixes, \(x) swow_cues[stringdist::stringdist(x, swow_cues) <= 2])
names(potential_matches) <- set4_typo_fixes

potential_matches

# manually select misspelled cues
cues_to_exclude <- c("Autostop", "bejaen", "Bousouki", "fleissig", "Komponent",
                     "Kompromiß", "Meta-Ebene", "Östereich", "Resumé", "Seperation",
                     "Südfrika", "Tricolore", "Überfluß", "fahig", "neid", "Stop",
                     "einsicht", "führer", "magazin", "Scheisse")

# Further handle misspelled cues manually --------------------------------------

# exclude all misspelled cues for which the correct version is in the data
cues_to_exclude <- c(cues_to_exclude, "Ausbilungsplatz", "Wisschenschaft", 
                     "katolisch", "Arbeitsnehmer", "Efekt", "beinflussen",
                     "Renter", "Wisschenschaft", "wichitg", "heiss",
                     "Artischoke", "Verhältniss", "Zufreidenheit")

# improve spelling for two cues for which the correct version is not in the data

# herrausragend -> herausragend
# Spass -> Spaß
cues <- cues %>% 
  mutate(cue = if_else(cue == "herrausragend", "herausragend", cue)) %>% 
  mutate(cue = if_else(cue == "Spass", "Spaß", cue))
data <- data %>% 
  mutate(cue = if_else(cue == "herrausragend", "herausragend", cue)) %>% 
  mutate(cue = if_else(cue == "Spass", "Spaß", cue))

# Remove misspelled cues from data ---------------------------------------------

cues <- cues %>% 
  mutate(exclude = if_else(cue %in% cues_to_exclude, TRUE, FALSE))

data <- data %>% 
  filter(cue %in% (cues %>% filter(exclude == FALSE) %>% pull(cue)))

# Save filtered data to disk ---------------------------------------------------

write_csv(cues, "01_Data/Raw/02_cues.csv")
write_csv(data, "01_Data/Raw/02_data.csv")
