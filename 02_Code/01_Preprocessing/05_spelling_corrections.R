# Dependencies -----------------------------------------------------------------

library(tidyverse)
library(hunspell)

source("02_Code/Functions/spellchecks.R")
source("02_Code/Functions/improve_spelling.R")

# Read files -------------------------------------------------------------------

# data
data <- read_csv("01_Data/Raw/04_data.csv")

# convert into longer format
data_long <- data %>% 
  pivot_longer(cols = starts_with("response_") | starts_with("no_further_response_"),
               names_to = c(".value", "response_position"), 
               names_pattern = "(.*)_(.)") %>% 
  mutate(response_corrected = response) %>% 
  select(id_r, id_p, id_c, cue, response, response_corrected, response_position, 
         no_further_response, unknown_word, section) %>% 
  mutate(correction_special_chars = FALSE, 
         correction_whitespace = FALSE,
         correction_word_constructs = FALSE, 
         correction_casing_eszett_umlaut = FALSE,
         correction_wikipedia = FALSE,
         correction_llm = FALSE)

# General removal of odd characters --------------------------------------------

# cache state for change tracking
pre_special_chars <- data_long

# remove `#`
data_long <- data_long %>% 
  mutate(response_corrected = str_remove_all(response_corrected, "#"))

# remove `¨`
data_long <- data_long %>% 
  mutate(response_corrected = str_remove_all(response_corrected, "¨"))

# remove `$`
data_long <- data_long %>% 
  mutate(response_corrected = str_remove_all(response_corrected, "\\$"))

# remove `´` at start and end of string, otherwise replace by `'`
data_long <- data_long %>% 
  mutate(response_corrected = str_remove_all(response_corrected, "^´"))
data_long <- data_long %>% 
  mutate(response_corrected = str_remove_all(response_corrected, "´$"))
data_long <- data_long %>% 
  mutate(response_corrected = str_replace_all(response_corrected, "´", "'"))

# remove `^`
data_long <- data_long %>% 
  mutate(response_corrected = str_remove_all(response_corrected, "\\^"))

# remove ```
data_long <- data_long %>% 
  mutate(response_corrected = str_remove_all(response_corrected, "`"))

# remove `<` and `>`
data_long <- data_long %>% 
  mutate(response_corrected = str_remove_all(response_corrected, ">"))
data_long <- data_long %>% 
  mutate(response_corrected = str_remove_all(response_corrected, "<"))

# remove `~`
data_long <- data_long %>% 
  mutate(response_corrected = str_remove_all(response_corrected, "~"))

# remove `?`
data_long <- data_long %>% 
  mutate(response_corrected = str_remove_all(response_corrected, "\\?"))
# remove `!`
data_long <- data_long %>% 
  mutate(response_corrected = str_remove_all(response_corrected, "\\!"))

# remove `\"`
data_long <- data_long %>% 
  mutate(response_corrected = str_remove_all(response_corrected, "\\\""))

# remove `&`
data_long <- data_long %>% 
  mutate(response_corrected = str_remove_all(response_corrected, "&"))

# remove `/`-only 
data_long <- data_long %>% 
  mutate(response_corrected = str_remove_all(response_corrected, "^/$"))

# remove `.`-only 
data_long <- data_long %>% 
  mutate(response_corrected = str_remove_all(response_corrected, "^\\.{1,}$"))

# remove `..`+ follwoed by string 
data_long <- data_long %>% 
  mutate(response_corrected = str_remove_all(response_corrected, "^\\.{2,}"))

# remove `'` from encased stings
data_long <- data_long %>% 
  mutate(response_corrected = if_else(str_detect(response_corrected, "^'[:alnum:]*'$"),
                                      str_remove_all(response_corrected, "'"),
                                      response_corrected))

# mark words corrected in this step
positions_corrected <- pre_special_chars$response_corrected != data_long$response_corrected
data_long$correction_special_chars[positions_corrected] <- TRUE
rm(pre_special_chars)

# Cleanup ----------------------------------------------------------------------

# cache state for change tracking
pre_whitespace <- data_long

# remove whitespace from beginning and end of strings, and reduce internal ws to length 1
data_long <- data_long %>% 
  mutate(response_corrected = str_squish(response_corrected))

# replace empty stings with NA
data_long <- data_long %>% 
  mutate(response_corrected = if_else(response_corrected == "", NA, response_corrected))

# mark words corrected in this step
positions_corrected <- pre_whitespace$response_corrected != data_long$response_corrected
data_long$correction_whitespace[positions_corrected] <- TRUE
rm(pre_whitespace)

# Render cue-response constructs for `-tisch` etc. -----------------------------

# cache state for change tracking
pre_word_constructs <- data_long

# starts with "-xxx": replace - with cue
data_long <- data_long %>% 
  mutate(response_corrected = str_replace_all(response_corrected, "^-{1}[:lower:]+",
                                              paste0(cue, str_remove(response_corrected, "-"))))

# starts with "-" not followed by lowercase letter: remove -
data_long <- data_long %>% 
  mutate(response_corrected = str_remove_all(response_corrected, "^-"))

# ends with "xxx-": paste response without -, then cue (lowercase)
response_corrected <- data_long %>% pull(response_corrected)
cue <- data_long %>% pull(cue)
to_replace_bool <- str_detect(response_corrected, "[:lower:]+-$")
to_replace_bool <- replace_na(to_replace_bool, FALSE)
response_corrected[to_replace_bool] <- paste0(str_remove(response_corrected[to_replace_bool], "-"), str_to_lower(cue[to_replace_bool]))
data_long$response_corrected <- response_corrected

# mark words corrected in this step
positions_corrected <- pre_word_constructs$response_corrected != data_long$response_corrected
data_long$correction_word_constructs[positions_corrected] <- TRUE
rm(pre_word_constructs)

# Whitelist spelling -----------------------------------------------------------

# single word
data_long <- data_long %>% 
  bind_cols(spelling_ok = hunspell_check(data_long$response_corrected, dict = "de_DE"))
  
mean(data_long$spelling_ok, na.rm = TRUE) # as is: 81.84% correct single words

# multi word
data_long <- data_long %>% 
  select(-spelling_ok) %>% 
  bind_cols(spelling_ok = spellcheck_multiword(data_long$response_corrected)) # 4 min

mean(data_long$spelling_ok, na.rm = TRUE) # as is: 84.47% correctly spelled

# Spelling improvements (casing, eszett, umlaut) -------------------------------

# cache state for change tracking
pre_casing_eszett_umlaut <- data_long

candidates <- data_long %>% 
  filter(!spelling_ok) %>% 
  select(response_corrected) %>% 
  distinct()

improved <- map_chr(candidates %>% pull(response_corrected), improve_spelling) # 40 Seconds

candidates <- candidates %>% 
  bind_cols(improved = improved) %>% 
  filter(response_corrected != improved)

data_long <- data_long %>% 
  left_join(candidates, by = "response_corrected") %>% 
  mutate(response_corrected = if_else(is.na(improved), response_corrected, improved)) %>% 
  select(-improved)

# multi word check
data_long <- data_long %>% 
  select(-spelling_ok) %>% 
  bind_cols(spelling_ok = spellcheck_multiword(data_long$response_corrected)) # 4 min

mean(data_long$spelling_ok, na.rm = TRUE) # 94.36% correctly spelled (+ 9.90%)

# mark words corrected in this step
positions_corrected <- pre_casing_eszett_umlaut$response_corrected != data_long$response_corrected
data_long$correction_casing_eszett_umlaut[positions_corrected] <- TRUE
rm(pre_casing_eszett_umlaut)

# Save long data including spelling details ------------------------------------

write_csv(data_long, "01_Data/Raw/05_data_long.csv")

# Apply spelling improvements and save -----------------------------------------

data_wide <- data_long %>% 
  select(-spelling_ok, -response, -starts_with("correction_")) %>%
  rename(response = response_corrected) %>% 
  pivot_wider(names_from = response_position, values_from = c(response, no_further_response))

data <- data_wide %>% 
  left_join(data %>% select(1:3, created_at_r:longitude), by = c("id_r", "id_p", "id_c"))

write_csv(data, "01_Data/Raw/05_data.csv")
