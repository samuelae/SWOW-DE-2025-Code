# Dependencies -----------------------------------------------------------------

library(tidyverse)
library(httr2)

# spellchecking
source("02_Code/Functions/spellchecks.R")

# interface to local ollama instance
source("02_Code/Functions/fp_ollama_interface.R")
source("02_Code/Functions/llm_spelling_eval.R")

# data_long (including spelling details)
data_long <- read_csv("01_Data/Raw/06_data_long.csv")

# Find candidates for llm check ------------------------------------------------

data <- data_long %>%
  mutate(resp = response_corrected) %>%
  select(
    id_r,
    cue,
    resp,
    response_corrected,
    spelling_ok,
    response_position,
    starts_with("correction_")
  ) %>%
  pivot_wider(
    names_from = response_position,
    values_from = c(
      resp,
      response_corrected,
      spelling_ok,
      starts_with("correction_")
    )
  )

# Compile dataset for LLM run --------------------------------------------------

# one word masked at a time

data <- data %>% 
  mutate(resp_1 = if_else(spelling_ok_1, resp_1, "WORD"),
         resp_2 = if_else(spelling_ok_2, resp_2, "WORD"),
         resp_3 = if_else(spelling_ok_3, resp_3, "WORD"))

all_r1_inc <- data %>% 
  filter((resp_1 == "WORD")) %>% 
  select(id_r, cue, 
         resp_1, 
         resp_2 = response_corrected_2, 
         resp_3 = response_corrected_3, 
         incorrect = response_corrected_1)
all_r2_inc <- data %>% 
  filter((resp_2 == "WORD")) %>% 
  select(id_r, cue, 
         resp_1 = response_corrected_1, 
         resp_2, 
         resp_3 = response_corrected_3, 
         incorrect = response_corrected_2)
all_r3_inc <- data %>% 
  filter((resp_3 == "WORD")) %>% 
  select(id_r, cue, 
         resp_1 = response_corrected_1,
         resp_2 = response_corrected_2, 
         resp_3, 
         incorrect = response_corrected_3)

one_plus_word_incorrect <- all_r1_inc %>% 
  bind_rows(all_r2_inc) %>% 
  bind_rows(all_r3_inc)

write_csv(one_plus_word_incorrect, "01_Data/Varia/LLM_Spellcheck/masked_words.csv")

# Create human-LLM evaluation set ----------------------------------------------

set.seed(1984)
evaluation_set <- one_plus_word_incorrect %>% 
  slice_sample(n = 1000)

# export for human gold-standard
evaluation_set %>% 
  write_csv("01_Data/Varia/LLM_Spellcheck/evaluation_set_for_human.csv")

# import with human gold-standard
evaluation_set <- read_csv("01_Data/Varia/LLM_Spellcheck/evaluation_set_from_human.csv") %>% 
  mutate(human_correction = str_squish(human_correction)) %>% 
  select(-issue)

# set aside 500 cases for LLM prompt development
set_train <- evaluation_set %>% slice(1:500)

# and another 500 cases for LLM out-of-sample evaluation
set_test <- evaluation_set %>% slice(501:1000)


# LLM spelling improvement model comparison ------------------------------------

set_train <- set_train %>% 
  mutate(`llama4:latest` = NA) %>% 
  mutate(`gpt-oss:120b` = NA)

prompt_system <- paste(
  readLines(
    "01_Data/Varia/LLM_Spellcheck/prompt_development/prompt_system.txt",
    encoding = "UTF-8"
  ),
  collapse = "\n"
)
prompt_user <- paste(
  readLines(
    "01_Data/Varia/LLM_Spellcheck/prompt_development/prompt_user.txt",
    encoding = "UTF-8"
  ),
  collapse = "\n"
)

tictoc::tic()
pb <- txtProgressBar(min = 0, max = nrow(set_train), style = 3)
for (i in 1:nrow(set_train)) {
  d <- set_train[i, ]
  
  result <- chat_completion(
    user = str_glue(prompt_user),
    system = prompt_system,
    model = "llama4:latest",
    temperature = 0
  )
  
  set_train$`llama4:latest`[i] <- result
  setTxtProgressBar(pb, i)
}
close(pb)
tictoc::toc() # 3 min (3 hours for 30k)

tictoc::tic()
pb <- txtProgressBar(min = 0, max = nrow(set_train), style = 3)
for (i in 1:nrow(set_train)) {
  d <- set_train[i, ]
  
  result <- chat_completion(
    user = str_glue(prompt_user),
    system = prompt_system,
    model = "gpt-oss:120b",
    temperature = 0
  )
  
  set_train$`gpt-oss:120b`[i] <- result
  setTxtProgressBar(pb, i)
}
close(pb)
tictoc::toc() # 26 min (26 hours for 30k)

write_csv(set_train, "01_Data/Varia/LLM_Spellcheck/set_train.csv")

# Evaluate LLM spelling improvement --------------------------------------------

# gpt-oss
evaluate(set_train$incorrect, set_train$human_correction, set_train$`gpt-oss:120b`)
# accuracy_misspelled 0.624 (ideal 1)
# over_correction 0.224 (ideal 0)
# general accuracy 0.662 (ideal 1)

# llama4
evaluate(set_train$incorrect, set_train$human_correction, set_train$`llama4:latest`)
# accuracy_misspelled 0.448 (ideal 1)
# over_correction 0.528 (ideal 0)
# general accuracy 0.454 (ideal 1)

# Run LLM spelling improvement -------------------------------------------------

one_plus_word_incorrect <- one_plus_word_incorrect %>% 
  mutate(`gpt-oss:120b` = NA)

prompt_system <- paste(
  readLines(
    "01_Data/Varia/LLM_Spellcheck/prompt_development/prompt_system.txt",
    encoding = "UTF-8"
  ),
  collapse = "\n"
)
prompt_user <- paste(
  readLines(
    "01_Data/Varia/LLM_Spellcheck/prompt_development/prompt_user.txt",
    encoding = "UTF-8"
  ),
  collapse = "\n"
)

tictoc::tic()
pb <- txtProgressBar(min = 0, max = nrow(one_plus_word_incorrect), style = 3)
for (i in 1:nrow(one_plus_word_incorrect)) {
  d <- one_plus_word_incorrect[i, ]
  
  result <- chat_completion(
    user = str_glue(prompt_user),
    system = prompt_system,
    model = "gpt-oss:120b",
    temperature = 0
  )
  
  one_plus_word_incorrect$`gpt-oss:120b`[i] <- result
  setTxtProgressBar(pb, i)
}
close(pb)
tictoc::toc() # ~ 26 hours

# cache results on hdd
write_csv(one_plus_word_incorrect, "01_Data/Varia/LLM_Spellcheck/llm_spelling_improvements_gpt-oss.csv")
one_plus_word_incorrect <- read_csv("01_Data/Varia/LLM_Spellcheck/llm_spelling_improvements_gpt-oss.csv")

# Work in the corrections ------------------------------------------------------

# cache state for change tracking
pre_llm <- data_long

# prepare long version of LLM improvements
llm_improvements_long <- one_plus_word_incorrect %>% 
  pivot_longer(cols = starts_with("resp_"),
               names_to = c(".value", "response_position"), 
               names_pattern = "(.*)_(.)") %>% 
  filter(resp == "WORD") %>% 
  select(-resp, -incorrect) %>% 
  mutate(response_position = as.numeric(response_position))

# apply corrections
data_long <- data_long %>% 
  left_join(llm_improvements_long, by = c("id_r", "cue", "response_position")) %>% 
  mutate(response_corrected = if_else(is.na(`gpt-oss:120b`), 
                                      response_corrected, 
                                      `gpt-oss:120b`)) %>% 
  select(-`gpt-oss:120b`)

# multi word spell check (inkl. wikipedia whitelist)
wikipedia_whitelist <- readRDS("01_Data/Varia/wikipedia_words_whitelist.rds")
data_long <- data_long %>% 
  select(-spelling_ok) %>% 
  bind_cols(spelling_ok = spellcheck_multiword_optimized(data_long$response_corrected, 
                                                         custom_words = wikipedia_whitelist)) # 20 min

mean(data_long$spelling_ok, na.rm = TRUE) # 98.79% correctly spelled (+ 1.62%)

# mark words corrected in this step
positions_corrected <- pre_llm$response_corrected != data_long$response_corrected
data_long$correction_llm[positions_corrected] <- TRUE
rm(pre_llm)

# Save long data including spelling details ------------------------------------

write_csv(data_long, "01_Data/Raw/07_data_long.csv")

# Apply spelling improvements and save original-style data set -----------------

data <- read_csv("01_Data/Raw/06_data.csv")

data_wide <- data_long %>% 
  select(-spelling_ok, -response, -starts_with("correction_")) %>%
  rename(response = response_corrected) %>% 
  pivot_wider(names_from = response_position, values_from = c(response, no_further_response))

data <- data_wide %>% 
  left_join(data %>% select(1:3, created_at_r:longitude), by = c("id_r", "id_p", "id_c"))

write_csv(data, "01_Data/Raw/07_data.csv")
