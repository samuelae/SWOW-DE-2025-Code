# Dependencies -----------------------------------------------------------------

library(tidyverse)
library(readxl)

# Lexical Decisions ------------------------------------------------------------

load("00_Cold_Storage/German_Norms/DeveL/DeveL_Words.RData")
devel_words <- tibble(ld.rt)
load("00_Cold_Storage/German_Norms/DeveL/DeveL_Nonwords.RData")
devel_nonwords <- tibble(ld.rt)

# combine lexical decision task data sets
lexical_decision_task <- devel_words |> 
  select(word, ends_with(".m")) |> 
  mutate(is_word = TRUE) |> 
  bind_rows(devel_nonwords |> select(word = nonword, ends_with(".m")) |> 
              mutate(is_word = FALSE)) |> 
  pivot_longer(ends_with(".m"), names_to = "age_group", values_to = "reaction_time") |> 
  separate_wider_delim(age_group, delim = ".", names = c(NA, "age_group", NA))

# Relatedness Judgments --------------------------------------------------------

wordsim353 <- read_delim(
  "00_Cold_Storage/German_Norms/wordsim353-multilingual/wordsim353-german.txt",
  delim = "\t"
) |> 
  rename(word_1 = Word1, word_2 = Word2, rating_original = Score) |> 
  mutate(source = "wordsim353")

simlex999 <- read_delim(
    "00_Cold_Storage/German_Norms/SimLex-999/simlex-german.txt"
  ) |> 
  rename(word_1 = `word 1`, word_2 = `word 2`, rating_original = score) |> 
  mutate(source = "simlex999")

gurevych65 <- read_delim(
  "00_Cold_Storage/German_Norms/TU Darmstadt/wortpaare65.gold.pos.txt",
  delim = ":"
) |> 
  select(word_1 = `#Word1`, word_2 = Word2, rating_original = Goldstandard) |> 
  mutate(source = "gurevych65")

gurevych222 <- read_delim(
  "00_Cold_Storage/German_Norms/TU Darmstadt/wortpaare222.gold.pos.txt",
  delim = ":"
) |> 
  select(word_1 = `#WORD1`, word_2 = WORD2, rating_original = GOLDSTANDARD) |> 
  mutate(source = "gurevych222")

gurevych350 <- read_delim(
  "00_Cold_Storage/German_Norms/TU Darmstadt/wortpaare350.gold.pos.txt",
  delim = ":"
) |> 
  select(word_1 = `#WORD1`, word_2 = WORD2, rating_original = Value) |> 
  mutate(source = "gurevych350")

wulff_2022 <- readRDS("00_Cold_Storage/German_Norms/Structural_Differences/Tablet_SimRatings.RDS")
wulff_2022 <- wulff_2022 |> 
  group_by(pair_id, group) |> 
  summarize(rating = mean(rating)) |> 
  ungroup() |> 
  left_join(wulff_2022 |> 
              select(pair_id, left_word, right_word) |> 
              distinct(), by = "pair_id")
wulff_younger <- wulff_2022 |> 
  filter(group == "young") |> 
  select(word_1 = left_word, word_2 = right_word, rating_original = rating) |> 
  mutate(source = "wulff_younger")
wulff_older <- wulff_2022 |> 
  filter(group == "old") |> 
  select(word_1 = left_word, word_2 = right_word, rating_original = rating) |> 
  mutate(source = "wulff_older")

wulff_older |> 
  summarize(min(rating_original), max(rating_original))

# combine relatedness judgment data sets and normalize ratings to interval [0, 1]
relatedness_judgments <- wordsim353 |> 
  mutate(rating_normalized = rating_original / 10) |> 
  bind_rows(simlex999 |> mutate(rating_normalized = rating_original / 10)) |>
  bind_rows(gurevych65 |> mutate(rating_normalized = rating_original / 4)) |> 
  bind_rows(gurevych222 |> mutate(rating_normalized = rating_original / 4)) |> 
  bind_rows(gurevych350 |> mutate(rating_normalized = rating_original / 4)) |> 
  bind_rows(wulff_younger |> mutate(rating_normalized = (rating_original - 1) / 19)) |> 
  bind_rows(wulff_older |> mutate(rating_normalized = (rating_original - 1) / 19))

# Psycho-linguistic word norms -------------------------------------------------

# Data
angst <- read_xlsx("00_Cold_Storage/German_Norms/ANGST/13428_2013_426_MOESM1_ESM.xlsx") %>% 
  select(word = `G-word`, valence_angst = VAL_Mean, arousal_bawl = `ARO_Mean_(BAWL)`,
         arousal_anew = `ARO_Mean_(ANEW)`, dominance = DOM_Mean, 
         potency = POT_Mean, imageability = IMA_MEAN) %>% 
  distinct()
lang <- read_delim("00_Cold_Storage/German_Norms/Leipzig_Affective_Norms/Kanske-BRM-2010/LANG_database.txt",
                   delim = "\t", skip = 7, col_types = "cddddddiii") %>% 
  select(word, valence_lang = valence_mean, arousal = arousal_mean, 
         concreteness = concreteness_mean) %>% 
  distinct()
aoa <- read_csv("00_Cold_Storage/German_Norms/Age_of_Acquisition/13428_2016_718_MOESM1_ESM.csv") %>% 
  select(word = Word, aoa = AoAestimate) %>% 
  distinct()

# combine data, keep means for words with multiple ratings per property
psycholinguistic_ratings <- angst %>% 
  full_join(lang, by = "word") %>% 
  full_join(aoa, by = "word") %>% 
  group_by(word) %>% 
  summarize(across(everything(), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(across(everything(), ~ if_else(is.nan(.x), NA, .x)))

# n per measure
psycholinguistic_ratings %>% 
  summarize(across(everything(), ~ sum(!is.na(.x))))

# n per word
psycholinguistic_ratings %>% 
  group_by(word) %>% 
  summarize(across(everything(), ~ sum(!is.na(.x)))) %>% 
  rowwise() %>% 
  mutate(n_measures = sum(c_across(!word))) %>% 
  select(word, n_measures)

# Store tidy validation data ---------------------------------------------------

write_csv(lexical_decision_task, "00_Cold_Storage/German_Norms/lexical_decision_task.csv")
write_csv(relatedness_judgments, "00_Cold_Storage/German_Norms/relatedness_judgments.csv")
write_csv(psycholinguistic_ratings, "00_Cold_Storage/German_Norms/psycholinguistic_ratings.csv")


