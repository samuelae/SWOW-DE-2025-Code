# Dependencies -----------------------------------------------------------------

library(tidyverse)
library(zipfR)

# Read files -------------------------------------------------------------------

# swow-de
swow <- read_csv("01_Data/Final/SWOW_DE_2025_R55.csv")

# Analyze gerneral types and tokens --------------------------------------------

# number of cues 
swow %>% pull(cue) %>% unique() %>% length()

# number of responses total (tokens)
swow %>% 
  summarize(r1_text = sum(!is.na(response_corrected_1)),
            r2_text = sum(!is.na(response_corrected_2)),
            r3_text = sum(!is.na(response_corrected_3)),
            r123_text = sum(!is.na(response_corrected_1)) + sum(!is.na(response_corrected_2)) + sum(!is.na(response_corrected_3)))

# number of responses unique (types)
types <- swow %>% 
  select(cue, starts_with("response_corrected_")) %>% 
  pivot_longer(cols = starts_with("response_corrected_"), names_to = "position", values_to = "response") %>% 
  mutate(position = str_sub(position, -1)) %>% 
  count(response) %>% 
  arrange(desc(n)) %>% 
  drop_na()

n_types <- types %>% nrow()
n_types

oneoffs <- types %>% 
  filter(n == 1) %>% 
  pull(response)
length(oneoffs)

# percentage of types that are oneoffs
length(oneoffs) / n_types

tokens <- swow %>% 
  select(cue, starts_with("response_corrected_")) %>% 
  pivot_longer(cols = starts_with("response_corrected_"), names_to = "position", values_to = "response") %>% 
  mutate(position = str_sub(position, -1)) %>% 
  drop_na() %>% 
  pull(response)

# percentage of tokens that are oneoffs
length(oneoffs) / length(tokens)

# number of cues that appear as responses (and proportion of cues)
cues <- swow %>% pull(cue) %>% unique()
sum(types$response %in% cues)
sum(tokens %in% cues) / length(tokens)


# response distribution (zipf-like?)
types %>% 
  bind_cols(rank = 1:nrow(types)) %>% 
  mutate(zipf_mandelbrod = 1 / (rank + 2.7)^0.47) %>% 
  ggplot(aes(x = rank, y = n)) +
  geom_point() +
  geom_point(aes(y = zipf_mandelbrod * 5000), color = "red") +
  coord_cartesian(xlim = c(0, 10000))

