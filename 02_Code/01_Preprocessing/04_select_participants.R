# Dependencies -----------------------------------------------------------------

library(tidyverse)
library(patchwork)

# Read files -------------------------------------------------------------------

data <- read_csv("01_Data/Raw/02_data.csv")
participant_scores <- read_csv("01_Data/Raw/03_participant_scores.csv") %>% 
  left_join(data %>% select(id_p, age) %>% distinct())
cues <- read_csv("01_Data/Raw/02_cues.csv")

# Remove participants who do not meet age criterion ----------------------------

criterion_age <- 16

participants_to_include <- participant_scores %>% 
  filter(age >= criterion_age) %>% 
  pull(id_p)

# remove youg participants from data
data <- data %>% filter(id_p %in% participants_to_include)

# remove young participants from participant criteria (for correct percentages)
participant_scores <- participant_scores %>% filter(id_p %in% participants_to_include)

# Define data quality criteria -------------------------------------------------

criterion_n_gram <- 0.3
criterion_unique_responses <- 0.8
criterion_spelling <- 0.6 
criterion_unknown_or_no_further_response <- 0.6

# Analyze participant score distributions --------------------------------------

# Distributions
p_total <- nrow(participant_scores)
r_total <- nrow(data)

# N-gram
id_p_score <- participant_scores %>% filter(score_n_gram < criterion_n_gram) %>% pull(id_p)
p <- length(id_p_score)
r <- nrow(data %>% filter(id_p %in% id_p_score))

plot_n_gram <- participant_scores %>% 
  ggplot(aes(x = score_n_gram)) +
  geom_histogram(aes(fill = score_n_gram < criterion_n_gram), bins = 50) +
  scale_fill_manual(values = c("darkred", "darkgreen"), 
                    name = paste("<", criterion_n_gram)) +
  labs(title = "N-gram (proportion n > 1)", x = "", 
       subtitle = paste0("Participants: ", p, "/", p_total, " (", 
                         sprintf("%.2f", ((p / p_total) * 100)), "%)", " | ",
                         "Responses: ", r, "/", r_total, " (", 
                         sprintf("%.2f", ((r / r_total) * 100)), "%)")) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 1)) +
  theme(text = element_text(family = "Helvetica"))

# Unique responses
id_p_score <- participant_scores %>% filter(score_unique_responses > criterion_unique_responses) %>% pull(id_p)
p <- length(id_p_score)
r <- nrow(data %>% filter(id_p %in% id_p_score))

plot_unique_responses <- participant_scores %>% 
  ggplot(aes(x = score_unique_responses)) +
  geom_histogram(aes(fill = score_unique_responses > criterion_unique_responses), bins = 50) +
  scale_fill_manual(values = c("darkred", "darkgreen"), 
                    name = paste(">", criterion_unique_responses))  +
  labs(title = "Unique responses (proportion unique)", x = "", 
       subtitle = paste0("Participants: ", p, "/", p_total, " (", 
                         sprintf("%.2f", ((p / p_total) * 100)), "%)", " | ",
                         "Responses: ", r, "/", r_total, " (", 
                         sprintf("%.2f", ((r / r_total) * 100)), "%)")) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 1)) +
  theme(text = element_text(family = "Helvetica"))

# Spelling
id_p_score <- participant_scores %>% filter(score_spelling > criterion_spelling) %>% pull(id_p)
p <- length(id_p_score)
r <- nrow(data %>% filter(id_p %in% id_p_score))

plot_spelling <- participant_scores %>% 
  ggplot(aes(x = score_spelling)) +
  geom_histogram(aes(fill = score_spelling > criterion_spelling), bins = 50) +
  scale_fill_manual(values = c("darkred", "darkgreen"), 
                    name = paste(">", criterion_spelling)) +
  labs(title = "Spelling (proportion hunspell check passed)", x = "", 
       subtitle = paste0("Participants: ", p, "/", p_total, " (", 
                         sprintf("%.2f", ((p / p_total) * 100)), "%)", " | ",
                         "Responses: ", r, "/", r_total, " (", 
                         sprintf("%.2f", ((r / r_total) * 100)), "%)")) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 1)) +
  theme(text = element_text(family = "Helvetica"))

# Unknown word & no further response
id_p_score <- participant_scores %>% filter(score_unknown_or_no_further_response < criterion_unknown_or_no_further_response) %>% pull(id_p)
p <- length(id_p_score)
r <- nrow(data %>% filter(id_p %in% id_p_score))

plot_unknown_or_no_further_response <- participant_scores %>% 
  ggplot(aes(x = score_unknown_or_no_further_response)) +
  geom_histogram(aes(fill = score_unknown_or_no_further_response < criterion_unknown_or_no_further_response), bins = 50) +
  scale_fill_manual(values = c("darkred", "darkgreen"), 
                    name = paste("<", criterion_unknown_or_no_further_response)) +
  labs(title = "Unknown word or missing response (proportion unknown | missing)", x = "", 
       subtitle = paste0("Participants: ", p, "/", p_total, " (", 
                         sprintf("%.2f", ((p / p_total) * 100)), "%)", " | ",
                         "Responses: ", r, "/", r_total, " (", 
                         sprintf("%.2f", ((r / r_total) * 100)), "%)")) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 1)) +
  theme(text = element_text(family = "Helvetica"))


# Export plot for paper --------------------------------------------------------

plot_distributions <- plot_n_gram + plot_unique_responses + plot_spelling + 
  plot_unknown_or_no_further_response & plot_layout(ncol = 1)

ggsave("03_Plots/participant_inclusion.png", plot_distributions, width = 16, 
       height = 30, units = "cm", bg = "white")


# Select participants based on quality scores ----------------------------------

participants_to_include <- participant_scores %>% 
  filter(score_n_gram < criterion_n_gram,
         score_unique_responses > criterion_unique_responses,
         score_spelling > criterion_spelling,
         score_unknown_or_no_further_response < criterion_unknown_or_no_further_response) %>% 
  pull(id_p)

p <- length(participants_to_include)
r <- nrow(data %>% filter(id_p %in% participants_to_include))

data %>% 
  filter(id_p %in% participants_to_include) %>% 
  count(cue) %>% 
  left_join(cues %>% select(cue, section) %>% distinct(), by = "cue") %>% 
  mutate(section = if_else(section == "set_2011", "set1", section)) %>% 
  ggplot(aes(x = fct_reorder(cue, desc(n)), y = n)) +
  geom_col(aes(fill = section, color = section)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  geom_hline(yintercept = 60, color = "grey40") +
  geom_hline(yintercept = 55, color = "grey40", linetype = "dashed") +
  geom_hline(yintercept = 50, color = "grey40", linetype = "dotted") +
  labs(title = "Respondents per cue after participant selection", x = "Cue", y = "Number of respondents",
       subtitle = paste0("Participants: ", p, "/", p_total, " (", 
                         sprintf("%.2f", ((p / p_total) * 100)), "%)", " | ",
                         "Responses: ", r, "/", r_total, " (", 
                         sprintf("%.2f", ((r / r_total) * 100)), "%)")) +
  theme(axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Helvetica")) +
  coord_cartesian(ylim = c(0, 70))

ggsave("03_Plots/participant_inclusion_cue_coverage.png", width = 18, 
       height = 10, units = "cm", bg = "white")


# Save selected data to disk ---------------------------------------------------

write_csv(data %>% filter(id_p %in% participants_to_include), "01_Data/Raw/04_data.csv")
