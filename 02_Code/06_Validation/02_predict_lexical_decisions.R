# Dependencies -----------------------------------------------------------------

library(tidyverse)
library(cocor)

# Data -------------------------------------------------------------------------

# Lexical decision task
lexical_decision_task <- read_csv("00_Cold_Storage/German_Norms/lexical_decision_task.csv")

# SWOW-DE
swow_de <- read_csv("01_Data/Final/SWOW_DE_2025_R55.csv")
swow_de_long <- swow_de %>%
  select(-unknown_word, -starts_with("no_further_response")) %>%
  pivot_longer(
    cols = matches("response_corrected_|spelling_ok_|corrections_"),
    names_to = c(".value", "position"),
    names_pattern = "(.*)_(\\d+)"
  )
swow_response_counts <- swow_de_long |> 
  group_by(response_corrected) |> 
  summarize(swow_frequency = n()) |> 
  rename(word = response_corrected)

# SUBTLEX-DE
subtlex_de <- read_csv2("00_Cold_Storage/SUBTLEX-DE/SUBTLEX-DE.csv") |> 
  select(word = Word, subtlex_frequency = SUBTLEX)

# Find subset of validation data, that overlaps with SWOW-DE cues and SUBTLEX-DE
ldt_validation_set <- lexical_decision_task |>
  filter(is_word, word %in% unique(swow_response_counts$word), word %in% subtlex_de$word)

# Correlate LDT  ---------------------------------------------------------------

predictors <- ldt_validation_set |> 
  select(word) |> 
  distinct() |> 
  left_join(subtlex_de) |> 
  left_join(swow_response_counts)

ldt_validation <- ldt_validation_set |> 
  left_join(predictors)
ldt_cors <- tibble(
  age_group = unique(ldt_validation$age_group),
  swow_freq_cor = numeric(7),
  swow_freq_p = numeric(7),
  swow_freq_cil = numeric(7),
  swow_freq_ciu = numeric(7),
  subtlex_freq_cor = numeric(7),
  subtlex_freq_p = numeric(7),
  subtlex_freq_cil = numeric(7),
  subtlex_freq_ciu = numeric(7),
  diff_p = numeric(7)
)

for(ag in 1:length(ldt_cors$age_group)) {
  
  data <- ldt_validation |> 
    filter(age_group == ldt_cors$age_group[ag]) |> 
    select(reaction_time, subtlex_frequency, swow_frequency)
  
  test_swow <- cor.test(data$reaction_time, log10(data$swow_frequency))
  ldt_cors$swow_freq_cor[ag] <- test_swow$estimate
  ldt_cors$swow_freq_p[ag] <- test_swow$p.value
  ldt_cors$swow_freq_cil[ag] <- test_swow$conf.int[1]
  ldt_cors$swow_freq_ciu[ag] <- test_swow$conf.int[2]
  
  test_subtlex <- cor.test(data$reaction_time, log10(data$subtlex_frequency))
  ldt_cors$subtlex_freq_cor[ag] <- test_subtlex$estimate
  ldt_cors$subtlex_freq_p[ag] <- test_subtlex$p.value
  ldt_cors$subtlex_freq_cil[ag] <- test_subtlex$conf.int[1]
  ldt_cors$subtlex_freq_ciu[ag] <- test_subtlex$conf.int[2]
  
  ldt_cors$diff_p[ag] <- cocor.dep.groups.overlap(
    r.jk = unname(test_swow$estimate),
    r.jh = unname(test_subtlex$estimate),
    r.kh = cor(log10(data$swow_frequency), log10(data$subtlex_frequency)),
    n = length(na.omit(data$reaction_time)),
    test = "steiger1980"
  )@steiger1980$p.value
  
}

# test if all comparisons between swow and subtlex are significant
ldt_cors$diff_p < 0.001
max(ldt_cors$diff_p)

ldt_data <- ldt_cors |>  
  pivot_longer(cols = -c(age_group, diff_p),
               names_to = c("predictor", ".value"),
               names_pattern = "(.*)_([^_]+)$") |> 
  mutate(predictor = str_split_i(predictor, "_", 1)) |> 
  rename(r = cor, ci_lower = cil, ci_upper = ciu) |> 
  mutate(r = abs(r), ci_lower = abs(ci_lower), ci_upper = abs(ci_upper)) |> 
  mutate(predictor = toupper(predictor)) |> 
  mutate(age_group = case_when(
    age_group == "g1" ~ "Grade 1",
    age_group == "g2" ~ "Grade 2",
    age_group == "g3" ~ "Grade 3",
    age_group == "g4" ~ "Grade 4",
    age_group == "g6" ~ "Grade 6",
    age_group == "ya" ~ "Adults 20-30",
    age_group == "oa" ~ "Adults 65-75",
  )) |> 
  mutate(age_group = fct_relevel(age_group, "Adults 20-30", "Adults 65-75", after = Inf))

diff_p_data <- ldt_data |> 
  select(age_group, diff_p) |> 
  distinct() |> 
  mutate(p = if_else(diff_p < 0.001, "p < 0.001", paste0("p = ", round(diff_p, 3))))

ldt_data |> 
  mutate(predictor = if_else(predictor == "SWOW", "SWOW-DE", predictor)) %>% 
  ggplot(aes(x = predictor, y = r, fill = predictor)) +
  geom_col() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3, color = "grey50") +
  facet_wrap(~age_group, ncol = 7) +
  scale_fill_manual(values = c("SWOW-DE" = "#40498EFF", "SUBTLEX" = "grey75"), guide = "none") +
  scale_y_continuous(limits = c(0, 0.73)) +
  labs(x = "", y = expression("|" * r * "|")) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(color = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        text = element_text(family = "Helvetica")) +
  geom_text(data = diff_p_data, aes(label = p), x = 1.5, y = 0.72, 
            size = 2, inherit.aes = FALSE) +
  geom_segment(x = 1, xend = 2, y = 0.69, linewidth = 0.2) +
  geom_segment(y = 0.67, yend = 0.69, linewidth = 0.2)

ggsave("03_Plots/validation_ldt.png", width = 18, height = 8, units = "cm", bg = "white", scale = 1.25)

# comparison numbers for the text
options(pillar.sigfig = 5)
ldt_data %>% 
  group_by(age_group) %>% 
  summarize(predictor, perc_increase = ((r/min(r)) - 1) * 100) %>% 
  filter(predictor == "SWOW") %>% 
  arrange(perc_increase)

