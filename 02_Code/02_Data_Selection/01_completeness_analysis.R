# Dependencies -----------------------------------------------------------------

library(tidyverse)

# Read data --------------------------------------------------------------------

# Import full data set
data <- read_csv("01_Data/Final/SWOW_DE_2025_RAW.csv")

# Calculate AUC-style optimum number of cues -----------------------------------

response_counts <- data %>% 
  group_by(cue) %>% 
  count() %>% 
  ungroup() %>%
  mutate(cue = fct_reorder(cue, -n)) %>% 
  mutate(ccol = case_when(n >= 60 ~ "good",
                          n < 60 & n >= 55 ~ "ok",
                          n < 55 & n >= 50 ~ "minimal", 
                          n < 50 ~ "bad"))

n_60 <- sum(response_counts$n >= 60)
n_55 <- sum(response_counts$n >= 55)
n_50 <- sum(response_counts$n >= 50)

max_co = 0
max_auc = 0

for(co in 1:70) {
  
  auc <- sum(response_counts$n >= co) * co
  if(auc > max_auc) {
    max_auc <- auc
    max_co <- co
  }
  
}
max_co
max_percent <- max_auc / sum(response_counts$n)

plot_cut_off_analysis <- response_counts %>%
  ggplot(aes(x = cue, y = n, fill = ccol, color = ccol)) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "good" = "green",
      "ok" = "yellow",
      "minimal" = "orange",
      "bad" = "red"
    ),
    aesthetics = c("fill", "color"),
    guide = "none"
  ) +
  geom_hline(yintercept = 60, color = "grey40") +
  annotate("text", x = n_60, y = 62, label = paste(n_60, "60+"), hjust = 1) +
  geom_hline(yintercept = 55, color = "grey40") +
  annotate("text", x = n_55, y = 57, label = paste(n_55, "55+"), hjust = 1) +
  geom_hline(yintercept = 50, color = "grey40") +
  annotate("text", x = n_50, y = 52, label = paste(n_50, "50+"), hjust = 1) +
  coord_cartesian(ylim = c(0, 80)) +
  labs(title = paste("55 responses cutoff includes", 
                     paste0(round(max_percent * 100, 2), "%"), 
                     "of responses"), 
       subtitle = paste("A cut-off of", max_co, "maximizes AUC"),
       x = NULL) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave("03_Plots/cut_off_analysis.png", plot_cut_off_analysis, width = 20, 
       height = 12, units = "cm", bg = "white")

