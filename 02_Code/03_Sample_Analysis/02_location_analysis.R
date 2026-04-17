# Dependencies -----------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(rnaturalearth)

# Read files -------------------------------------------------------------------

# swow-de
data <- read_csv("01_Data/Final/SWOW_DE_2025_R55.csv")

# Prepare descriptive analysis -------------------------------------------------

# distinct participants
participants <- data %>% 
  select(participant_id, age, gender, education, native_language, 
         latitude, longitude) %>% 
  distinct() %>% 
  mutate(p_language_label = case_when(native_language == "German (Austria)" ~ "Austria",
                                      native_language == "German (Belgium)" ~ "Belgium",
                                      native_language == "German (Germany)" ~ "Germany",
                                      native_language == "German (Italy)" ~ "Italy",
                                      native_language == "German (Luxembourg)" ~ "Luxembourg",
                                      native_language == "German (Switzerland / Lichtenstein)" ~ "Switzerland & LI")) %>% 
  filter(!is.na(native_language))

# Plot location ----------------------------------------------------------------

# Get the map data for Europe
world <- ne_countries(scale = "large", returnclass = "sf")

location_plot <- ggplot() +
  geom_sf(data = world, linewidth = 0.5, fill = "grey95") +
  geom_point(data = participants, 
             aes(y = latitude, x = longitude, colour = p_language_label), 
             alpha = 0.5, size = 0.75) +
  scale_color_viridis_d() +
  geom_sf_text(data = world, aes(label = iso_a2), color = "grey5", 
               fontface = "bold", size = 4) +
  # coord_sf(xlim = c(4, 19), ylim = c(45.5, 56.5)) +
  coord_sf(xlim = c(3.5, 19.5), ylim = c(45.5, 56.5)) +
  theme_minimal() +
  guides(colour = guide_legend(title = "German Variant", 
                               position = "bottom",
                               override.aes = list(size = 2, alpha = 0.75))) +
  theme(panel.background = element_rect(fill = "#CDE4EB"),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title.position = "left",
        legend.title = element_text(size = 10, margin = margin(r = 10)),
        legend.text = element_text(size = 8, margin = margin(l = 3)),
        legend.key.spacing.y = unit(0, "pt"),
        legend.box.spacing = unit(0, "pt"))
location_plot

# combine with age distribution plot
age_plot <- readRDS("01_Data/Varia/Plotting/age_plot.rds")

demographics_plot <- age_plot + 
  location_plot + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(widths = c(0.45, 0.55))

ggsave("03_Plots/swow-de_demographics.png", demographics_plot, width = 24, 
       height = 14, bg = "white", units = "cm", dpi = 300)

# Get native language data for paper -------------------------------------------

participants <- data %>% 
  select(participant_id, age, gender, education, native_language, 
         latitude, longitude) %>% 
  distinct()

options(pillar.sigfig = 5)

total_p <- length(unique(data$participant_id))
total_p

participants %>% 
  count(native_language) %>% 
  mutate(percecnt = n / total_p * 100) %>% 
  arrange(desc(n)) %>% 
  mutate(german_native = if_else(is.na(native_language), FALSE, TRUE)) %>% 
  group_by(german_native) %>% 
  summarize(n = sum(n), 
            percent = sum(n) / total_p * 100)

participants %>% 
  count(native_language) %>% 
  mutate(percecnt = n / total_p * 100) %>% 
  arrange(desc(n)) %>% 
  mutate(german_native = if_else(is.na(native_language), FALSE, TRUE))



