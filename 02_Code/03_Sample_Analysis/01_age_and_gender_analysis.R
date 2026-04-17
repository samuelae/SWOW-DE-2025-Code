# Dependencies -----------------------------------------------------------------

library(tidyverse)

# Read files -------------------------------------------------------------------

# swow-de
data <- read_csv("01_Data/Final/SWOW_DE_2025_R55.csv")

# German population age/gender
age_de <- read_delim("01_Data/Varia/Sample/12411-0013_00.csv", delim = ";", 
           skip = 4, locale = locale(encoding = "ISO-8859-1"), col_names = FALSE) 
names <- paste0(age_de[1, ], ".", age_de[2, ])
names[1:2] <- c("date", "age")
names(age_de) <- names
age_de <- age_de %>% slice(-c(1, 2)) 
age_de <- age_de %>% slice(-c(92:96))
age_de <- age_de %>% 
  pivot_longer(cols = !c(date, age)) %>% 
  separate_wider_delim(cols = name, delim = ".", names = c("state", "gender")) %>% 
  mutate(age = parse_number(age)) %>% 
  filter(gender %in% c("männlich", "weiblich")) %>% 
  mutate(gender = if_else(gender == "männlich", "male", "female")) %>% 
  select(age, state, gender, n = value) %>% 
  mutate(n = as.integer(n), 
         age = as.integer(age))

# Prepare descriptive analysis -------------------------------------------------

# distinct participants
participants <- data %>% 
  select(participant_id, age, gender, education, native_language, 
         latitude, longitude) %>% 
  distinct()

# Analyze Participants ---------------------------------------------------------

swow_age <- participants %>% 
  select(age, gender) %>% 
  mutate(age = if_else(age >= 90, NA, age)) %>% # germany data codes all ages > 90 as 90
  group_by(age, gender) %>% 
  count(.drop = FALSE)
pop_age <- age_de %>% 
  mutate(age = na_if(age, 90)) %>% # germany data codes all ages > 90 as 90
  group_by(age, gender) %>% 
  summarize(n = sum(n)) %>% 
  filter(age >= 16)
swow_sum <- swow_age$n %>% sum()
pop_sum <- pop_age$n %>% sum()
swow_age <- swow_age %>% 
  mutate(n_prop = n / swow_sum,
         group = "SWOW-DE")
pop_age <- pop_age %>% 
  mutate(n_prop = n / pop_sum,
         group = "Germany")
age_data <- swow_age %>% 
  bind_rows(pop_age)

# Combined plot
age_plot <- ggplot() +
  geom_vline(xintercept = 0) +
  geom_area(
    data = age_data %>% filter(group == "Germany", gender == "male"),
    aes(x = n_prop, y = age),
    color = "#413D7BFF",
    fill = "#413D7BFF",
    linewidth = 1,
    orientation = "y",
    alpha = 0.35
  ) +
  geom_area(
    data = age_data %>% filter(group == "Germany", gender == "female"),
    aes(x = -n_prop, y = age),
    color = "#348FA7FF",
    fill = "#348FA7FF",
    linewidth = 1,
    orientation = "y",
    alpha = 0.35
  ) +
  geom_col(
    data = age_data %>% filter(group == "SWOW-DE", gender == "male"),
    aes(x = n_prop, y = age),
    fill = "#413D7BFF",
    orientation = "y",
    alpha = 1
  ) +
  geom_col(
    data = age_data %>% filter(group == "SWOW-DE", gender == "female"),
    aes(x = -n_prop, y = age),
    fill = "#348FA7FF",
    orientation = "y",
    alpha = 1
  ) +
  scale_y_continuous(
    breaks = seq(20, 100, 10),
    minor_breaks = seq(15, 95, 10)
  ) +
  scale_x_continuous(limits = c(-0.05, 0.05)) +
  labs(y = "Age") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank()
  )
age_plot

ggsave("03_Plots/swow-de_age.pdf", plot = age_plot, bg = "white", units = "cm",
       width = 13, height = 15)

saveRDS(age_plot, "01_Data/Varia/Plotting/age_plot.rds")

# Get gender data for paper ----------------------------------------------------

total_p <- length(unique(data$participant_id))
total_p

options(pillar.sigfig = 5)

participants %>% 
  count(gender) %>% 
  mutate(percent = n / total_p * 100)

# Get age data for paper -------------------------------------------------------

options(pillar.sigfig = 5)

participants %>% 
  summarize(mean_age = mean(age, na.rm = TRUE),
            min_age = min(age, na.rm = TRUE),
            max_age = max(age, na.rm = TRUE), 
            sd_age = sd(age, na.rm = TRUE))

