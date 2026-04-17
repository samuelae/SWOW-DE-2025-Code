# Dependencies -----------------------------------------------------------------

library(tidyverse)
library(igraph)
# library(parallel)

# source("02_Code/07_Networks/Functions/sample_swow_dataset.R")
source("02_Code/07_Networks/Functions/infer_network.r")
source("02_Code/07_Networks/Functions/analyze_network.r")

# Read files -------------------------------------------------------------------

# Combined, matched, and unified SWOW data
swow_combined <- read_csv("00_Cold_Storage/SWOW/Unified_Matched_Highest_Frequency/swow_combined.csv")

# Derive German sample size metrics --------------------------------------------

# Extract German data (use all)
swow_de <- swow_combined %>% filter(dataset == "de")

# number of cues
n_cues <- swow_de %>% 
  pull(cue) %>% 
  unique() %>% 
  length()

# number of responses per cue
n_responses <- swow_de %>% 
  count(cue) %>% 
  pull(n)

# Analyze networks -------------------------------------------------------------

nw_de <- infer_network(swow_combined %>% filter(dataset == "de"))
nw_metrics_de <- analyze_network(nw_de)

nw_en <- infer_network(swow_combined %>% filter(dataset == "en"))
nw_metrics_en <- analyze_network(nw_en)

nw_rp <- infer_network(swow_combined %>% filter(dataset == "rp"))
nw_metrics_rp <- analyze_network(nw_rp)

nw_zh <- infer_network(swow_combined %>% filter(dataset == "zh"))
nw_metrics_zh <- analyze_network(nw_zh)

nw_nl <- infer_network(swow_combined %>% filter(dataset == "nl"))
nw_metrics_nl <- analyze_network(nw_nl)

# Create table -----------------------------------------------------------------

options(pillar.sigfig = 5)
results <- bind_rows(nw_metrics_de, nw_metrics_nl, nw_metrics_en, nw_metrics_rp, nw_metrics_zh) %>% 
  mutate(dataset = c("de", "nl", "en", "rp", "zh"), .before = n_nodes) %>% 
  select(dataset, n_edges, avg_strength, aspl, avg_local_cc)
results

# Calculate percentage difference range
percentages <- results %>% 
  summarize(
    perc_n_edges = 1 - (min(n_edges) / max(n_edges)), 
    perc_avg_strength = 1 - (min(avg_strength) / max(avg_strength)),
    perc_aspl = 1 - (min(aspl) / max(aspl)),
    perc_avg_local_cc = 1 - (min(avg_local_cc) / max(avg_local_cc))
  )
max(percentages)

# Analyze Smallworldness -------------------------------------------------------

n = 5877 
m = 242330

# random network (erdös rény)
set.seed(1984)
g <- sample_gnm(n, m, directed = TRUE, loops = FALSE)
E(g)$weight = E(nw_de)$weight

analyze_network(g) # cc = 0.01, aspl = 108.39

# regular lattice
set.seed(1984)
g <- make_lattice(n, directed = TRUE, nei = 42)
E(g)$weight = sample(x = E(nw_de)$weight, size = ecount(g), replace = TRUE)

analyze_network(g) # cc = 0.74, aspl = 1688.06




