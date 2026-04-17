sample_cues <- function(cues, n) {
  sample(cues, size = n)
}

sample_responses <- function(data, cue, n_responses) {
  c <- cue
  data %>% 
    filter(cue == c) %>% 
    slice_sample(n = n_responses)
}

sample_swow_dataset <- function(data, n_cues, n_responses) {
  
  # Sample SWOW dataset in specific size
  # @param data tibble of swow data
  # @param n_cues integer scalar
  # @param n_responses integer vector of length n_cues
  # @return tibble of with sum(n_responses) rows
  
  # Sample cues
  cues <- sample_cues(unique(data$cue), n = n_cues)
  
  # Sample responses 
  responses <- map2(cues, n_responses, \(x, y) sample_responses(data, x, y))
  
  # Combine into sample data
  data_sample <- bind_rows(responses)
  
  data_sample
  
}
