# calculates zipf value as proposed by 
zipf_value <- function(count, total_words, types) {
  log10((count + 1) / ((total_words / 1000000) + (types / 1000000))) + 3
}
