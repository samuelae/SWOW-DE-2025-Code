library(hunspell)
library(rlang)

# Spellchecks ------------------------------------------------------------------

spellcheck_all_cases <- function(words, custom_words = NULL) {
  
  # create other casings
  lower <- str_to_lower(words)
  upper <- str_to_upper(words)
  sentence <- str_to_sentence(words)
  title <- str_to_title(words)
  
  # check
  if(is.null(custom_words)) {
    check_original <- hunspell_check(words, dict = "de_DE")
    check_lower <- hunspell_check(lower, dict = "de_DE")
    check_upper <- hunspell_check(upper, dict = "de_DE")
    check_sentence <- hunspell_check(sentence, dict = "de_DE")
    check_title <- hunspell_check(title, dict = "de_DE")
  } else {
    check_original <- hunspell_check(words, dict = dictionary(lang = "de_DE", add_words = custom_words))
    check_lower <- hunspell_check(lower, dict = dictionary(lang = "de_DE", add_words = custom_words))
    check_upper <- hunspell_check(upper, dict = dictionary(lang = "de_DE", add_words = custom_words))
    check_sentence <- hunspell_check(sentence, dict = dictionary(lang = "de_DE", add_words = custom_words))
    check_title <- hunspell_check(title, dict = dictionary(lang = "de_DE", add_words = custom_words))
  }
  
  # output
  check_original | check_lower | check_upper | check_sentence | check_title
  
}

# Split multiple words to check individually -----------------------------------

spellcheck_multiword <- function(x, custom_words = NULL) {
  
  string_vectors <- str_split(x, " ")
  if(is.null(custom_words)) {
    map_lgl(string_vectors, \(x) all(hunspell_check(x, dict = "de_DE")))
  } else {
    map_lgl(string_vectors, \(x) all(hunspell_check(x, dict = dictionary(lang = "de_DE", add_words = custom_words))))
  }
  
}

# Optimized performance for long lists -----------------------------------------

spellcheck_multiword_optimized <- function(x, custom_words = NULL) {
  
  string_vectors <- str_split(x, " ")
  
  if (is.null(custom_words)) {
    
    map_lgl(string_vectors, \(x) all(hunspell_check(x, dict = "de_DE")), .progress = TRUE)
    
  } else {
    
    results <- tibble(id = seq_along(string_vectors),
                      spelling_ok_std = NA,
                      spelling_ok_custom = NA)
    
    
    results$spelling_ok_std <- map_lgl(string_vectors, \(x) all(hunspell_check(x, dict = "de_DE")), .progress = "Standard spellcheck (1/2)")
    
    ids_custom <- results$id[!results$spelling_ok_std & !is.na(results$spelling_ok_std)]
    string_vectors_custom <- string_vectors[!results$spelling_ok_std & !is.na(results$spelling_ok_std)]
    
    results$spelling_ok_custom[ids_custom] <- map_lgl(string_vectors_custom, \(x) all(hunspell_check(x, dict = dictionary(lang = "de_DE", add_words = custom_words))), .progress = "Custom words spellcheck (2/2)")
    
    results$spelling_ok_std | results$spelling_ok_custom
    
  }
  
}



