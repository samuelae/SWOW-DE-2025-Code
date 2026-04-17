library(hunspell)
library(stringr)

# Individual issues ------------------------------------------------------------

gen_sz <- function(word) {
  
  more_sz <- str_replace_all(word, "(?<!^)ss", "ß")
  less_sz <- str_replace_all(word, "ß", "ss")
  
  unique(c(word, more_sz, less_sz))

}

gen_uml <- function(word) {

  more_ä <- str_replace_all(word, "ae", "ä")
  more_Ä <- str_replace_all(word, "Ae", "Ä")
  more_ö <- str_replace_all(word, "oe", "ö")
  more_Ö <- str_replace_all(word, "Oe", "Ö")
  more_ü <- str_replace_all(word, "ue", "ü")
  more_Ü <- str_replace_all(word, "Ue", "Ü")
  
  unique(c(word, more_ä, more_Ä, more_ö, more_Ö, more_ü, more_Ü))
  
}

gen_casing <- function(word) {
  
  sentence <- str_to_sentence(word)
  lower <- str_to_lower(word)
  upper <- str_to_upper(word)
  
  unique(c(word, sentence, lower, upper))
  
}

# Combinations -----------------------------------------------------------------

generate_variants <- function(word) {

  # generate all combinations
  variants <- word
  variants <- lapply(variants, gen_sz) %>% unlist() %>% unique()
  variants <- lapply(variants, gen_uml) %>% unlist() %>% unique()
  variants <- lapply(variants, gen_casing) %>% unlist() %>% unique()
  
  # find correctly spelled variants
  viable_variants <- variants[hunspell_check(variants, dict = "de_DE")]
  
  # any variants viable?
  if(length(viable_variants) > 0) {
    # there is a viable variant
    if(length(viable_variants) > 1) {
      # more than one viable variants
      if(length(viable_variants[viable_variants != str_to_upper(viable_variants)]) == 1) {
        # thake the variant that is not all upper case
        result <- viable_variants[viable_variants != str_to_upper(viable_variants)][1]
      } else {
        # take the first variant
        result <- viable_variants[1]
      }
    } else {
      # only one viable variant, take this one
      result <- viable_variants[1]
    }
  } else {
    # no improvement, keep word as is
    result <- word
  }
  
  return(result)

}

# Multiple words ---------------------------------------------------------------

improve_spelling <- function(string) {
  
  # split into words
  words <- str_split(string, " ", simplify = TRUE)
  
  # for each phrase that is not already ok, improve casing
  words <- map_chr(words, generate_variants)
  
  # output as one string
  exec(paste, !!!words)
  
}







