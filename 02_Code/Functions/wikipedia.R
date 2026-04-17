library(httr2)
library(xml2)
library(tidyverse)

# https://api.wikimedia.org/wiki/Rate_limits

# cannot handle "#" in strings

check_wikipedia_entry <- function(words) {
  
  tryCatch({
    
    # Encode each word for URL and join them with a pipe '|'
    encoded_words <- sapply(words, URLencode, USE.NAMES = FALSE)
    query_string <- paste(encoded_words, collapse = "|")
    
    # URL with query parameters
    url <- paste0("https://de.wikipedia.org/w/api.php?action=query&titles=", query_string, "&format=json")
    
    # Make the GET request to the Wikipedia API
    response <- request(url) %>% req_perform()  # does not always return json (why??)
    
    # Parse the JSON response
    content <- resp_body_json(response)
    
    # Compile list of normalized titles
    from <- character()
    to <- character()
    for(i in 1:length(content$query$normalized)) {
      from[i] <- content$query$normalized[[i]]$from
      to[i] <- content$query$normalized[[i]]$to
    }
    
    # Compile list of page_ids
    title <- character()
    wikipedia_page <- logical()
    page_id <- integer()
    for(i in 1:length(content$query$pages)) {
      title[i] <- content$query$pages[[i]]$title
      wikipedia_page[i] <- if_else(!is.null(content$query$pages[[i]]$pageid), TRUE, FALSE)
      if (wikipedia_page[i]) {
        page_id[i] <- content$query$pages[[i]]$pageid
      }
    }
    
    # Out
    output <- tibble(title, wikipedia_page, page_id) %>% 
      left_join(tibble(from, to), by = c("title" = "to")) %>% 
      mutate(word = if_else(is.na(from), title, from)) %>% 
      mutate(title = if_else(wikipedia_page, title, NA)) %>% 
      select(word, wikipedia_page, page_id, title)
    
    return(output)
    
  }, 
  error = function(e) {
    cat("An error occurred:\n", e$message, "\n")
    return(NA)  
  })
  
}


