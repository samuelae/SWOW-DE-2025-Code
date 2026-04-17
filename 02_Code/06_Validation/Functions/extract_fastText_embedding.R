extract_fastText_embedding <- function(file, words) {
  
  # this function reads a file as published by fastText line by 
  # line and extracts a matrix for a given set of words of interest 
  # (case insensitive matching, original casing as in file as output)
  
  words <- tolower(words)
  con <- file(file, "r")
  vector_list <- list()
  i <- 0
  while(TRUE) {
    line <- readLines(con, n = 1)
    if(length(line) == 0) {
      break
    }
    linevector <- stringi::stri_split_fixed(line, " ")[[1]]
    if(tryCatch(expr = tolower(linevector[1]) %in% words, error = function(e) FALSE)) {
      i <- i + 1
      vector_list[[i]] <- linevector
    }
  }
  close(con)
  
  # make matrix from list
  mat <- do.call(rbind, vector_list)
  
  # move word to names and change type to numeric
  rownames(mat) <- mat[, 1]
  mat <- mat[, -1]
  mode(mat) <- "numeric"
  
  return(mat)
}