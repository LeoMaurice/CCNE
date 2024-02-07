has_words <- function(sentence, words) {
  # Check if any of the words are in the sentence
  test <- sapply(words, function(word) grepl(paste0("\\b", word, "\\b"), sentence))
  # Return TRUE if any word is found, FALSE otherwise
  return(any(test))
}