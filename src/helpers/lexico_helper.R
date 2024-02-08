has_words <- function(sentence, words) {
  # Check if any of the words are in the sentence
  test <- sapply(words, function(word) grepl(paste0("\\b", word, "\\b"), sentence))
  # Return TRUE if any word is found, FALSE otherwise
  return(any(test))
}

count_digits <- function(text) {
  # Utilisation d'une expression régulière pour trouver les chiffres dans la phrase
  digit_pattern <- "\\b\\d+\\b"
  # Compter le nombre d'occurrences de chiffres
  count <- sum(grepl(digit_pattern, text))
  return(count)
}

count_words <- function(text){
  return(str_count(text, "\\w+"))
}
