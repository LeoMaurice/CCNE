"
Collection of functions that produce markdown reports of LDA topic models created with
the library topicmodels
To be used with kntir rmarkdown.
v 0.2
samuel.coavoux@gmail.com
copié depuis la thèse
"

report_topics <- function(lda,nb=20) {
  ## Function to create a markdown report of topics
  if(!(class(lda) %in% c("LDA_Gibbs","LDA_VEM"))) stop("Not a LDA model")
  for (i in 1:length(unique(topics(lda)))) {
    cat("**Topic ",i,"** : ",sep="")
    cat(terms(lda,nb)[,i],sep=", ")
    cat("\n\n")
  } 
}



report_topics_table <- function(lda,nb=20,caption=paste(nb,"termes associés aux topics")) {
  ## Function to create a markdown report of topics
  require(pander)
  if(!(class(lda) %in% c("LDA_Gibbs","LDA_VEM"))) stop("Not a LDA model")
  
  t <- t(terms(lda,nb))
  args <- c(as.data.frame(t(terms(lda,nb))),sep=", ")
  r <- do.call(paste,args)
  r <- as.data.frame(r)
  row.names(r) <- row.names(t)
  knitr::kable(r, caption = caption)
  
}

sig_text <- function(lda,corpus,nb=1,topic_range=1:length(unique(topics(lda)))) {
  ## Report the nb most significant texts in the corpus for each topic.
  ## Significance is determined by highest posteriori probability
  if(!(class(lda) %in% c("LDA_Gibbs","LDA_VEM"))) stop("Not a LDA model")
  
  for(i in topic_range) {
    p <- posterior(lda)$topics[,i]
    names(p) <- 1:length(p)
    q <- sort(p,decreasing = TRUE)[1:nb]
    
    cat("**Topic ",i,"** :\n\n",sep="")  
    
    for(j in 1:nb) {
      cat(as.character(corpus[as.numeric(names(q)[j])]))
      cat(" *(posterior : ",q[j],")*",sep="")
      cat("\n\n")
    }
  } 
}

## https://gist.github.com/trinker/477d7ae65ff6ca73cace
topicmodels2LDAvis <- function(x, reorder.topics = FALSE, ...){
  print(reorder.topics)
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE),
    reorder.topics = FALSE
  )
}


stm2LDAvis <- function(stm_model) {
  # Extract number of topics from the STM model
  num_topics <- stm_model$K
  
  # Extract topic-term matrix
  topic_term_matrix <- as.matrix(terms(stm_model, K = num_topics))
  
  # Extract document-topic matrix
  document_topic_matrix <- as.data.frame(theta(stm_model))
  
  # Create JSON for LDAvis
  json_data <- createJSON(
    lambda = topic_term_matrix,
    theta = document_topic_matrix,
    doc.labels = rownames(document_topic_matrix),
    vocab = colnames(topic_term_matrix),
    term.frequency = rowSums(topic_term_matrix),
    plot.opts = list(width = 800, height = 400)
  )
  
  return(json_data)
}

