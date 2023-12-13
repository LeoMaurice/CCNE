president_CCNE_by_date <- Vectorize(function(date){
  # Définir l'ordre des niveaux du facteur
  ordre_presidents <- c("Bernard (83-91)", "Changeux (92-99)", "Sicard (00-08)", 
                        "Grimfeld (09-11)", "Ameisen (12-15)", "Delfraissy (16-)")
  
  # Définir le facteur avec les niveaux dans l'ordre spécifié
  facteur_presidents <- factor(ordre_presidents, levels = ordre_presidents)
  
  if(date>=as.Date("2016-12-01"))
    return(facteur_presidents[6]) #Jean-François Delfraissy
  else if(date >= as.Date("2012-01-01"))
    return(facteur_presidents[5]) #Jean Claude Ameisen
  else if(date >= as.Date("2008-01-01"))
    return(facteur_presidents[4]) #Alain Grimfeld
  else if(date >= as.Date("1999-01-01"))
    return(facteur_presidents[3]) #Didier Sicard
  else if(date >= as.Date("1992-01-01"))
    return(facteur_presidents[2]) #Jean-Pierre Changeux
  else
    return(facteur_presidents[1]) #Jean Bernard
})