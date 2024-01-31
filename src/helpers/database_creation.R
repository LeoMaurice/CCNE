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

open_avis <- function(rescrap_texte=FALSE){
  if(rescrap_texte){
    # Définir le chemin vers le dossier contenant les fichiers PDF
    dossier_avis <- "../data/raw/avis"
    
    # Liste des fichiers PDF dans le dossier
    liste_fichiers <- list.files(path = dossier_avis, pattern = "\\.pdf$", full.names = TRUE)
    
    # Initialiser un data.frame vide
    base_avis_ccne <- data.frame(num = integer(), avis = character(), 
                                 nom_fichier = character(), nb_pages = integer(),
                                 stringsAsFactors = FALSE)
    
    # Boucle pour lire chaque fichier PDF et extraire le texte
    for (fichier in liste_fichiers) {
      # Extraire le numéro du fichier
      numero <- as.integer(strsplit(strsplit(basename(fichier), " ")[[1]][2], ".pdf")[[1]])
      list_bugged <- c(69,70,71,72,76,77,78,79,80,83,84,86,89)
      if(numero %in% list_bugged){
        texte <- pdf_ocr_text(fichier) # embending de tesseract
      } else{
        texte <- pdf_text(fichier)
      }
      
      
      nombre_page <- pdf_info(fichier)$pages
      
      texte <- paste(texte, collapse = " ")
      
      # Ajouter les données au data.frame
      base_avis_ccne <- base_avis_ccne |>
        bind_rows(data.frame(num = numero, 
                             avis = texte,
                             nom_fichier = basename(fichier),
                             nb_pages = nombre_page,
                             stringsAsFactors = FALSE))
    }
    saveRDS(base_avis_ccne, "../data/intermediate/base_avis_ccne.rds")
  } else {
    readRDS("../data/intermediate/base_avis_ccne.rds") -> base_avis_ccne
  }
  return(base_avis_ccne)
}

join_metadata - function(base_avis_ccne){
  list_saisine_obligatoire <- c(
    "PR", #président
    "PM", #Premier Minsitre
    "MS", #ministère et secrétariat santé et autres appelations (solidarité, affaire social)
    "DGS", #direction générale de la santé
    "Parlement", #président d'AN et/ou sénat
    "Gouvernement", #autres membres du gouvernement
    "EP", #Etablissement public
    "EPES", #Etablissement public d'enseignement supérieur
    "CNRS",
    "INSERM",
    "IGAS", #inspection des affaires sociales = doute,
    "MILDT" # Mission Interministérielle de luttes contre la dépendance et la toxicomanie
  )
  
  base_avis_ccne |>
    left_join(read_excel("../data/raw/collected_metadata/metadata_avis.xlsx", 
                         col_types = c("numeric", "date", "text", 
                                       "text", "text", "text", "logical", 
                                       "text", "text", "text", "date", "logical", 
                                       "text", "logical", "text"))|>
                mutate(saisine = saisine_precise %in% list_saisine_obligatoire,
                       date = as.Date(date)
                )|>
                select(num,saisine,rapporteurs, membres_gt,titre_court,titre,date,theme,divergence))|>
    mutate(nb_mots = str_count(avis, "\\w+"),
           president = sapply(date, president_CCNE_by_date))-> base_avis_ccne
  
  base_avis_ccne|>
    rename(Titre = titre,
           Date = date)|>
    mutate(Annee =year(Date),
           theme = as.factor(theme)) -> base_avis_ccne
  
  return(base_avis_ccne)
}