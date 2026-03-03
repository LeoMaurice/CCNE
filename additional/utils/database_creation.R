# last modification
# 23/01/2026
library(pdftools)
library(tidyverse)
library(readxl)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(tesseract)
library(udpipe)

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

president_CCNE_by_year <- Vectorize(function(year){
  # Définir l'ordre des niveaux du facteur
  ordre_presidents <- c("Bernard (83-91)", "Changeux (92-99)", "Sicard (00-08)", 
                        "Grimfeld (09-11)", "Ameisen (12-15)", "Delfraissy (16-)")
  
  # Définir le facteur avec les niveaux dans l'ordre spécifié
  facteur_presidents <- factor(ordre_presidents, levels = ordre_presidents)
  
  if(year>=2016)
    return(facteur_presidents[6]) #Jean-François Delfraissy
  else if(year >= 2012)
    return(facteur_presidents[5]) #Jean Claude Ameisen
  else if(year >= 2008)
    return(facteur_presidents[4]) #Alain Grimfeld
  else if(year >= 1999)
    return(facteur_presidents[3]) #Didier Sicard
  else if(year >= 1992)
    return(facteur_presidents[2]) #Jean-Pierre Changeux
  else
    return(facteur_presidents[1]) #Jean Bernard
})

open_avis <- function(rescrap_texte=FALSE, lemmatized = TRUE){
  if(rescrap_texte){
    # Définir le chemin vers le dossier contenant les fichiers PDF
    dossier_avis <- "./data/raw/avis"
    
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
    saveRDS(base_avis_ccne, "./data/intermediate/base_avis_ccne_additionnal.rds")
  } else {
    if(lemmatized){
      lemma <- lemmatization()
      
      base_avis_ccne <- readRDS("~/Projets - Code/CCNE/data/intermediate/base_avis_ccne_additionnal.rds") %>%
        left_join(
          lemma %>%
            mutate(num = as.integer(doc_id)) %>%
            group_by(num) %>%
            summarise(lemma = paste(lemma, collapse = " ")),
          by = "num"
        )%>%
        mutate(avis_original = avis,
               avis = lemma)
    } else {
      base_avis_ccne <- readRDS("~/Projets - Code/CCNE/data/intermediate/base_avis_ccne_additionnal.rds")
    }
  }
  return(base_avis_ccne)
}

open_corrected_avis <- function(rescrap_texte=TRUE){
  if(rescrap_texte){
    # Définir le chemin vers le dossier contenant les fichiers PDF
    dossier_avis <- "./data/raw/avis_corrected"
    
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
      print(numero)
      texte <- pdf_text(fichier)
      
      
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
    saveRDS(base_avis_ccne, "./data/intermediate/base_corrected_avis_ccne.rds")
  } else {
    readRDS("./data/intermediate/base_corrected_avis_ccne.rds") -> base_avis_ccne
  }
  return(base_avis_ccne)
}

open_metadata <- function(){
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
  
  return(read_excel("./data/raw/collected_metadata/metadata_avis.xlsx", 
                    col_types = c("numeric", "date", "text", 
                                  "text", "text", "text", 
                                  "logical", "text", "text", 
                                  "text", "date", "logical", 
                                  "text", "logical", "logical",
                                  "text","text"))|>
           mutate(saisine = saisine_precise %in% list_saisine_obligatoire,
                  date = as.Date(date)
           )|>
           select(num,saisine,rapporteurs, membres_gt,titre_court,titre,date,theme,divergence, citations_autres_avis))
}

join_metadata <- function(base_avis_ccne){
  
  
  base_avis_ccne |>
    left_join(open_metadata())|>
    mutate(nb_mots = str_count(avis, "\\w+"),
           president = sapply(date, president_CCNE_by_date))-> base_avis_ccne
  
  base_avis_ccne|>
    rename(Titre = titre,
           Date = date)|>
    mutate(Annee =year(Date),
           theme = as.factor(theme)) -> base_avis_ccne
  
  return(base_avis_ccne)
}

is_president_CCNE <- function(nomination) {
  components <- strsplit(nomination, ";")[[1]]
  "PR" %in% components
}

remove_date <- function(base_avis_ccne) {
  # numbers and dates tokenisation
  
  number_pattern <- "\\b\\d+\\b"
  
  # Date pattern
  date_pattern <- "\\b(?:\\d{4}-\\d{2}-\\d{2}|\\d{2}-\\d{2}-\\d{4}|\\d{4}-\\d{2}|\\d{2}-\\d{4}|\\d{2}-\\d{2})\\b"
  
  
  # Replace numbers and dates with labels
  base_avis_ccne$avis <- base_avis_ccne$avis |>
    gsub(pattern = date_pattern, replacement = "date")
  return(base_avis_ccne)
}

remove_ponctuation_saut <- function(base_avis_ccne){
  base_avis_ccne$avis <- base_avis_ccne$avis|>
    str_replace_all("\\s+", " ") |>
    str_replace_all("[']", " ") |>
    str_replace_all("[’]", " ") |>
    str_replace_all("[`]", " ") |>
    str_replace_all("\r", " ") |>
    str_replace_all("\n", " ") |>
    str_replace_all("[,]",".") # pour certains chiffres
  
  base_avis_ccne$Titre <- base_avis_ccne$Titre|>
    str_replace_all("\r", " ") |>
    str_replace_all("\n", " ")
  
  return(base_avis_ccne)
}

tokenisation_custom <- function(base_avis_ccne){
  cp <- corpus(base_avis_ccne$avis, 
               docvars = base_avis_ccne |> select(num,
                                                  Titre, Annee, Date, president) |> as.data.frame(), 
               docnames = base_avis_ccne$num)
  # tokenisation
  tk_original <- tokens(cp, remove_punct = TRUE, remove_numbers = TRUE)
  
  # equivalence between differents words (or multiples words)
  important_expressions <- dictionary(list(
    vntr = c("variable number of tandem repeat", "variable number tandem repeat"),
    ccne = c("comité national consultatif d éthique",
             "comité national d éthique",
             "comité consultatif d éthique",
             "comité d éthique",
             "comité consultatif national d ethique",
             "comité national consultatif d ethique",
             "comité national d ethique",
             "comité consultatif d ethique",
             "comité d ethique",
             "comité consultatif national d ethique",
             "comit é national consultatif d éthique",
             "comit é national d éthique",
             "comit é consultatif d éthique",
             "comit é d éthique",
             "comit é consultatif national d ethique",
             "comit é national consultatif d ethique",
             "comit é national d ethique",
             "comit é consultatif d ethique",
             "comit é consultatif national d ethique",
             "consultatif national d éthique",
             "Consultatif National d Éthique",
             "ce comité",
             "ccne",
             "ccné"),
    fin_de_vie = c("fin de vie", "fin de la vie"),
    personne_humaine_potentielle = c("personne humaine potentielles",
                                     "personnes humaines potentielles"),
    personne_agée = c("personne agée","personnes agées"),
    personne_handicapée_mentale = c("personne handicapée mentale",
                                    "personnes handicapées mentales",
                                    "personne atteinte d handicap mental",
                                    "personne atteinte d handicaps mentaux",
                                    "personnes atteintes d handicaps mentaux",
                                    "personnes atteintes d handicap mental",
                                    "personne atteinte de maladie mentale",
                                    "personne atteinte de maladies mentales",
                                    "personnes atteintes de maladie mentale",
                                    "personnes atteintes de maladies mentales"),
    personne_malade = c("personne malade",
                        "personnes malades",
                        "personne atteinte de maladie",
                        "personne atteinte de maladies",
                        "personnes atteintes de maladie",
                        "personnes atteintes de maladies"),
    personne_handicapée = c("personne handicapée",
                            "personnes handicapées",
                            "personne atteinte d handicap",
                            "personne atteinte d handicaps",
                            "personnes atteintes d handicaps",
                            "personnes atteintes d handicap"),
    personne_confiance = c("personne de confiance",
                           "personnes de confiance"),
    cellule_souche = c("cellule souche","cellules souches"),
    don_d_organe = c("don d organe","don d organes",
                     "dons d organe","dons d organes",
                     "don de moelle", "dons de moelle",
                     "don de moelles", "dons de moelles",
                     "don de tissu", "dons de tissu",
                     "don de tissus", "dons de tissus"),
    don_de_sang = c("don du sang","don de sang",
                    "dons du sang","dons de sang"),
    don_gamète = c("don d ovocyte", "don de gamète", "don d embryon",
                   "don d ovocytes", "don de gamètes", "don d embryons", 
                   "don de l embryon", "don de la gamète", "don de l ovocyte", 
                   "don de ces ovocytes", "don de ses ovocytes", "don de sperme", 
                   "don du sperme", "don de spermes",
                   "dons d ovocyte", "dons de gamète", "dons d embryon",
                   "dons d ovocytes", "dons de gamètes", "dons d embryons", 
                   "dons de l embryon", "dons de la gamète", "dons de l ovocyte", 
                   "dons de ces ovocytes", "dons de ses ovocytes", "dons de sperme", 
                   "dons du sperme", "dons de spermes"),
    don_cellule = c("don de cellule", "dons de cellule",
                    "don d'une cellule", "dons d'une cellule",
                    "don de cellules", "dons de cellules"),
    santé_publique = c("santé publique","santé pu- blique"),
    santé_travail = c("santé au travail", "santé du travail"),
    sécurité_sociale = c("sécurité sociale"),
    assurance_maladie = c("assurance_maladie"),
    ricoeur = c("ricoeur", "ricœur"),
    pma = c("amp","pma","PMA","AMP",
            "assistance médicale à la procréation",
            "assistances médicales à la procréation",
            "procréation médicalement assistée",
            "procréations médicalement assistées"),
    gpa = c("gpa",
            "gestation pour autrui"),
    vie_privée = c("vie privée",
                   "vies privées"),
    CRISPR_Cas9 = c("CRISPR-Cas9",
                    "crispr cas",
                    "crispr-cas",
                    "crispr-cas9",
                    "crispr cas9",
                    "crispr",
                    "CRISPR Cas9",
                    "CRISPR Cas",
                    "CRISPR-Cas",
                    "CRISPR",
                    "Cas9"),
    vih = c("vih","sida"),
    avortement = c("IVG","ivg",
                   "avortement",
                   "avortements",
                   "interruptions de grossesses",
                   "interruption de grossesse",
                   "interruption de grossesses",
                   "interruptions de grossesse"),
    fiv = c("fertilisation in vitro",
            "fiv",
            "fivete",
            "ivf",
            "ICSI",
            "injection intracytoplasmique de spermatozoïde",
            "intra cytoplasmic sperm injection",
            "intracytoplasmic sperm injection"),
    expérience = c("essai","expérience","expérimentation")
    
  ))
  
  # suppression pluriel, genre
  
  tk <- tokens_compound(tk_original, pattern = important_expressions, case_insensitive = TRUE)
  
  replacement_dict <- dictionary(list(
    embryon = c("embryon*"),
    parent = c("parent*"),
    enfant = c("enfant*"),
    éthique = c("éthiques"),
    médical = c("médical*"),
    droit = c("droits"),
    don = c("dons"),
    gène = c("gènes"),
    femme = c("femmes"),
    malade = c("malades"),
    médecin = c("médecins"),
    patient = c("patients"),
    humain = c("humain*"), # risque de négliger les distinctions de genre ?
    personne = c("personnes","personne"),
    cellule = c("cellules"),
    test = c("tests"),
    génétique = c("génétiques"),
    problème = c("problèmes"),
    risque = c("risques"),
    question = c("questions"),
    résultat = c("résultats")
  ))
  
  tk <- tokens_lookup(tk, replacement_dict, exclusive = FALSE)
  
  # remove stop words
  toremove <- c(stopwords("fr"), stopwords("en"),
                c("number_token", "number_token-number_token"),
                c("être", "a", "plus", "peut", "comme", 
                  "d’une", "cas", "d’un", 
                  "si", "entre", "fait", "non", "doit", 
                  "dont", "aussi", 
                  "ainsi", "tout", "faire", "donc", 
                  "très", "°", "peuvent", "chez", 
                  "bien", "où", "toute",
                  "autres", "elles", 
                  "moins", "in", "après", 
                  "encore", "notamment", "certains", 
                  "alors", "pourrait", "mise",
                  "part", "autre","tous", "possible",
                  "exemple", "n’est", "avoir", "souvent","of",">","<","+","u","NA"),
                c("qu’il", "avant", 
                  "c’est", "certaines", 
                  "selon", "celle", 
                  "doivent", "déjà", 
                  "celui", "lors", "plusieurs", 
                  "sous", "toujours", 
                  "depuis", "toutes", "concernant", 
                  "devrait", "seulement",
                  "faut", "telle",
                  "également", "cependant", "façon", "fois",
                  "prendre", "point", "nécessaire", 
                  "p", "partir", "donner",
                  "dès", "ni", "nouvelles",
                  "aujourd","hui","agit",
                  "objet","place","projet",
                  "deux","ment","e",
                  "méme","étre","etre","|","ȼ","Ȼ","¢" # ce symbole est utilisé en bio pour désigné cellule apparement.
                )
  )
  tk <- tokens_remove(tk,toremove)
  
  
}

to_dfm_custom <- function(tk) {
  toremove <- c(stopwords("fr"), stopwords("en"),
                c("number_token", "number_token-number_token"),
                c("être", "a", "plus", "peut", "comme", 
                  "d’une", "cas", "d’un", 
                  "si", "entre", "fait", "non", "doit", 
                  "dont", "aussi", 
                  "ainsi", "tout", "faire", "donc", 
                  "très", "°", "peuvent", "chez", 
                  "bien", "où", "toute",
                  "autres", "elles", 
                  "moins", "in", "après", 
                  "encore", "notamment", "certains", 
                  "alors", "pourrait", "mise",
                  "part", "autre","tous", "possible",
                  "exemple", "n’est", "avoir", "souvent","of",">","<","+","u","NA"),
                c("qu’il", "avant", 
                  "c’est", "certaines", 
                  "selon", "celle", 
                  "doivent", "déjà", 
                  "celui", "lors", "plusieurs", 
                  "sous", "toujours", 
                  "depuis", "toutes", "concernant", 
                  "devrait", "seulement",
                  "faut", "telle",
                  "également", "cependant", "façon", "fois",
                  "prendre", "point", "nécessaire", 
                  "p", "partir", "donner",
                  "dès", "ni", "nouvelles",
                  "aujourd","hui","agit",
                  "objet","place","projet",
                  "deux","ment","e",
                  "méme","étre","etre","|","ȼ","Ȼ","¢" # ce symbole est utilisé en bio pour désigné cellule apparement.
                )
  )
  
  important_expressions <- dictionary(list(
    vntr = c("variable number of tandem repeat", "variable number tandem repeat"),
    ccne = c("comité national consultatif d éthique",
             "comité national d éthique",
             "comité consultatif d éthique",
             "comité d éthique",
             "comité consultatif national d ethique",
             "comité national consultatif d ethique",
             "comité national d ethique",
             "comité consultatif d ethique",
             "comité d ethique",
             "comité consultatif national d ethique",
             "comit é national consultatif d éthique",
             "comit é national d éthique",
             "comit é consultatif d éthique",
             "comit é d éthique",
             "comit é consultatif national d ethique",
             "comit é national consultatif d ethique",
             "comit é national d ethique",
             "comit é consultatif d ethique",
             "comit é consultatif national d ethique",
             "consultatif national d éthique",
             "Consultatif National d Éthique",
             "ce comité",
             "ccne",
             "ccné"),
    fin_de_vie = c("fin de vie", "fin de la vie"),
    personne_humaine_potentielle = c("personne humaine potentielles",
                                     "personnes humaines potentielles"),
    personne_agée = c("personne agée","personnes agées"),
    personne_handicapée_mentale = c("personne handicapée mentale",
                                    "personnes handicapées mentales",
                                    "personne atteinte d handicap mental",
                                    "personne atteinte d handicaps mentaux",
                                    "personnes atteintes d handicaps mentaux",
                                    "personnes atteintes d handicap mental",
                                    "personne atteinte de maladie mentale",
                                    "personne atteinte de maladies mentales",
                                    "personnes atteintes de maladie mentale",
                                    "personnes atteintes de maladies mentales"),
    personne_malade = c("personne malade",
                        "personnes malades",
                        "personne atteinte de maladie",
                        "personne atteinte de maladies",
                        "personnes atteintes de maladie",
                        "personnes atteintes de maladies"),
    personne_handicapée = c("personne handicapée",
                            "personnes handicapées",
                            "personne atteinte d handicap",
                            "personne atteinte d handicaps",
                            "personnes atteintes d handicaps",
                            "personnes atteintes d handicap"),
    personne_confiance = c("personne de confiance",
                           "personnes de confiance"),
    cellule_souche = c("cellule souche","cellules souches"),
    don_d_organe = c("don d organe","don d organes",
                     "dons d organe","dons d organes",
                     "don de moelle", "dons de moelle",
                     "don de moelles", "dons de moelles",
                     "don de tissu", "dons de tissu",
                     "don de tissus", "dons de tissus"),
    don_de_sang = c("don du sang","don de sang",
                    "dons du sang","dons de sang"),
    don_gamète = c("don d ovocyte", "don de gamète", "don d embryon",
                   "don d ovocytes", "don de gamètes", "don d embryons", 
                   "don de l embryon", "don de la gamète", "don de l ovocyte", 
                   "don de ces ovocytes", "don de ses ovocytes", "don de sperme", 
                   "don du sperme", "don de spermes",
                   "dons d ovocyte", "dons de gamète", "dons d embryon",
                   "dons d ovocytes", "dons de gamètes", "dons d embryons", 
                   "dons de l embryon", "dons de la gamète", "dons de l ovocyte", 
                   "dons de ces ovocytes", "dons de ses ovocytes", "dons de sperme", 
                   "dons du sperme", "dons de spermes"),
    don_cellule = c("don de cellule", "dons de cellule",
                    "don d'une cellule", "dons d'une cellule",
                    "don de cellules", "dons de cellules"),
    santé_publique = c("santé publique","santé pu- blique"),
    santé_travail = c("santé au travail", "santé du travail"),
    sécurité_sociale = c("sécurité sociale"),
    assurance_maladie = c("assurance_maladie"),
    ricoeur = c("ricoeur", "ricœur"),
    pma = c("amp","pma","PMA","AMP",
            "assistance médicale à la procréation",
            "assistances médicales à la procréation",
            "procréation médicalement assistée",
            "procréations médicalement assistées"),
    gpa = c("gpa",
            "gestation pour autrui"),
    vie_privée = c("vie privée",
                   "vies privées"),
    CRISPR_Cas9 = c("CRISPR-Cas9",
                    "crispr cas",
                    "crispr-cas",
                    "crispr-cas9",
                    "crispr cas9",
                    "crispr",
                    "CRISPR Cas9",
                    "CRISPR Cas",
                    "CRISPR-Cas",
                    "CRISPR",
                    "Cas9"),
    vih = c("vih","sida"),
    avortement = c("IVG","ivg",
                   "avortement",
                   "avortements",
                   "interruptions de grossesses",
                   "interruption de grossesse",
                   "interruption de grossesses",
                   "interruptions de grossesse"),
    fiv = c("fertilisation in vitro",
            "fiv",
            "fivete",
            "ivf",
            "ICSI",
            "injection intracytoplasmique de spermatozoïde",
            "intra cytoplasmic sperm injection",
            "intracytoplasmic sperm injection"),
    expérience = c("essai","expérience","expérimentation")
    
  ))
  
  dfm <- dfm(tk) |>
    dfm_lookup(important_expressions, exclusive = FALSE) |>
    dfm_remove(toremove) 
  
  return(dfm)
}

lemmatization <- function(base_avis_ccne = NULL, redo = FALSE){
  if(redo){
    model <- udpipe_load_model(file = "french-gsd-ud-2.5-191206.udpipe")
    
    anno <- udpipe_annotate(model, x = base_avis_ccne$avis, doc_id = base_avis_ccne$num)
    
    lemma_by_doc <- as.data.frame(anno)
    saveRDS(lemma_by_doc, "./data/intermediate/base_avis_ccne_additional_lemmas.rds")
    return(lemma_by_doc)
    
  } else {
    lemma_by_doc <- readRDS("~/Projets - Code/CCNE/data/intermediate/base_avis_ccne_additional_lemmas.rds")
    return(lemma_by_doc)
  }
  
  
}
