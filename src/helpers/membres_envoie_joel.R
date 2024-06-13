# Supposons que votre data.frame s'appelle df et que la variable s'appelle "nom"
library(readr)
library(data.table)
membres_ccne <- read_delim("./data/intermediate/qui gouverne la science/membres_ccne.csv", 
                           delim = ";", escape_double = FALSE, col_types = cols(id_membre = col_integer(), 
                                                                                naissance = col_integer(), ...7 = col_skip(), 
                                                                                ...8 = col_skip(), ...9 = col_skip(), 
                                                                                ...10 = col_skip()), na = "NA", trim_ws = TRUE)
# Fonction pour transformer le nom et le prénom
transform_nom_prenom <- function(nom) {
  # Séparer le nom et le prénom
  nom_parts <- strsplit(nom, " ")[[1]]
  prenom <- nom_parts[1]
  nom_de_famille <- paste(nom_parts[-1], collapse=" ")
  
  # Remplacer les espaces dans les prénoms composés par "-"
  prenom <- gsub(" ", "-", toupper(prenom))
  
  # Mettre les noms en majuscules et remplacer les espaces par "@"
  nom_de_famille <- gsub(" ", "@", toupper(nom_de_famille))
  
  # Combiner le nom et le prénom
  iconv(paste(nom_de_famille, prenom, sep="_"), from = "UTF-8", to = "ASCII//TRANSLIT")
}

# Appliquer la fonction à la colonne "nom" et créer une nouvelle colonne "NOM_PRENOM"
membres_ccne$NOM_PRENOM <- sapply(membres_ccne$nom, transform_nom_prenom)

# Afficher le data.frame avec la nouvelle colonne
fwrite(membres_ccne, 
      "./data/intermediate/qui gouverne la science/membres_ccne_NOM_PRENOM.csv",
      sep=";")
