#!/bin/bash

# Définir les dossiers
OUTPUT_DIR="output"
MODELS_DIR="./src/models"

# Créer les dossiers si ils n'existent pas
mkdir -p $OUTPUT_DIR
mkdir -p $MODELS_DIR

# URL du fichier ZIP à télécharger
ZIP_URL="https://github.com/LeoMaurice/CCNE/releases/download/memoire_v13/2024-06-24_CamemBERT_personne_1400annotations_twocat_vivant_personne_models.zip"

# Nom du fichier ZIP (extraire le nom du fichier à partir de l'URL)
ZIP_FILE=$(basename $ZIP_URL)

# Télécharger le fichier ZIP dans le dossier ./src/models
curl -o $MODELS_DIR/$ZIP_FILE $ZIP_URL

# Vérifier si le téléchargement a réussi
if [ $? -eq 0 ]; then
  echo "Téléchargement réussi: $ZIP_FILE"
else
  echo "Erreur lors du téléchargement de: $ZIP_FILE"
  exit 1
fi

# Extraire le nom de base sans l'extension .zip
BASE_NAME=$(basename $ZIP_FILE .zip)

# Créer un dossier pour le contenu dézippé
UNZIP_DIR="$MODELS_DIR/$BASE_NAME"
mkdir -p $UNZIP_DIR

# Dézipper le fichier dans le dossier créé
unzip $MODELS_DIR/$ZIP_FILE -d $UNZIP_DIR

# Vérifier si la décompression a réussi
if [ $? -eq 0 ]; then
  echo "Décompression réussie dans le dossier: $UNZIP_DIR"
else
  echo "Erreur lors de la décompression de: $ZIP_FILE"
  exit 1
fi