# Définir les dossiers
$OUTPUT_DIR = "output"

$MODELS_DIR = ".\src\models"

# Créer les dossiers si ils n'existent pas
if (!(Test-Path -Path $OUTPUT_DIR)) {
    New-Item -ItemType Directory -Path $OUTPUT_DIR
}
if (!(Test-Path -Path $MODELS_DIR)) {
    New-Item -ItemType Directory -Path $MODELS_DIR
}

# URL du fichier ZIP à télécharger
$ZIP_URL = "https://github.com/LeoMaurice/CCNE/releases/download/memoire_v13/2024-06-24_CamemBERT_personne_1400annotations_twocat_vivant_personne_models.zip"

# Nom du fichier ZIP (extraire le nom du fichier à partir de l'URL)
$ZIP_FILE = [System.IO.Path]::GetFileName($ZIP_URL)

# Télécharger le fichier ZIP dans le dossier .\src\models
Invoke-WebRequest -Uri $ZIP_URL -OutFile "$MODELS_DIR\$ZIP_FILE"

# Vérifier si le téléchargement a réussi
if ($?) {
    Write-Host "Téléchargement réussi: $ZIP_FILE"
} else {
    Write-Host "Erreur lors du téléchargement de: $ZIP_FILE"
    exit 1
}

# Extraire le nom de base sans l'extension .zip
$BASE_NAME = [System.IO.Path]::GetFileNameWithoutExtension($ZIP_FILE)

# Créer un dossier pour le contenu dézippé
$UNZIP_DIR = "$MODELS_DIR\$BASE_NAME"
if (!(Test-Path -Path $UNZIP_DIR)) {
    New-Item -ItemType Directory -Path $UNZIP_DIR
}

# Dézipper le fichier dans le dossier créé
Add-Type -AssemblyName System.IO.Compression.FileSystem
[System.IO.Compression.ZipFile]::ExtractToDirectory("$MODELS_DIR\$ZIP_FILE", $UNZIP_DIR)

# Vérifier si la décompression a réussi
if ($?) {
    Write-Host "Décompression réussie dans le dossier: $UNZIP_DIR"
} else {
    Write-Host "Erreur lors de la décompression de: $ZIP_FILE"
    exit 1
}
