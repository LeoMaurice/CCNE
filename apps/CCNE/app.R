library(shiny)
library(httr)

# Fonction pour "réveiller" les applications
wake_app <- function(url) {
  try(GET(url), silent = TRUE)
}

# UI
ui <- fluidPage(
  titlePanel("Visualisations du Réseau de Citations du CCNE"),
  
  # Description en haut
  p("Graphiques interactifs créés dans le cadre d'un mémoire de sociologie sous la direction d'Emmanuel Didier, par Léopold MAURICE, sur le Comité Consultatif National d'Ethique (CCNE)"),
  
  br(),
  # Boutons de redirection en colonne
  fluidRow(
    column(12, actionButton("ego_network", "Ego Réseau de chaque avis", 
                            onclick = "window.location.href='https://leopoldmaurice.shinyapps.io/Ego_Citation_avis/'",
                            width = '100%')),
    column(12, actionButton("meso_network", "Réseaux entre les avis et entre les citations",
                            onclick = "window.location.href='https://leopoldmaurice.shinyapps.io/Mesonetwork/'",
                            width = '100%')),
    column(12, actionButton("top_citations", "Top citations dans les avis",
                            onclick = "window.location.href='https://leopoldmaurice.shinyapps.io/Top_citations/'",
                            width = '100%')),
    column(12, actionButton("list_avis", "Liste des avis du CCNE, avec informations",
                            onclick = "window.location.href='https://leopoldmaurice.shinyapps.io/Liste_avis/'",
                            width = '100%'))
  ),
  
  # Description en bas
  br(),
  p("Crédit : Léopold MAURICE"),
  p("Données : Avis du CCNE, citations collectées par l'auteur"),
  p("Graphiques fabriqués avec R, visNetwork, Posit et Shiny")
)

# Server
server <- function(input, output, session) {
  
  # Réveiller toutes les applications cibles au démarrage de l'application "hub"
  observe({
    wake_app("https://leopoldmaurice.shinyapps.io/Ego_Citation_avis/")
    wake_app("https://leopoldmaurice.shinyapps.io/Mesonetwork/")
    wake_app("https://leopoldmaurice.shinyapps.io/Top_citations/")
    wake_app("https://leopoldmaurice.shinyapps.io/Liste_avis/")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
