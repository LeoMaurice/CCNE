# Installer les packages nécessaires si ce n'est pas déjà fait
if (!require("shiny")) install.packages("shiny")
if (!require("dplyr")) install.packages("dplyr")
if (!require("DT")) install.packages("DT")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("arrow")) install.packages("arrow")

# Charger les packages
library(shiny)
library(dplyr)
library(DT)
library(shinyjs)
library(arrow)

# Charger les données
base_avis <- readRDS("base_avis.rds")

# Définir l'interface utilisateur
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Liste des avis, avec informations"),
  fluidRow(
    column(width = 12,
           div(class = "top-buttons",
               actionButton("back_button", "Retour aux choix"),
               actionButton("explain_button", "Explications de l'application")
           ),
           DTOutput("table")
    )
  )
)

# Définir le serveur
server <- function(input, output, session) {
  output$table <- renderDT({
    datatable(base_avis, 
              rownames = FALSE, 
              options = list(
                scrollY = 400, 
                scrollX = TRUE
              ),
              escape = FALSE
    )
  })
  
  observeEvent(input$back_button, {
    shinyjs::runjs("window.location.href = 'https://leomaurice.github.io/CCNE/'")
  })
  
  observeEvent(input$explain_button, {
    showModal(modalDialog(
      title = "Explications de l'application",
      HTML("
        <ul>
          <li><b>Numéro avis :</b> Le numéro attribué à l'avis.</li>
          <li><b>Titre :</b> Le titre de l'avis.</li>
          <li><b>Président :</b> Le président responsable de l'avis.</li>
          <li><b>Date :</b> La date de publication de l'avis.</li>
          <li><b>Thème :</b> Le thème principal de l'avis.</li>
          <li><b>Nombre de pages :</b> Le nombre total de pages de l'avis.</li>
          <li><b>Rapporteurs :</b> Les rapporteurs associés à l'avis.</li>
          <li><b>Membres du Groupe de Travail :</b> Les membres du groupe de travail ayant contribué à l'avis.</li>
        </ul>
      "),
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
