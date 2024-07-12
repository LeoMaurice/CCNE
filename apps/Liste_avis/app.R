# Installer les packages nécessaires si ce n'est pas déjà fait
if (!require("shiny")) install.packages("shiny")
if (!require("dplyr")) install.packages("dplyr")
if (!require("DT")) install.packages("DT")

# Charger les packages
library(shiny)
library(dplyr)
library(DT)

# Charger les données
load("metadata_avis.RData")

# Définir l'interface utilisateur
ui <- fluidPage(
  titlePanel("Liste_avis"),
  fluidRow(
    column(width = 12,
           DTOutput("table"))
  ),
  tags$style(type = "text/css", 
             ".container-fluid { padding-left: 0px; padding-right: 0px; }",
             ".dataTables_wrapper .col-sm-12 { width: 100%; }",
             ".dataTables_wrapper .row { width: 100%; }"
  )
)

# Définir le serveur
server <- function(input, output) {
  output$table <- renderDT({
    datatable(base_avis, 
              options = list(
                pageLength = 10, 
                autoWidth = TRUE
              ),
              width = '100%'
    )
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
