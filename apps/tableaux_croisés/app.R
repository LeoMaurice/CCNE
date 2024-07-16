library(shiny)
library(tidyr)
library(dplyr)
library(DT)
library(shinyjs)

load("tableaux_croisés.RData")

# Définir l'interface utilisateur
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Information de l'angle des avis en fonction de la profession."),
  sidebarLayout(
    sidebarPanel(
      div(style = "margin-bottom: 10px;", actionButton("back_button", "Retour aux choix")),
      div(style = "margin-bottom: 10px;", actionButton("explain_button", "Explications")),
      width = 2  # Ajustez cette valeur pour définir la largeur de la barre latérale
    ),
    mainPanel(
      h3(tags$b("Pourcentage des avis qui ont au moins une citation du type")),
      DTOutput("citations_by_professions"),
      h3(tags$b("Pourcentage des avis qui ont au moins une phrase associée à une certaine personnalité humaine (classification BERT, 2 classes)")),
      DTOutput("bert_by_professions"),
      h3(tags$b("Pourcentage des avis qui ont au moins une phrase associée à une certaine personnalité humaine (classification GPT, 7 classes)")),
      DTOutput("gpt_by_professions")
    )
  )
)

# Définir la logique du serveur
server <- function(input, output, session) {
  output$citations_by_professions <- renderDT({
    datatable(citations_by_professions, 
              rownames = TRUE, 
              escape = FALSE,
              options = list(pageLength = 11)
    )
  })
  
  output$bert_by_professions <- renderDT({
    datatable(bert_by_professions, 
              rownames = TRUE, 
              escape = FALSE
    )
  })
  
  output$gpt_by_professions <- renderDT({
    datatable(gpt_by_professions, 
              rownames = TRUE, 
              escape = FALSE
    )
  })
  
  observeEvent(input$back_button, {
    shinyjs::runjs("window.location.href = 'https://leopoldmaurice.shinyapps.io/CCNE/'")
  })
  
  observeEvent(input$explain_button, {
    showModal(modalDialog(
      title = "Explications de l'application",
      HTML("
        <ul>
          <li><b>Tableau des citations par professions :</b> On y compte pour les avis ayant au moins une des professions en tant que rapporteurs, le pourcentage de ces avis qui citent une des catégories établies..</li>
          <li><b>Tableau BERT par professions :</b> On y compte pour les avis ayant au moins une des professions en tant que rapporteurs, le pourcentage de ces avis qui utilisent au moins une fois une des catégories établies.</li>
          <li><b>Tableau GPT par professions :</b> On y compte pour les avis ayant au moins une des professions en tant que rapporteurs, le pourcentage de ces avis qui utilisent au moins une fois une des catégories établies.</li>
        </ul>
      "),
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
