library(shiny)
library(dplyr)
library(tidyr)
library(igraph)

# Vérifier si le fichier existe avant de le charger
if (file.exists("network_data.RData")) {
  load("network_data.RData")
} else {
  stop("Le fichier 'network_data.RData' est manquant.")
}

# Calculer le nombre d'edges connectés pour chaque noeud dans le graphe complet
try({
  node_degree <- degree(graph_from_data_frame(mesograph_edgelist_df, directed = TRUE), mode = "all")
  node_degree <- data.frame(name = names(node_degree), degree = as.numeric(node_degree))
}, silent = TRUE)

# Données des présidents
max_min_par_president <- data.frame(
  president = c("Bernard (83-91)", "Changeux (92-99)", "Sicard (00-08)", "Grimfeld (09-11)", "Ameisen (12-15)", "Delfraissy (16-)"),
  avis_max = c(28, 60, 103, 115, 124, 144),
  avis_min = c(1, 29, 61, 104, 116, 125)
)

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #backButton {
        position: fixed;
        top: 10px;
        right: 10px;
        z-index: 1000;
      }
    "))
  ),
  tags$div(
    id = "backButton",
    actionButton("back", "Retour aux différents choix", 
                 onclick = "window.location.href='https://leopoldmaurice.shinyapps.io/CCNE/'")
  ),
  titlePanel("Top Citations du CCNE"),
  sidebarLayout(
    sidebarPanel(
      div(
        style = "display: flex; flex-wrap: wrap; gap: 10px;",
        uiOutput("president_buttons")
      ),
      br(),
      sliderInput("avis_range", "Sélectionner la plage des avis à afficher:",
                  min = min(as.numeric(mesograph_nodes_df$name), na.rm = TRUE),
                  max = max(as.numeric(mesograph_nodes_df$name), na.rm = TRUE),
                  value = c(125, 144),  # Valeurs par défaut du slider
                  step = 1),
      br(),
      selectInput("top_category", "Choisir une catégorie pour calculer le top des plus cités:", 
                  choices = unique(mesograph_nodes_df$Categorie), 
                  selected = "Loi"),
      selectInput("top_n", "Nombre de nœuds à afficher dans le top:", 
                  choices = c(10, 20, 50, 100), selected = 10),
      radioButtons("name_choice", "Choisir le type de noms:",
                   choices = list("Noms détaillés" = "meso", "Noms simplifiés" = "micro"), 
                   selected = "meso"),
      uiOutput("degree_buttons")
    ),
    mainPanel(
      htmlOutput("top_cited_single"),
      htmlOutput("top_cited_multiple")
    )
  )
)

# Server
server <- function(input, output, session) {
  current_graph <- reactive({
    input$name_choice
  })
  current_degree_mode <- reactiveVal("in")
  
  graph_data <- reactive({
    if (current_graph() == "meso") {
      list(nodes = mesograph_nodes_df, edges = mesograph_edgelist_df)
    } else {
      list(nodes = micrograph_nodes_df, edges = micrograph_edgelist_df)
    }
  })
  
  output$president_buttons <- renderUI({
    buttons <- lapply(1:nrow(max_min_par_president), function(i) {
      actionButton(paste0("president_", i), max_min_par_president$president[i], style = "margin-right: 5px;")
    })
    do.call(tagList, buttons)
  })
  
  observe({
    lapply(1:nrow(max_min_par_president), function(i) {
      observeEvent(input[[paste0("president_", i)]], {
        selected_president <- max_min_par_president$president[i]
        range <- max_min_par_president %>% filter(president == selected_president)
        updateSliderInput(session, "avis_range", value = c(range$avis_min, range$avis_max))
      })
    })
  })
  
  output$degree_buttons <- renderUI({
    if (input$top_category == "CCNE") {
      tagList(
        actionButton("toggle_degree", "Avis les plus cités/Avis qui citent le plus")
      )
    } else {
      NULL
    }
  })
  
  observeEvent(input$toggle_degree, {
    new_degree_mode <- if (current_degree_mode() == "in") "out" else "in"
    current_degree_mode(new_degree_mode)
  })
  
  observe({
    choices <- unique(graph_data()$nodes$Categorie)
    if (!"Loi" %in% choices) {
      choices <- c("Loi", choices)
    }
    updateSelectInput(session, "top_category", choices = choices, selected = "Loi")
  })
  
  # Render the table counting single citations per document
  output$top_cited_single <- renderUI({
    top_category <- input$top_category
    avis_range <- input$avis_range
    top_n <- as.numeric(input$top_n)
    
    data <- graph_data()
    
    # Filtrer les avis dans le range sélectionné
    filtered_nodes <- data$nodes %>%
      filter(as.numeric(name) >= avis_range[1] & as.numeric(name) <= avis_range[2])
    
    # Inclure les liens impliquant des avis dans le range sélectionné
    filtered_edges <- data$edges %>% 
      filter((as.numeric(from) >= avis_range[1] & as.numeric(from) <= avis_range[2]) |
               (as.numeric(to) >= avis_range[1] & as.numeric(to) <= avis_range[2]))
    
    # Inclure les noeuds pour tous les liens sélectionnés
    linked_nodes <- unique(c(filtered_edges$from, filtered_edges$to))
    filtered_nodes <- data$nodes %>% 
      filter(name %in% linked_nodes)
    
    # Compter les citations simples
    citation_count <- filtered_edges %>%
      group_by(to) %>%
      summarise(citations = n_distinct(from))
    
    top_nodes <- filtered_nodes %>%
      filter(Categorie == top_category) %>%
      left_join(citation_count, by = c("name" = "to")) %>%
      arrange(desc(citations)) %>%
      top_n(top_n, citations) %>%
      select(name, citations)
    
    top_html <- paste0(
      "<div><h4>Top ", top_n, " des documents les plus cités pour ", top_category, ", parmi la plage sélectionnée, en comptant une citation par avis :</h4>",
      "<ul>",
      paste0(
        "<li>", top_nodes$name, " (", top_nodes$citations, " citations)</li>",
        collapse = ""
      ),
      "</ul></div>"
    )
    
    HTML(top_html)
  })
  
  # Render the table counting multiple citations per document
  output$top_cited_multiple <- renderUI({
    top_category <- input$top_category
    avis_range <- input$avis_range
    top_n <- as.numeric(input$top_n)
    
    data <- graph_data()
    
    # Filtrer les avis dans le range sélectionné
    filtered_nodes <- data$nodes %>%
      filter(as.numeric(name) >= avis_range[1] & as.numeric(name) <= avis_range[2])
    
    # Inclure les liens impliquant des avis dans le range sélectionné
    filtered_edges <- data$edges %>% 
      filter((as.numeric(from) >= avis_range[1] & as.numeric(from) <= avis_range[2]) |
               (as.numeric(to) >= avis_range[1] & as.numeric(to) <= avis_range[2]))
    
    # Inclure les noeuds pour tous les liens sélectionnés
    linked_nodes <- unique(c(filtered_edges$from, filtered_edges$to))
    filtered_nodes <- data$nodes %>% 
      filter(name %in% linked_nodes)
    
    # Compter les citations multiples
    citation_count <- filtered_edges %>%
      group_by(to) %>%
      summarise(citations = sum(Citations))
    
    top_nodes <- filtered_nodes %>%
      filter(Categorie == top_category) %>%
      left_join(citation_count, by = c("name" = "to")) %>%
      arrange(desc(citations)) %>%
      top_n(top_n, citations) %>%
      select(name, citations)
    
    top_html <- paste0(
      "<div><h4>Top ", top_n, " des documents les plus cités pour ", top_category, ", parmi la plage sélectionnée, en comptant de multiples citations par avis :</h4>",
      "<ul>",
      paste0(
        "<li>", top_nodes$name, " (", top_nodes$citations, " citations)</li>",
        collapse = ""
      ),
      "</ul></div>"
    )
    
    HTML(top_html)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)