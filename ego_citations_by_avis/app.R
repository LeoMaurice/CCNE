library(shiny)
library(dplyr)
library(tidyr)
library(igraph)
library(visNetwork)

# Charger les objets pré-construits
load("network_data.RData")

# Couleurs fixes pour chaque catégorie
category_colors <- c(
  "CCNE" = "#285291",
  "Comité d'éthique" = "#4F2B8E",
  "Comparaison pays" = "#91188E",
  "Etat" = "#539027",
  "Forums, Autorités" = "#0D405B",
  "Loi" = "#34274D",
  "Org Internationales" = "#91181D",
  "Organismes recherches" = "#2C3324",
  "Presse" = "#022240",
  "Science, littérature" = "#21063F",
  "Société" = "#3D0620",
  "Avis étudié" = "#022013"
)

# UI
ui <- fluidPage(
  titlePanel("Visualisation de l'égo réseau des citations autour d'un avis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("node_id", "Sélectionner le numéro de l'avis:", 
                  choices = sort(as.numeric(micrograph_nodes_df$name[micrograph_nodes_df$Categorie == "CCNE"])), 
                  selected = min(as.numeric(micrograph_nodes_df$name[micrograph_nodes_df$Categorie == "CCNE"]), na.rm = TRUE)),
      checkboxGroupInput("categories", "Choisir les catégories à afficher:",
                         choices = unique(micrograph_nodes_df$Categorie),
                         selected = unique(micrograph_nodes_df$Categorie)),
      actionButton("toggle_select", "Décocher/Recocher toutes les catégories"),
      actionButton("toggle_non_direct", "Enlever/Mettre les liens non directs"),
      actionButton("toggle_ccne", "Montrer uniquement les citations internes au CCNE"),
      br(),
      tags$div(
        tags$h4("Légende des flèches"),
        tags$div(
          style = "display: flex; align-items: center;",
          tags$div(style = "width: 20px; height: 2px; background-color: black; margin-right: 5px;"),
          "Citations directes"
        ),
        tags$div(
          style = "display: flex; align-items: center; margin-top: 5px;",
          tags$div(style = "width: 20px; height: 2px; border-top: 2px dashed gray; margin-right: 5px;"),
          "Citations indirectes"
        )
      )
    ),
    mainPanel(
      visNetworkOutput("network", height = "800px")
    )
  )
)

# Server
server <- function(input, output, session) {
  all_categories <- unique(micrograph_nodes_df$Categorie)
  current_state <- reactiveVal(TRUE)  # Initial state is all categories selected
  show_non_direct <- reactiveVal(TRUE)  # Initial state is to show non-direct links
  
  observeEvent(input$toggle_select, {
    if (current_state()) {
      updateCheckboxGroupInput(session, "categories", selected = character(0))
    } else {
      updateCheckboxGroupInput(session, "categories", selected = all_categories)
    }
    current_state(!current_state())  # Toggle the state
  })
  
  observeEvent(input$toggle_non_direct, {
    show_non_direct(!show_non_direct())  # Toggle the state for non-direct links
  })
  
  observeEvent(input$toggle_ccne, {
    selected_categories <- input$categories
    if ("CCNE" %in% selected_categories) {
      if (length(selected_categories) == 1) {
        updateCheckboxGroupInput(session, "categories", selected = all_categories)
      } else {
        updateCheckboxGroupInput(session, "categories", selected = "CCNE")
      }
    } else {
      updateCheckboxGroupInput(session, "categories", selected = "CCNE")
    }
  })
  
  output$network <- renderVisNetwork({
    selected_node <- as.character(input$node_id)
    selected_categories <- input$categories
    show_non_direct_val <- show_non_direct()
    
    # Filtrer les noeuds et les liens pour afficher seulement ceux à une profondeur de 1 du noeud sélectionné
    subgraph_nodes <- neighborhood(microreseau_citation_igraph, order = 1, nodes = selected_node, mode = "all")[[1]]
    subgraph <- induced_subgraph(microreseau_citation_igraph, subgraph_nodes)
    
    # Ajouter "Avis étudié" à la liste des catégories sélectionnées
    if ("CCNE" %in% selected_categories) {
      selected_categories <- c(selected_categories, "Avis étudié")
    }
    
    # Filtrer par catégories sélectionnées, en gardant "Avis étudié"
    subgraph <- delete.vertices(subgraph, V(subgraph)[!(V(subgraph)$Categorie %in% selected_categories | V(subgraph)$name == selected_node)])
    
    # Convertir le sous-graph en format compatible avec visNetwork
    nodes <- data.frame(id = V(subgraph)$name, label = V(subgraph)$name, group = V(subgraph)$Categorie)
    edges <- as.data.frame(get.data.frame(subgraph, what = "edges"))
    
    # Mettre à jour la catégorie et les attributs du noeud sélectionné
    if (nrow(nodes) == 0) {
      nodes <- data.frame(id = selected_node, label = selected_node, group = "Avis étudié", size = 30, color = "#022013")
      edges <- data.frame(from = character(0), to = character(0), color = character(0), dashes = logical(0), width = numeric(0))
    } else {
      nodes <- nodes %>%
        mutate(group = ifelse(id == selected_node, "Avis étudié", group),
               size = ifelse(id == selected_node, 30, 15))  # Augmenter la taille du noeud sélectionné
    }
    
    # Appliquer les couleurs fixes aux catégories
    nodes$color <- category_colors[nodes$group]
    
    # Mettre en gras les liens à partir du noeud sélectionné et les autres en pointillé ou les enlever
    if (nrow(edges) > 0) {
      if (show_non_direct_val) {
        edges <- edges %>%
          mutate(color = ifelse(from == selected_node | to == selected_node, "black", "gray"),
                 dashes = ifelse(from == selected_node | to == selected_node, FALSE, TRUE),
                 width = ifelse(from == selected_node | to == selected_node, 2, 1))
      } else {
        edges <- edges %>%
          filter(from == selected_node | to == selected_node) %>%
          mutate(color = "black", dashes = FALSE, width = 2)
      }
    }
    
    # Créer le graphique visNetwork
    visNetwork(nodes, edges) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visGroups(groupname = "CCNE", color = "#285291", shape = "square") %>%
      visGroups(groupname = "Comité d'éthique", color = "#4F2B8E", shape = "triangle") %>%
      visGroups(groupname = "Comparaison pays", color = "#91188E", shape = "triangle") %>%
      visGroups(groupname = "Etat", color = "#539027", shape = "triangle") %>%
      visGroups(groupname = "Forums, Autorités", color = "#0D405B", shape = "triangle") %>%
      visGroups(groupname = "Loi", color = "#34274D", shape = "triangle") %>%
      visGroups(groupname = "Org Internationales", color = "#91181D", shape = "triangle") %>%
      visGroups(groupname = "Organismes recherches", color = "#2C3324", shape = "triangle") %>%
      visGroups(groupname = "Presse", color = "#022240", shape = "triangle") %>%
      visGroups(groupname = "Science, littérature", color = "#21063F", shape = "triangle") %>%
      visGroups(groupname = "Société", color = "#3D0620", shape = "triangle") %>%
      visGroups(groupname = "Avis étudié", color = "#022013", shape = "square") %>%
      addFontAwesome() %>%
      visLegend(useGroups = TRUE, position = "right", width = 0.2, ncol = 1,
                main = list(text = "Catégorie de citations",
                            style = "color:#000000;font-size:24px;text-align:center;")) %>%
      visEdges(arrows = 'to', color = list(color = "gray", highlight = "black")) %>%
      visNodes(font = list(size = 20))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
