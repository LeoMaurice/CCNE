library(shiny)
library(dplyr)
library(tidyr)
library(igraph)
library(networkD3)

# Vérifier si le fichier existe avant de le charger
if (file.exists("network_data.RData")) {
  load("network_data.RData")
} else {
  stop("Le fichier 'network_data.RData' est manquant.")
}

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
  "Société" = "#3D0620"
)

# Calculer le nombre d'edges connectés pour chaque noeud dans le graphe complet
try({
  node_degree <- degree(graph_from_data_frame(mesograph_edgelist_df, directed = TRUE), mode = "all")
  node_degree <- data.frame(name = names(node_degree), degree = as.numeric(node_degree))
}, silent = TRUE)

# UI
ui <- fluidPage(
  titlePanel("Visualisation du Réseau de Citations du CCNE"),
  sidebarLayout(
    sidebarPanel(
      div(style = "display: flex; align-items: center; justify-content: space-between;",
          actionButton("toggle_network", "Afficher/Masquer le réseau"),
          htmlOutput("legend")
      ),
      sliderInput("avis_range", "Sélectionner la plage des avis à afficher:",
                  min = min(as.numeric(mesograph_nodes_df$name), na.rm = TRUE),
                  max = max(as.numeric(mesograph_nodes_df$name), na.rm = TRUE),
                  value = c(124, 144),  # Valeurs par défaut du slider
                  step = 1),
      checkboxGroupInput("categories", "Choisir les catégories à afficher:",
                         choices = unique(mesograph_nodes_df$Categorie),
                         selected = c("Loi", "CCNE")),
      actionButton("toggle_select", "Décocher/Recocher toutes les catégories"),
      selectInput("top_category", "Choisir une catégorie pour calculer le top des plus cités:", choices = unique(mesograph_nodes_df$Categorie), selected = "Loi"),
      selectInput("top_n", "Nombre de nœuds à afficher dans le top:", choices = c(10, 20, 50, 100), selected = 10),
      htmlOutput("top_cited")
    ),
    mainPanel(
      conditionalPanel(
        condition = "output.networkVisible",
        forceNetworkOutput("network", height = "800px")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  all_categories <- unique(mesograph_nodes_df$Categorie)
  current_state <- reactiveVal(TRUE)  # Initial state is all categories selected
  network_visible <- reactiveVal(FALSE)  # Initial state is network hidden
  
  observeEvent(input$toggle_select, {
    if (current_state()) {
      updateCheckboxGroupInput(session, "categories", selected = character(0))
    } else {
      updateCheckboxGroupInput(session, "categories", selected = all_categories)
    }
    current_state(!current_state())  # Toggle the state
  })
  
  observeEvent(input$toggle_network, {
    network_visible(!network_visible())
  })
  
  output$networkVisible <- reactive({
    network_visible()
  })
  outputOptions(output, "networkVisible", suspendWhenHidden = FALSE)
  
  output$network <- renderForceNetwork({
    selected_categories <- input$categories
    avis_range <- input$avis_range
    
    # Filtrer les noeuds par catégories sélectionnées et range d'avis
    filtered_nodes <- mesograph_nodes_df %>% 
      filter(Categorie %in% selected_categories & 
               as.numeric(name) >= avis_range[1] & 
               as.numeric(name) <= avis_range[2])
    
    # Inclure les liens impliquant des avis dans le range sélectionné
    filtered_edges <- mesograph_edgelist_df %>% 
      filter((Categorie %in% selected_categories) &
               ((as.numeric(from) >= avis_range[1] & as.numeric(from) <= avis_range[2]) |
                  (as.numeric(to) >= avis_range[1] & as.numeric(to) <= avis_range[2])))
    
    # Inclure les noeuds pour tous les liens sélectionnés
    linked_nodes <- unique(c(filtered_edges$from, filtered_edges$to))
    filtered_nodes <- mesograph_nodes_df %>% 
      filter(name %in% linked_nodes & Categorie %in% selected_categories)
    
    # Créer une correspondance entre les noms et les indices pour networkD3
    node_indices <- filtered_nodes %>%
      mutate(index = row_number() - 1) %>%
      select(name, index)
    
    edges <- filtered_edges %>%
      inner_join(node_indices, by = c("from" = "name")) %>%
      rename(source = index) %>%
      inner_join(node_indices, by = c("to" = "name")) %>%
      rename(target = index)
    
    nodes <- filtered_nodes %>%
      left_join(node_degree, by = "name") %>%
      mutate(group = as.numeric(factor(Categorie)),
             color = category_colors[Categorie],
             size = degree,  # Taille variable en fonction du nombre d'edges connectés
             label = name) %>%  # Labels permanents pour CCNE
      select(name, group, color, size, label)
    
    # Créer le graphique networkD3
    forceNetwork(Links = edges, Nodes = nodes, 
                 Source = 'source', Target = 'target', 
                 NodeID = 'label', Group = 'group',
                 Nodesize = 'size', 
                 fontSize = 12, linkDistance = 100, charge = -200,
                 colourScale = JS("d3.scaleOrdinal().domain([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]).range(['#285291', '#4F2B8E', '#91188E', '#539027', '#0D405B', '#34274D', '#91181D', '#2C3324', '#022240', '#21063F', '#3D0620']);"),
                 zoom = TRUE, opacity = 0.8)
  })
  
  output$legend <- renderUI({
    selected_categories <- input$categories
    legend_colors <- category_colors[selected_categories]
    legend_html <- paste0(
      "<div style='display: flex; flex-wrap: wrap;'>",
      paste0(
        "<div style='margin: 5px;'><svg width='20' height='20'><rect width='20' height='20' style='fill:", legend_colors, ";'/></svg> ", names(legend_colors), "</div>",
        collapse = ""
      ),
      "</div>"
    )
    HTML(legend_html)
  })
  
  output$top_cited <- renderUI({
    top_category <- input$top_category
    avis_range <- input$avis_range
    top_n <- as.numeric(input$top_n)
    
    # Filtrer les avis dans le range sélectionné
    filtered_nodes <- micrograph_nodes_df %>%
      filter(as.numeric(name) >= avis_range[1] & as.numeric(name) <= avis_range[2])
    
    # Inclure les liens impliquant des avis dans le range sélectionné
    filtered_edges <- micrograph_edgelist_df %>% 
      filter((as.numeric(from) >= avis_range[1] & as.numeric(from) <= avis_range[2]) |
               (as.numeric(to) >= avis_range[1] & as.numeric(to) <= avis_range[2]))
    
    # Inclure les noeuds pour tous les liens sélectionnés
    linked_nodes <- unique(c(filtered_edges$from, filtered_edges$to))
    filtered_nodes <- micrograph_nodes_df %>% 
      filter(name %in% linked_nodes)
    
    # Calculer le top n des noeuds les plus cités pour la catégorie sélectionnée
    top_nodes <- filtered_nodes %>%
      filter(Categorie == top_category) %>%
      left_join(node_degree, by = "name") %>%
      arrange(desc(degree)) %>%
      top_n(top_n, degree) %>%
      select(name, degree)
    
    top_html <- paste0(
      "<div><h4>Top ", top_n, " des noeuds les plus cités pour ", top_category, ", parmi la plage sélectionnée:</h4>",
      "<ul>",
      paste0(
        "<li>", top_nodes$name, " (", top_nodes$degree, " citations)</li>",
        collapse = ""
      ),
      "</ul></div>"
    )
    
    HTML(top_html)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)