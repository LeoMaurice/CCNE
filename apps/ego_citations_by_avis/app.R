library(shiny)
library(dplyr)
library(tidyr)
library(igraph)
library(visNetwork)

# Charger les objets pré-construits
load("network_data.RData")

# Couleurs fixes pour chaque catégorie
category_colors <- c(
  "Auteurs" = "#6CC7B3",
  "Autorités" = "#285291",
  "CCNE" = "#9D3A5E",
  "Comité d'éthique" = "#579125",
  "Comparaison pays" = "#0F0F5C",
  "Etat" = "#91188F",
  "Forums" = "#0E405C",
  "Loi" = "#915B11",
  "Org Internationales" = "#4F2B91",
  "Presse" = "#91181E",
  "Science, littérature" = "#0B5C2D",
  "Société" = "#BD6345",
  "Avis étudié" = "#4269A0"
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
      #explanationButton {
        position: fixed;
        top: 50px;
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
  tags$div(
    id = "explanationButton",
    actionButton("explanation", "Explication de l'application")
  ),
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
      radioButtons("non_direct_links", "Liens non directs:",
                   choices = list("Afficher" = TRUE, "Masquer" = FALSE), 
                   selected = TRUE),
      radioButtons("graph_mode", "Mode du graphe:",
                   choices = list("Noms complets" = "micro", "Simplifié" = "meso"), 
                   selected = "micro"),  # Inverser les valeurs ici
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
  
  observeEvent(input$toggle_select, {
    if (current_state()) {
      updateCheckboxGroupInput(session, "categories", selected = character(0))
    } else {
      updateCheckboxGroupInput(session, "categories", selected = all_categories)
    }
    current_state(!current_state())  # Toggle the state
  })
  
  output$network <- renderVisNetwork({
    selected_node <- as.character(input$node_id)
    selected_categories <- input$categories
    show_non_direct_val <- as.logical(input$non_direct_links)
    mode <- input$graph_mode
    
    # Sélectionner les objets en fonction du mode (micro ou meso)
    if (mode == "micro") {
      reseau_citation_igraph <- microreseau_citation_igraph
      graph_nodes_df <- micrograph_nodes_df
      graph_edgelist_df <- micrograph_edgelist_df
    } else {
      reseau_citation_igraph <- mesoreseau_citation_igraph
      graph_nodes_df <- mesograph_nodes_df
      graph_edgelist_df <- mesograph_edgelist_df
    }
    
    # Filtrer les noeuds et les liens pour afficher seulement ceux à une profondeur de 1 du noeud sélectionné
    subgraph_nodes <- neighborhood(reseau_citation_igraph, order = 1, nodes = selected_node, mode = "all")[[1]]
    subgraph <- induced_subgraph(reseau_citation_igraph, subgraph_nodes)
    
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
      visGroups(groupname = "Auteurs", color = "#6CC7B3", shape = "square") %>%
      visGroups(groupname = "Autorités", color = "#285291", shape = "square") %>%
      visGroups(groupname = "CCNE", color = "#9D3A5E", shape = "triangle") %>%
      visGroups(groupname = "Comité d'éthique", color = "#579125", shape = "square") %>%
      visGroups(groupname = "Comparaison pays", color = "#0F0F5C", shape = "square") %>%
      visGroups(groupname = "Etat", color = "#91188F", shape = "square") %>%
      visGroups(groupname = "Forums", color = "#0E405C", shape = "square") %>%
      visGroups(groupname = "Loi", color = "#915B11", shape = "square") %>%
      visGroups(groupname = "Org Internationales", color = "#4F2B91", shape = "square") %>%
      visGroups(groupname = "Presse", color = "#91181E", shape = "square") %>%
      visGroups(groupname = "Science, littérature", color = "#0B5C2D", shape = "square") %>%
      visGroups(groupname = "Société", color = "#BD6345", shape = "square") %>%
      visGroups(groupname = "Avis étudié", color = "#4269A0", shape = "star", 
                font = list(size = 30, color = "#4269A0")) %>%
      addFontAwesome() %>%
      visLegend(useGroups = TRUE, position = "right", width = 0.2, ncol = 1,
                main = list(text = "Catégorie de citations",
                            style = "color:#000000;font-size:24px;text-align:center;")) %>%
      visEdges(arrows = 'to', color = list(color = "gray", highlight = "black")) %>%
      visNodes(font = list(size = 20))
  })
  
  observeEvent(input$explanation, {
    showModal(modalDialog(
      title = "Explication de l'application",
      HTML("<div><h4>Explication de l'application :</h4>
        <p>Cette application permet de visualiser le graphe des citations autour de chaque avis émis par le Comité Consultatif National d'Éthique (CCNE).</p>
        <p>Sélectionner un numéro d'avis permet de choisir l'avis à regarder.</p>
        <p>On peut choisir les types de citations à afficher parmi :</p>
        <ul>
          <li>Science, littérature : livres et articles scientifiques. Oeuvres de fictions ou autobiographiques, regroupés par domaine.</li>
          <li>Presse : Journeaux de la presse quotidienne et Bulletins spécialisés (médicaux ou en santé publique).</li>
          <li>Etat : Ministères, Parlement, Président, Premier Ministre, Administration (à l'exclusion des autorités indépendantes et/ou consultatives)</li>
          <li>Auteurs : Auteurs considérés comme suffisamment importants pour apparaitre en propre, inclus des membres du CCNE et des 'classiques'.</li>
          <li>Loi : Lois votées au Parlement, regroupées par thématique et Codes de lois.</li>
          <li>Forums : Organisations consultatives associées à une posture de conseils au gouvernement et d'expertise (Forums Hybride, Callon et al. 2001) comme le CCNE, les académies ou l'Ordre des médecins.</li>
          <li>Comité d'éthique : Tout autre comité d'éthique, français ou d'un autre pays.</li>
          <li>Org Internationales : Toutes les organisations internationales incluant : institutions européennes, onusiennes, économiques/financières, olympiques.</li>
          <li>Comparaison pays : Pays qui font l'objet d'une comparaison dans l'un des avis.</li>
          <li>Société : Société civile, regroupée dans des catégories simples : collectifs, syndicats, religion, fondations caritatives</li>
          <li>Autorités : Autorités administratives publics ou indépendantes, au sens de la loi de 2016, qui un pouvoir de contrôle effectif.</li>
          <li>CCNE : Ensemble des avis numérotés publiés par le CCNE, à l'exclusion de celui choisi pour être le centre du graphe.</li>
        </ul>
        <p>Deux options sont disponibles :</p>
        <ul>
        <li>Liens non directs sont les liens des citations autre que le noeud centrale. On peut les masquer </li>
        <li>Mode du graphe permet de choisir entre les noms complets des citations, ou des regroupements plus simples.</li>
        </ul>
        </div>")
      ,
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
