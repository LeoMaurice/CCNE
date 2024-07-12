library(shiny)
library(dplyr)
library(tidyr)
library(igraph)
library(visNetwork)

# Vérifier si le fichier existe avant de le charger
if (file.exists("network_data.RData")) {
  load("network_data.RData")
} else {
  stop("Le fichier 'network_data.RData' est manquant.")
}

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
      #explanationButton {
        position: fixed;
        top: 50px;
        right: 10px;
        z-index: 1000;
      }    "))
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
  titlePanel("Visualisation du Réseau de Citations du CCNE"),
  sidebarLayout(
    sidebarPanel(
      div(style = "display: flex; align-items: center; justify-content: space-between;",
          radioButtons("toggle_network", "Réseau:",
                       choices = list("Afficher" = TRUE, "Cacher" = FALSE), 
                       selected = TRUE),
          htmlOutput("legend")
      ),
      sliderInput("avis_range", "Sélectionner la plage des avis à afficher (par numéro de publication, dans l'odre chronologique):",
                  min = min(as.numeric(mesograph_nodes_df$name), na.rm = TRUE),
                  max = max(as.numeric(mesograph_nodes_df$name), na.rm = TRUE),
                  value = c(125, 144),  # Valeurs par défaut du slider
                  step = 1),
      br(),
      h4("Sélectionnez un président pour ajuster le slider avec les avis publiés sous son mandat :"),
      uiOutput("president_buttons"),
      checkboxGroupInput("categories", "Choisir les catégories à afficher:",
                         choices = unique(mesograph_nodes_df$Categorie),
                         selected = c("Loi", "CCNE")),
      actionButton("toggle_select", "Décocher/Recocher toutes les catégories")
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.toggle_network == 'TRUE'",
        visNetworkOutput("network", height = "800px")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  all_categories <- unique(mesograph_nodes_df$Categorie)
  current_state <- reactiveVal(TRUE)  # Initial state is all categories selected
  
  observeEvent(input$toggle_select, {
    if (current_state()) {
      updateCheckboxGroupInput(session, "categories", selected = character(0))
    } else {
      updateCheckboxGroupInput(session, "categories", selected = all_categories)
    }
    current_state(!current_state())  # Toggle the state
  })
  
  output$networkVisible <- reactive({
    input$toggle_network == TRUE
  })
  outputOptions(output, "networkVisible", suspendWhenHidden = FALSE)
  
  output$network <- renderVisNetwork({
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
    
    # Créer une correspondance entre les noms et les indices pour visNetwork
    nodes <- filtered_nodes %>%
      left_join(node_degree, by = "name") %>%
      mutate(id = name,
             label = name,
             group = Categorie,
             value = degree,  # Taille variable en fonction du nombre d'edges connectés
             title = name,
             color = category_colors[Categorie]) %>%
      select(id, label, group, value, title, color)
    
    edges <- filtered_edges %>%
      rename(from = from, to = to) %>%
      select(from, to)
    
    visNetwork(nodes, edges) %>%
      visEdges(arrows = 'to') %>%
      visNodes(scaling = list(min = 10, max = 30)) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1), nodesIdSelection = TRUE) %>%
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
      visGroups(groupname = "Société", color = "#BD6345", shape = "square")
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
  
  output$president_buttons <- renderUI({
    buttons <- lapply(1:nrow(max_min_par_president), function(i) {
      actionButton(paste0("president_", i), max_min_par_president$president[i])
    })
    do.call(tagList, buttons)
  })
  
  observe({
    lapply(1:nrow(max_min_par_president), function(i) {
      observeEvent(input[[paste0("president_", i)]], {
        updateSliderInput(session, "avis_range", value = c(max_min_par_president$avis_min[i], max_min_par_president$avis_max[i]))
      })
    })
  })
  
  observeEvent(input$explanation, {
    showModal(modalDialog(
      title = "Explication de l'application",
      HTML("<div><h4>Explication de l'application :</h4>
        <p>Cette application permet de visualiser le graphe des citations des avis émis par le Comité Consultatif National d'Éthique (CCNE).</p>
        <p>La plage d'avis permet de se restreindre à certains avis et à leurs citations. On peut choisir le premier avis à prendre en compte et le dernier</p>
        <p>Les six noms de présidents du CCNE permet de restreindre la plage des avis publiés sous leur mandat.</p>
        <p>On peut choisir les types de citations à afficher parmi :</p>
        <ul>
          <li>Science, littérature : livres et articles scientifiques, Oeuvres de fictions ou autobiographiques, regroupés par domaine.</li>
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
          <li>CCNE : Ensemble des avis numérotés publiés par le CCNE.</li>
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
