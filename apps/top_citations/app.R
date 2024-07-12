library(shiny)
library(dplyr)
library(tidyr)
library(igraph)
library(shinyjs)

is_number <- Vectorize(function(x) {
  grepl("^-?\\d*(\\.\\d+)?$", x)
})

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
  useShinyjs(),
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
                   choices = list("Noms détaillés" = "micro", "Noms simplifiés" = "meso"), 
                   selected = "micro"),
    ),
    mainPanel(
      htmlOutput("top_cited_single"),
      htmlOutput("top_cited_multiple"),
      uiOutput("explanation_text")  # Espace pour l'explication
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
  
  observeEvent(input$top_category, {
    if (input$top_category == "CCNE") {
      updateRadioButtons(session, "degree_mode", choices = list("Avis les plus cités" = "in", "Avis qui citent le plus" = "out"), selected = "in")
      shinyjs::disable("name_choice")
    } else {
      updateRadioButtons(session, "degree_mode", choices = list("Avis les plus cités" = "in"), selected = "in")
      shinyjs::enable("name_choice")
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
  
  observe(if(input$top_category != "CCNE"){
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
  }else{
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
        "<div><h4>Top ", top_n, " des avis les plus cités pour ", top_category, ", parmi la plage sélectionnée, en comptant une citation par avis :</h4>",
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
        filter(is_number(to)) %>%
        group_by(from) %>%
        summarise(citations = n_distinct(to))
      
      top_nodes <- filtered_nodes %>%
        filter(Categorie == top_category) %>%
        left_join(citation_count, by = c("name" = "from")) %>%
        arrange(desc(citations)) %>%
        top_n(top_n, citations) %>%
        select(name, citations)
      
      top_html <- paste0(
        "<div><h4>Top ", top_n, " des avis qui citent le plus pour ", top_category, ", parmi la plage sélectionnée, en comptant de multiples citations par avis :</h4>",
        "<ul>",
        paste0(
          "<li>", top_nodes$name, " (", top_nodes$citations, " citations)</li>",
          collapse = ""
        ),
        "</ul></div>"
      )
      
      HTML(top_html)
    })  
  })
  
  observeEvent(input$explanation, {
    showModal(modalDialog(
      title = "Explication de l'application",
      HTML("<div><h4>Explication de l'application :</h4>
        <p>Cette application permet de visualiser les documents les plus cités au sein des avis émis par le Comité Consultatif National d'Éthique (CCNE).</p>
        <p>En sélectionnant une plage d'avis et une catégorie, l'application génère deux listes :</p>
        <ul>
          <li>Top des documents les plus cités en comptant une citation par avis.</li>
          <li>Top des documents les plus cités en comptant de multiples citations par avis.</li>
        </ul>
        <p>La plage d'avis permet de se restreindre à certains avis et à leurs citations.<p>
        <p>Les six noms de présidents du CCNE permet de restreindre la plage des avis publiés sous leur mandat.<p>
        <p>Il est aussi possible de choisir entre avoir le nom détaillé des documents cités (sauf pour les références scientifiques) ou avoir des agrégations de documents rendus la lecture plus intelligible.<p>
        <p>Dans le cas où l'option CCNE est choisi, on s'intéresse au réseau des citations internes entre avis.<p>
        <p>L'option Noms détaillés/Simplifiés est alors caduc, l'application génère deux listes :<p>
        <ul>
          <li>Top des avis les plus cités (en comptant une citation par avis).</li>
          <li>Top des avis qui citent le plus d'autres avis (en comptant de multiples citations par avis).</li>
        </ul>
       <p>On peut choisir les catégories de citations à afficher parmi :</p>
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
