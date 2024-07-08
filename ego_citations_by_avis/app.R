# Charger les bibliothèques nécessaires
require(pacman,quietly = T)

pacman::p_load(shiny,
               tidyverse,
               igraph,
               networkD3,
               visNetwork,
               tidygraph,
               leaflet)

source("../src/helpers/database_creation.R")
source("../src/helpers/figures.R")

# Ouverture données

designation_recoding <- Vectorize(function(type, Auteur_Institution, Institution_Domaine, freq_auteur, freq) {
  if (type == "classic" && freq_auteur >= 2) {
    return(Auteur_Institution)
  } else if (type == "collectif") {
    return("Collectif")
  } else if (type == "presse spécialisée" && freq < 2) {
    return("Presse Spécialisée diverse")
  } else if (type == "presse" && freq < 2) {
    return("Presse diverse")
  } else if (type == "loi" && freq < 2) {
    return("Loi diverse")
  } else if (type == "comité d'éthique" && freq < 2) {
    return("Comité d'éthique divers")
  } else if (type == "autre pays" && freq < 2) {
    return("Comité d'éthique divers")
  } else {
    return(Institution_Domaine)
  }
})


metadata_avis <- open_metadata()

metadata_citation <- read_excel("../data/raw/collected_metadata/metadata_citation.xlsx", 
                                col_types = c("text", "text", "skip", 
                                              "text", "text", "text", "skip", "skip", 
                                              "skip"))%>%
  separate_rows(cité_dans_avis_num, sep = ";")%>%
  separate_rows(Institution_Domaine, sep = ";")%>%
  group_by(Auteur_Institution)%>%
  mutate(freq_auteur = n())%>%
  ungroup()%>%
  group_by(Institution_Domaine)%>%
  mutate(freq_institution = n())%>%
  ungroup()%>%
  mutate(type = ifelse(is.na(type),"",type),
         type = ifelse(type %in% c("livre","article","colloque","thèse"),"académique",type),
         Désignation = designation_recoding(type,Auteur_Institution, Institution_Domaine, freq_auteur,freq_institution)
  )

recat <- read_excel("../data/intermediate/reseau/recategorize_designation.xlsx")%>%
  select(-count)

metadata_citation <- metadata_citation%>%
  left_join(recat, by = "Désignation")%>%
  rename(Precise_Désignation = Désignation,
         Désignation = Rename)

# Création des dataframes nécessaires
reseau_citation_extra <- metadata_citation %>%
  rename(Avis = cité_dans_avis_num) %>%
  group_by(Avis, Désignation, Groupe) %>%
  summarise(Citations = n()) %>%
  rename(from = Avis,
         to = Désignation,
         Categorie = Groupe) %>%
  mutate(shape = "Extra citation")

reseau_citation_intra <- metadata_avis %>%
  filter(!(is.na(citations_autres_avis))) %>%
  separate_rows(citations_autres_avis, sep = ";") %>%
  rename(from = num,
         to = citations_autres_avis) %>%
  group_by(from, to) %>%
  summarise(Citations = n()) %>%
  mutate(shape = "Intra citation",
         Categorie = "CCNE",
         from = as.character(from))

mesograph_edgelist_df <- bind_rows(reseau_citation_extra, reseau_citation_intra) %>%
  filter(!(to == "covid"))  # supprimer les avis covid pour l'instant

mesograph_nodes_df <- bind_rows(
  metadata_citation %>%
    rename(name = Désignation, Categorie = Groupe) %>%
    mutate(shape = "Extra citation") %>%
    select(name, Categorie, shape) %>%
    distinct(),
  metadata_avis %>%
    mutate(name = as.character(num),
           shape = "Intra citation",
           Categorie = "CCNE") %>%
    select(name, Categorie, shape) %>%
    distinct()
) %>%
  filter(!(is.na(name)))

# Création de l'objet tbl_graph
reseau_citation <- tbl_graph(edges = mesograph_edgelist_df %>% 
                               select(from, to, Citations, Categorie, shape),
                             nodes = mesograph_nodes_df %>%
                               select(name, Categorie, shape),
                             directed = TRUE, node_key = "name") %>%
  activate(nodes) %>%
  filter(degree(.) > 0)

# Convertir l'objet tbl_graph en igraph
reseau_citation_igraph <- as.igraph(reseau_citation)

library(shiny)
library(dplyr)
library(tidyr)
library(igraph)
library(visNetwork)

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

# UI
ui <- fluidPage(
  titlePanel("Visualisation du Réseau de Citations"),
  sidebarLayout(
    sidebarPanel(
      selectInput("node_id", "Sélectionner le noeud (num):", 
                  choices = sort(as.numeric(mesograph_nodes_df$name[mesograph_nodes_df$Categorie == "CCNE"])), 
                  selected = min(as.numeric(mesograph_nodes_df$name[mesograph_nodes_df$Categorie == "CCNE"]), na.rm = TRUE)),
      checkboxGroupInput("categories", "Choisir les catégories à afficher:",
                         choices = unique(mesograph_nodes_df$Categorie),
                         selected = unique(mesograph_nodes_df$Categorie)),
      actionButton("toggle_select", "Décocher/Recocher toutes les catégories")
    ),
    mainPanel(
      visNetworkOutput("network", height = "800px")
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
  
  output$network <- renderVisNetwork({
    selected_node <- as.character(input$node_id)
    selected_categories <- input$categories
    
    # Filtrer les noeuds et les liens pour afficher seulement ceux à une profondeur de 1 du noeud sélectionné
    subgraph_nodes <- neighborhood(reseau_citation_igraph, order = 1, nodes = selected_node, mode = "all")[[1]]
    subgraph <- induced_subgraph(reseau_citation_igraph, subgraph_nodes)
    
    # Filtrer par catégories sélectionnées
    subgraph <- delete.vertices(subgraph, V(subgraph)[!(V(subgraph)$Categorie %in% selected_categories)])
    
    # Convertir le sous-graph en format compatible avec visNetwork
    nodes <- data.frame(id = V(subgraph)$name, label = V(subgraph)$name, group = V(subgraph)$Categorie)
    edges <- as.data.frame(get.data.frame(subgraph, what = "edges"))
    
    # Appliquer les couleurs fixes aux catégories
    nodes$color <- category_colors[nodes$group]
    
    # Mettre en gras les liens à partir du noeud sélectionné et les autres en pointillé
    edges$color <- ifelse(edges$from == selected_node | edges$to == selected_node, "black", "gray")
    edges$dashes <- ifelse(edges$from == selected_node | edges$to == selected_node, FALSE, TRUE)
    edges$width <- ifelse(edges$from == selected_node | edges$to == selected_node, 2, 1)
    
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
      addFontAwesome()%>%
      visLegend(useGroups = TRUE, position = "right", width = 0.2, ncol = 2,
                main = list(text = "Type de citations",
                            style = "color:#000000;font-size:12px;text-align:center;")) %>%
      visEdges(arrows = 'to', color = list(color = "gray", highlight = "black")) %>%
      visNodes(font = list(size = 20))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)






