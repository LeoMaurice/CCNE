library(tidyverse)
library(stringr)
library(quanteda)
library(ggprism)
library(ggpubr)
library(kableExtra)
library(udpipe)
library(writexl)

source("./additional/utils/database_creation.R")

is.lemma = TRUE

base_avis_ccne <- open_avis(rescrap_texte = FALSE, lemmatized = is.lemma)

# lemma <- lemmatization(base_avis_ccne, redo = TRUE) # compute lemmatization

join_metadata(base_avis_ccne) -> base_avis_ccne

tk <- tokenisation_custom(base_avis_ccne)

mot = "éthique"

suffix <- ifelse(is.lemma, "_lemma","")

# contexte suivant le mot progrès  ----
kwic(tk, mot, window=2) |> 
  as.data.frame() |> 
  select("post") |> 
  table() |>
  sort(decreasing = TRUE) |>
  head(20)|> 
  as.data.frame()|>
  mutate(post = tolower(post)) |>
  rename(`Contexte après` = post, `Fréquence` = Freq) |>
  writexl::write_xlsx(str_c("./additional/output/post_", mot, suffix, ".xlsx"))

# contexte précèdant le mot progrès ----
kwic(tk, mot, window=2) |> 
  as.data.frame() |> 
  select("pre") |> 
  table() |>
  sort(decreasing = TRUE) |>
  head(20)|> 
  as.data.frame()|>
  mutate(pre = tolower(pre)) |>
  rename(`Contexte avant` = pre, `Fréquence` = Freq) |>
  writexl::write_xlsx(str_c("./additional/output/pre_", mot, suffix, ".xlsx"))

# DFM format ----
dfm <- to_dfm_custom(tk)

dfm$nb_mots <- rowSums(dfm)

# Mots au cours des avis ----

important_words<- c("progrès", "éthique")

freq <- dfm |>
  dfm_keep(pattern = important_words) |>
  textstat_frequency(groups = num) |> 
  mutate(num = as.numeric((group)),
         feature = factor(feature, levels = important_words))|>
  left_join(dfm@docvars, by = "num")|> 
  mutate(occurences = frequency,
    frequency = 100 * frequency / nb_mots)
  
g1 <- freq |>
  filter(feature == "progrès") |>
  ggpubr::ggscatter(x="Date", y = "frequency", xlab = "Temps",ylab = "Fréquence (en % du nombre de mots)", title = "")+
  geom_smooth(formula = 'y ~ x', method = "loess", se = FALSE, color = "#C000C0") +  # Lissage LOESS
  theme_prism() +
  theme(
   text = element_text(size = 30) 
  )

g2 <- freq |>
  filter(feature == "éthique") |>
  ggpubr::ggscatter(x="Date", y = "frequency", xlab = "Temps",ylab = "Fréquence (en % du nombre de mots)", title = "")+
  geom_smooth(formula = 'y ~ x', method = "loess", se = FALSE, color = "#C000C0") +  # Lissage LOESS
  theme_prism() +
  theme(
    text = element_text(size = 30) 
  )

cowplot::plot_grid(g1, g2, labels = c("Progrès", "Ethique"), label_size = 35) 

g3 <- freq |>
  filter(feature == "progrès") |>
  ggpubr::ggscatter(x="Date", y = "occurences", xlab = "Temps",ylab = "Nombre d'occurences", title = "")+
  geom_smooth(formula = 'y ~ x', method = "loess", se = FALSE, color = "#C000C0") +  # Lissage LOESS
  theme_prism() +
  theme(
    text = element_text(size = 30) 
  )

g4 <- freq |>
  filter(feature == "éthique") |>
  ggpubr::ggscatter(x="Date", y = "occurences", xlab = "Temps",ylab = "Nombre d'occurences", title = "")+
  geom_smooth(formula = 'y ~ x', method = "loess", se = FALSE, color = "#C000C0") +  # Lissage LOESS
  theme_prism() +
  theme(
    text = element_text(size = 30) 
  )

cowplot::plot_grid(g3, g4, labels = c("Progrès", "Ethique"), label_size = 35)


lemma |>
  group_by(doc_id, sentence_id)|>
  mutate(nb_mots = n_distinct(token_id)) |>
  filter(tolower(lemma) %in% c("progrès", "éthique")) |>
  mutate(token_id = as.integer(token_id)) |>
  pivot_wider(id_cols = c(doc_id, sentence_id),
              names_from = lemma, values_from = token_id)
  