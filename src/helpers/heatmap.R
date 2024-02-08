source("helpers/df_to_matrix.R")
# X = variable en x, Y var en Y, Zs = liste des noms des variables à mettre en couleur dans chacune des heatmap
heatmap_figure_frequence_quantile <- function(df_heatmap,
                                              X,Y,Zs,
                                              names_legend,
                                              subrow_title,
                                              subcol_title,
                                              mainrow_title,
                                              maincol_title,
                                              colors_hexcodes,
                                              xtrem_values = c(0,1)
                                              ){

  # metadonnées : pas changeable dans la fct pour l'instant
  
  # Préparation des annotations
  colors_palette <-
    ggprism::scale_colour_prism(palette = "colors")$palette
  pearl_palette <-
    ggprism::scale_colour_prism(palette = "office")$palette
  theme_colors <-
    setNames(colors_palette(length(unique(df_heatmap$theme))), unique(df_heatmap$theme))
  president_colors <-
    setNames(pearl_palette(length(unique(
      df_heatmap$president
    ))), unique(df_heatmap$president))
  
  top_annotation <- HeatmapAnnotation(
    df = df_heatmap |>
      group_by(num, president, theme) |>
      summarise() |>
      ungroup() |>
      arrange(num) |>
      select(-num),
    "nombre phrase" = anno_barplot(
      df_heatmap |>
        group_by(num, number_sentences) |>
        summarise() |>
        ungroup() |>
        arrange(num) |>
        select(-num) |>
        as.vector(),
      add_numbers = TRUE,
      height = unit(3,"cm"),
      gp = gpar(fill ="#285291"),
      numbers_rot = 90
    ),
    which = "column",
    show_legend = TRUE,
    annotation_name_side = "right",
    col = list(president = president_colors,
               theme = theme_colors)
  )
  
  heatmap_list = top_annotation

  for(i in 1:length(Zs)){
    matrix <- df_to_matrix(df_heatmap,
                           Xname = X,
                           Yname = Y,
                           Zname = Zs[i])

    heatmap <- Heatmap(
      matrix,
      name = names_legend[i],
      row_title = paste(subrow_title,names_legend[i]),
      column_title = ifelse(i==length(Zs),subcol_title,""),
      column_title_side = "bottom",
      col = colorRamp2(xtrem_values, c("white", colors_hexcodes[i])),
      column_names_gp = gpar(fontsize = 8),
      cluster_rows = FALSE,
      cluster_columns = FALSE,
    )
    heatmap_list = heatmap_list %v% heatmap
  }
  draw(heatmap_list, column_title = maincol_title,row_title = mainrow_title)
}

