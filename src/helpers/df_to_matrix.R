pacman::p_load(ComplexHeatmap,
               reshape2)

df_to_matrix <- function(df, Xname, Yname, Zname){
  df <- df|>
    rename("X" = Xname,
           "Y" = Yname,
           "Z" = Zname)
  heatmap_data <- dcast(df, Y ~ X, value.var = "Z")
  heatmap_matrix <- as.matrix(heatmap_data[, -1])
  rownames(heatmap_matrix) <- heatmap_data$quantile_group
  colnames(heatmap_matrix) <- colnames(heatmap_data)[-1]
  return(heatmap_matrix)
}

df_to_heatmapannotation <- function(df,metadataName,XorYname){
  #does not work
  return(df|>
          rename("var"=XorYname)|>
          group_by(c("var",metadataName))|>
          summarise()|> 
          ungroup()|>
          arrange("var")|>
          select(-`"var"`))
}