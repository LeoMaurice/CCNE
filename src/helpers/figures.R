require(pacman,quietly = T)
pacman::p_load(rstatix,
               
               ggplot2,
               gridExtra,
               ggprism,
               ggpubr,
               ggcorrplot)

theme_ready <-
  function(text_size = 14,
           default_theme = theme_pubclean()) {
    return(
      default_theme +
        theme(
          text = element_text(size = text_size),
          axis.title = element_text(size = text_size + 2, face =
                                      "bold"),
          axis.text = element_text(size = text_size),
          legend.title = element_text(size = text_size),
          legend.text = element_text(size = text_size - 2)
        )
    )
  }

t_test_pvalue <-
  function(
    mapping = NULL,
    data = NULL,
    group.by_var,
    p.adjust.method = "BH",
    cutpoints = c(0, 1e-04, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("****", "***", "**", "*", "•", "ns"),
    significance = c("p < 0.0001", "p < 0.001", "p < 0.01", 
                    "p < 0.05", "p < 0.1", "p>0.1"),
    inherit.aes = TRUE
    ) {

    y_var <- deparse(mapping$y)
    x_var <- deparse(mapping$x)
    
    signif_caption_text <- paste("Significance levels:\n",
                                paste(symbols, ":", significance, collapse = ", "))
    
    test_result.t <- data |>
    group_by(.add = group.by_var) |>
    t_test(as.formula(paste(y_var, "~", x_var)), 
           p.adjust.method = p.adjust.method) |> 
    add_significance(cutpoints = cutpoints,
                     symbols = symbols) |>
    add_xy_position()
  
  return(
    stat_pvalue_manual(
      test_result.t,
      label = "p.adj.signif",
      hide.ns = T,
      step.group.by = group.by_var
    )+
      labs(subtitle = signif_caption_text)
  )
  }

# Define the ggproto object for TTest annotation
StatTTest <- ggproto("StatTTest", Stat,
                     compute_group = function(data, scales, group.by_var, p.adjust.method = "BH",
                                              cutpoints = c(0, 1e-04, 0.001, 0.01, 0.05, 0.1, 1),
                                              symbols = c("****", "***", "**", "*", "•", "ns"),
                                              significance = c("p < 0.0001", "p < 0.001", "p < 0.01", 
                                                               "p < 0.05", "p < 0.1", "p>0.1")) {
                       # Perform the t-test
                       test_result <- data %>%
                         group_by(.data[[group.by_var]]) %>%
                         t_test(as.formula(paste(dep(parse_aes(data)$y), "~", dep(parse_aes(data)$x)))) %>%
                         adjust_pvalue(method = p.adjust.method) %>%
                         add_significance('p.adj', cutpoints = cutpoints, symbols = symbols) %>%
                         add_xy_position(dodge = 0.8)
                       
                       # Add significance levels as an attribute
                       attr(test_result, "signif_caption_text") <- paste("Significance levels:\n",
                                                                         paste(symbols, ":", significance, collapse = ", "))
                       
                       test_result
                     },
                     
                     compute_panel = function(data, scales, ...) {
                       data
                     },
                     
                     required_aes = c("x", "y", "group")
)

# Function to initiate StatTTest within ggplot
stat_t_test <- function(mapping = NULL, data = NULL, geom = "text", position = "identity",
                        ..., group.by_var, inherit.aes = TRUE) {
  layer(
    stat = StatTTest, data = data, mapping = mapping, geom = geom, position = position,
    show.legend = NA, inherit.aes = inherit.aes,
    params = list(group.by_var = group.by_var, ...)
  )
}

# Helper function to parse the aes mappings to strings
parse_aes <- function(data) {
  lapply(data$aes_params, deparse)
}

# Helper function for deparsing aes elements
dep <- function(x) {
  if (length(x) == 1) {
    return(deparse(x))
  }
  x
}