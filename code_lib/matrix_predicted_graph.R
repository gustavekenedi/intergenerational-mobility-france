# compare transition matrices
matrix_predicted_graph <- function(dt = fs_predict[[1]][[1]],
                                   caption_lab = caption_fct(fs = fs_predict[[1]][[8]],
                                                             instru_labels = instru_labs[[1]]),
                                   ols_legend_none = T,
                                   tstsls_legend_none = F,
                                   ols_caption = F,
                                   title_size = 10,
                                   legend_size = 9,
                                   pct_size = 4,
                                   n_dodge = 1,
                                   keep_data = F) {
  dt <- dt %>% 
    mutate(across(c(rank_parent, rank_parent_predict, rank_child), ~ case_when(. %in% 1:20 ~ "Bottom 20%",
                                                                               . %in% 21:40 ~ "Quintile 2",
                                                                               . %in% 41:60 ~ "Quintile 3",
                                                                               . %in% 61:80 ~ "Quintile 4",
                                                                               . %in% 81:100 ~ "Top 20%"),
                  .names = "{.col}_quintile"),
           rank_parent_quintile = factor(rank_parent_quintile, levels = c("Bottom 20%", "Quintile 2", "Quintile 3", "Quintile 4", "Top 20%")),
           rank_parent_predict_quintile = factor(rank_parent_predict_quintile, levels = c("Bottom 20%", "Quintile 2", "Quintile 3", "Quintile 4", "Top 20%")),
           rank_child_quintile = factor(rank_child_quintile, levels = c("Bottom 20%", "Quintile 2", "Quintile 3", "Quintile 4", "Top 20%")))
  
  # OLS
  dt_ols <- dt %>%
    group_by(rank_parent_quintile, rank_child_quintile) %>% 
    summarise(n_obs = n()) %>% 
    group_by(rank_parent_quintile) %>% 
    mutate(pct = n_obs/sum(n_obs)) %>% 
    ungroup()
  
  graph_ols <- dt_ols %>%
    ggplot(aes(x = rank_parent_quintile, y = pct, fill = fct_rev(rank_child_quintile))) +
    geom_col(position = "fill") +
    geom_text(aes(label = paste0(round(pct*100,2), "%")), position = position_stack(vjust = 0.5),
              show.legend = FALSE, color = "white", size = pct_size) +
    scale_fill_manual(values = alpha(my_palette[c(5,4,3,2,1)], alpha), guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(expand = c(0,0), labels = scales::percent) +
    scale_x_discrete(expand = c(0,0), guide = guide_axis(n.dodge = n_dodge)) +
    labs(x = "Parent income quintile",
         y = NULL,
         fill = "Child income quintile:",
         title = "Observed") +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          legend.text = element_text(size = legend_size))
  
  # TSLTS
  dt_tstsls <- dt %>%
    group_by(rank_parent_predict_quintile, rank_child_quintile) %>% 
    summarise(n_obs = n()) %>% 
    group_by(rank_parent_predict_quintile) %>% 
    mutate(pct = n_obs/sum(n_obs)) %>% 
    ungroup()
  
  graph_tstsls <- dt_tstsls %>%
    ggplot(aes(x = rank_parent_predict_quintile, y = pct, fill = fct_rev(rank_child_quintile))) +
    geom_col(position = "fill") +
    geom_text(aes(label = paste0(round(pct*100,2), "%")), position = position_stack(vjust = 0.5),
              show.legend = FALSE, color = "white", size = pct_size) +
    scale_fill_manual(values = alpha(my_palette[c(5,4,3,2,1)], alpha), guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(expand = c(0,0), labels = scales::percent) +
    scale_x_discrete(expand = c(0,0), guide = guide_axis(n.dodge = n_dodge)) +
    labs(x = "Parent income quintile",
         y = NULL,
         fill = "Child income quintile:",
         title = "TSTSLS") +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          legend.text = element_text(size = legend_size))
  graph_tstsls
  
  if (ols_legend_none == T) {
    graph_ols <- graph_ols +
      theme(legend.position = "none")
  }
  
  if (tstsls_legend_none == T) {
    graph_tstsls <- graph_tstsls +
      theme(legend.position = "none")
  }
  
  graph_combined <- graph_ols + graph_tstsls +
    plot_annotation(title = caption_lab[1],
                    caption = caption_lab[2]) +
    plot_layout(guides = "collect")
  
  if (ols_caption == T) {
    graph_ols <- graph_ols +
    labs(caption = caption_lab[2])
  }
  
  graph_tstsls <- graph_tstsls +
    labs(title = caption_lab[1],
         caption = caption_lab[2]) +
    theme(plot.title = element_text(size = title_size))
  
  graph_combined <- wrap_elements(graph_combined)
  
  if (keep_data == F) {
    return(list(graph_combined, graph_ols, graph_tstsls))
  } else {
    return(bind_rows(dt_ols |> mutate(method = "ols"),
                     dt_tstsls |> mutate(method = "tstsls") |> rename(rank_parent_quintile = rank_parent_predict_quintile)))
  }
}
