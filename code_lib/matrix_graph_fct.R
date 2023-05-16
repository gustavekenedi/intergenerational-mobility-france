matrix_graph_fct <- function(dt, facet = F) {
  graph <- dt |> 
    group_by(child_inc_var, child_inc_var_lab, parent_inc_var, parent_inc_var_lab, parent_inc_quintile, child_inc_quintile) %>% 
    summarise(n_obs = n()) %>% 
    group_by(child_inc_var, child_inc_var_lab, parent_inc_var, parent_inc_var_lab, parent_inc_quintile) %>% 
    mutate(pct = n_obs/sum(n_obs)) %>% 
    ungroup() |> 
    ggplot(aes(x = parent_inc_quintile, y = pct, fill = fct_rev(child_inc_quintile))) +
    geom_col(position = "fill") +
    geom_text(aes(y = pct, label = paste0(round(pct*100,2), "%")), position = position_fill(vjust = .5),
              show.legend = FALSE, color = "white", size = 3) +
    scale_fill_manual(values = alpha(my_palette[c(5,4,3,2,1)], alpha), guide = guide_legend(reverse = T)) +
    scale_y_continuous(expand = c(0,0), labels = scales::percent) +
    scale_x_discrete(expand = c(0,0), guide = guide_axis(n.dodge = 2)) +
    labs(x = "Parent income quintile",
         y = NULL,
         fill = "Child income\nquintile:") +
    theme(legend.position = "top",
          panel.grid.major = element_blank())
  
  if (facet == T) {
    graph <- graph +
      facet_grid(child_inc_var_lab ~ parent_inc_var_lab)
  }
  
  return(graph)
}
