# predicted vs observed parent income graph
prediction_graph <- function(dt = fs_predict[[1]][[1]],
                             regional = F,
                             region_labels = region_labs,
                             caption_lab = caption_fct(fs = fs_predict[[1]][[8]],
                                                       instru_labels = instru_labs[[1]]),
                             by_gender = F,
                             annotation_size = 3) {
  
  
  
  if (regional == T) {
    caption_lab <- paste0("Region: ", region_labels, "\n", caption_lab)
  }
  
  max_inc <- dt %>% summarise(max(parent_inc)) %>% pull()
  min_inc <- min(dt %>% summarise(min(parent_inc)) %>% pull(),
                 dt %>% summarise(min(parent_inc_predict)) %>% pull())
  
  graph <- dt %>%
    ggplot(aes(x = parent_inc, y = parent_inc_predict)) +
    geom_point(alpha = 0.8) +
    # annotate("text", x = min_inc, y = max_inc, label = caption_lab[2], hjust = 0, vjust = 1, size = annotation_size) +
    geom_abline(slope = 1) +
    scale_x_continuous(expand = c(0.01,0.01), labels = scales::comma) +
    scale_y_continuous(lim = c(min_inc, max_inc), expand = c(0.01,0.01), labels = scales::comma) +
    labs(x = "Observed family labor income",
         y = "Predicted family labor income",
         title = caption_lab[1],
         caption = caption_lab[2])
  
  if(by_gender == T) {
    graph <- graph +
      facet_wrap(vars(gender))
  }
  
  return(graph)
}
