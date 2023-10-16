# predicted vs observed parent income rank graph
prediction_rank_graph <- function(dt = fs_predict[[1]],
                                  caption_lab = caption_fct(fs = fs_predict[[1]][[8]],
                                                            instru_labels = instru_labs[[1]]),
                                  annotation_size = 3,
                                  by_gender = F) {
  
  if (by_gender == T) {
    dt <- dt %>% 
      group_by(gender, rank_parent) %>%
      summarise(mean_rank_parent_predict = mean(rank_parent_predict),
                p25_rank_parent_predict = quantile(rank_parent_predict, probs = .25),
                p75_rank_parent_predict = quantile(rank_parent_predict, probs = .75))
  } else {
    dt <- dt %>% 
      group_by(rank_parent) %>%
      summarise(mean_rank_parent_predict = mean(rank_parent_predict),
                p25_rank_parent_predict = quantile(rank_parent_predict, probs = .25),
                p75_rank_parent_predict = quantile(rank_parent_predict, probs = .75))
  }
  
  graph <- dt %>%
    ggplot(aes(x = rank_parent, y = mean_rank_parent_predict)) +
    geom_point(alpha = 0.8) +
    geom_ribbon(aes(ymin = p25_rank_parent_predict, ymax = p75_rank_parent_predict), alpha = 0.25) +
    # annotate("text", x = 70, y = 96, label = "interquartile range", hjust = 0, color = swatch()[2]) +
    # annotate("text", x = 1, y = 99, label = caption_lab[2], hjust = 0, vjust = 1, size = annotation_size) +
    geom_abline(slope = 1) +
    scale_x_continuous(expand = c(0.01,0.01)) +
    scale_y_continuous(lim = c(0,100), expand = c(0.01, 0.01)) +
    # scale_x_continuous(lim = c(0,100), expand = c(0.01, 0.01)) +
    labs(x = "Observed family labor income",
         y = "Predicted family labor income",
         title = caption_lab[1],
         caption = caption_lab[2])
  
  if (by_gender == T) {
    graph <- graph +
      facet_wrap(vars(gender))
  }
  
  return(graph)
}
