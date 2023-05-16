# predicted vs observed parent individual income graph
indiv_pred_graph <- function(dt = fs_predict[[1]][[9]],
                             by_gender = T,
                             caption_lab = caption_fct(fs = fs_predict[[1]][[8]],
                                                       instru_labels = instru_labs[[1]]),
                             pct_size = 3) {
  
  # income level comparisons
  graph_level = dt %>% 
    mutate(sample = factor(gender, levels = c("Male", "Female"), labels = c("Fathers", "Mothers"))) %>%
    ggplot(aes(x = parent_inc, y = parent_inc_predict)) +
    geom_point(alpha = 0.8) +
    geom_abline(slope = 1) +
    # geom_smooth(method = "lm", color = "black", se = F) +
    scale_x_continuous(lim = c(0,15), expand = c(0,0)) +
    scale_y_continuous(lim = c(0,15), expand = c(0,0)) +
    labs(x = "Observed log labor income",
         y = "Predicted log labor income",
         title = caption_lab[1],
         caption = caption_lab[2]) +
    theme(panel.spacing.x = unit(1, "cm"))
  
  if (by_gender == T) {
    graph_level <- graph_level +
      facet_wrap(~ sample)
  }
  
  # obtain income ranks
  if (by_gender == T) {
    dt <- dt %>% 
      mutate(sample = factor(gender, levels = c("Male", "Female"), labels = c("Fathers", "Mothers"))) %>% 
      group_by(sample) %>% 
      arrange(parent_inc_predict) %>% 
      mutate(rank_parent_predict = ceiling(row_number()/n()*100)) %>%
      group_by(sample, parent_inc_predict) %>%
      mutate(rank_parent_predict = ceiling(mean(rank_parent_predict))) %>%
      group_by(sample) %>% 
      arrange(parent_inc) %>% 
      mutate(rank_parent = ceiling(row_number()/n()*100)) %>%
      group_by(sample, parent_inc) %>%
      mutate(rank_parent = ceiling(mean(rank_parent))) %>%
      ungroup()
    
    graph_rank_data <- dt |> 
      group_by(sample, rank_parent)
  } else {
    dt <- dt %>% 
      arrange(parent_inc_predict) %>% 
      mutate(rank_parent_predict = ceiling(row_number()/n()*100)) %>%
      group_by(parent_inc_predict) %>%
      mutate(rank_parent_predict = ceiling(mean(rank_parent_predict))) %>%
      ungroup() |> 
      arrange(parent_inc) %>% 
      mutate(rank_parent = ceiling(row_number()/n()*100)) %>%
      group_by(parent_inc) %>%
      mutate(rank_parent = ceiling(mean(rank_parent))) %>%
      ungroup()
    
    graph_rank_data <- dt |> 
      group_by(rank_parent)
  }
  
  # rank comparisons
  graph_rank_data <- graph_rank_data |> 
    summarise(mean_rank_parent_predict = mean(rank_parent_predict),
              p25_rank_parent_predict = quantile(rank_parent_predict, probs = .25),
              p75_rank_parent_predict = quantile(rank_parent_predict, probs = .75))
  
  graph_rank <- graph_rank_data |> 
    ggplot(aes(x = rank_parent, y = mean_rank_parent_predict)) +
    geom_point(alpha = 0.8) +
    geom_ribbon(aes(ymin = p25_rank_parent_predict, ymax = p75_rank_parent_predict), alpha = 0.25) +
    geom_abline(slope = 1) +
    scale_x_continuous(lim = c(0, 100), expand = c(0,0)) +
    scale_y_continuous(lim = c(0, 100), expand = c(0,0)) +
    labs(x = "Observed labor income rank",
         y = "Predicted labor income rank",
         title = caption_lab[1],
         caption = caption_lab[2]) +
    theme(panel.spacing.x = unit(1, "cm"))
  
  if (by_gender == T) {
    graph_rank <- graph_rank +
      facet_wrap(~ sample)
  }
    
    # rank quintile comparison
    graph_quintile_rank <- dt %>% 
      mutate(across(c(rank_parent, rank_parent_predict), ~ case_when(. %in% 1:20 ~ "Bottom 20%",
                                                                     . %in% 21:40 ~ "Quintile 2",
                                                                     . %in% 41:60 ~ "Quintile 3",
                                                                     . %in% 61:80 ~ "Quintile 4",
                                                                     . %in% 81:100 ~ "Top 20%"),
                    .names = "{.col}_quintile"),
             rank_parent_quintile = factor(rank_parent_quintile, levels = c("Bottom 20%", "Quintile 2", "Quintile 3", "Quintile 4", "Top 20%")),
             rank_parent_predict_quintile = factor(rank_parent_predict_quintile, levels = c("Bottom 20%", "Quintile 2", "Quintile 3", "Quintile 4", "Top 20%")))
    
    if (by_gender == T) {
      graph_quintile_rank <- graph_quintile_rank %>%
        group_by(sample, rank_parent_quintile, rank_parent_predict_quintile) %>% 
        summarise(n_obs = n()) %>% 
        group_by(sample, rank_parent_quintile) %>% 
        mutate(pct = n_obs/sum(n_obs)) %>% 
        ungroup()
    } else {
      graph_quintile_rank <- graph_quintile_rank %>%
        group_by(rank_parent_quintile, rank_parent_predict_quintile) %>% 
        summarise(n_obs = n()) %>% 
        group_by(rank_parent_quintile) %>% 
        mutate(pct = n_obs/sum(n_obs)) %>% 
        ungroup()
    }
    
    graph_quintile_rank <- graph_quintile_rank %>%
      ggplot(aes(x = rank_parent_quintile, y = pct, fill = fct_rev(rank_parent_predict_quintile))) +
      geom_col(position = "fill", alpha = 0.8) +
      geom_text(aes(label = paste0(round(pct*100,2), "%")), position = position_stack(vjust = 0.5),
                show.legend = FALSE, color = "white", size = pct_size) +
      scale_fill_manual(values = alpha(my_palette[c(5,4,3,2,1)], alpha), guide = guide_legend(reverse = TRUE)) +
      scale_y_continuous(expand = c(0,0), labels = scales::percent) +
      scale_x_discrete(expand = c(0,0), guide = guide_axis(n.dodge = 2)) +
      labs(x = "Observed labor income quintile",
           y = NULL,
           fill = "Predicted labor\nincome quintile:",
           title = caption_lab[1],
           caption = caption_lab[2]) +
      theme(panel.spacing.x = unit(1, "cm"),
            panel.grid.major.x = element_blank())
    
    if (by_gender == T) {
      graph_quintile_rank <- graph_quintile_rank +
        facet_wrap(~ sample)
    }
    
  return(list(graph_level, graph_rank, graph_quintile_rank, graph_rank_data))
}
