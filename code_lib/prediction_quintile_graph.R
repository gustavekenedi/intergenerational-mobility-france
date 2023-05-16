# prediction vs observed parent income quintile graph
prediction_quintile_graph <- function(dt = fs_predict[[1]][[1]],
                                      caption_lab = caption_fct(fs = fs_predict[[1]][[8]],
                                                                instru_labels = instru_labs[[1]]),
                                      pct_size = 3) {
  dt %>% 
    mutate(across(c(rank_parent, rank_parent_predict), ~ case_when(. %in% 1:20 ~ "Bottom 20%",
                                                                   . %in% 21:40 ~ "Quintile 2",
                                                                   . %in% 41:60 ~ "Quintile 3",
                                                                   . %in% 61:80 ~ "Quintile 4",
                                                                   . %in% 81:100 ~ "Top 20%"),
                  .names = "{.col}_quintile"),
           rank_parent_quintile = factor(rank_parent_quintile, levels = c("Bottom 20%", "Quintile 2", "Quintile 3", "Quintile 4", "Top 20%")),
           rank_parent_predict_quintile = factor(rank_parent_predict_quintile, levels = c("Bottom 20%", "Quintile 2", "Quintile 3", "Quintile 4", "Top 20%"))) %>%
    group_by(rank_parent_quintile, rank_parent_predict_quintile) %>% 
    summarise(n_obs = n()) %>% 
    group_by(rank_parent_quintile) %>% 
    mutate(pct = n_obs/sum(n_obs)) %>% 
    ungroup() %>%
    ggplot(aes(x = rank_parent_quintile, y = pct, fill = fct_rev(rank_parent_predict_quintile))) +
    geom_col(position = "fill") +
    geom_text(aes(label = paste0(round(pct*100,2), "%")), position = position_stack(vjust = 0.5),
              show.legend = FALSE, color = "white", size = pct_size) +
    scale_fill_manual(values = alpha(my_palette[c(5,4,3,2,1)], alpha), guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(expand = c(0,0), labels = scales::percent) +
    scale_x_discrete(expand = c(0,0), guide = guide_axis(n.dodge = 1)) +
    labs(x = "Observed family labor income quintile",
         y = NULL,
         fill = "Predicted family \nlabor income quintile:",
         title = caption_lab[1],
         caption = caption_lab[2]) +
    theme(panel.grid.major.x = element_blank())
}
