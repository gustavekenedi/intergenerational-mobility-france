# compare AUM
aum_comparison_graph <- function(dt = fs_predict[[1]],
                                 text_size = 20,
                                 annotation_size = 5,
                                 birth_year_fe = T) {
  dt_long <- dt %>% 
    select(rank_child, rank_parent, rank_parent_predict) %>% 
    pivot_longer(cols = -rank_child,
                 names_to = "parent_inc_var",
                 values_to = "rank_parent") %>% 
    mutate(parent_inc_var = case_when(parent_inc_var == "rank_parent" ~ "Observed",
                                      parent_inc_var == "rank_parent_predict" ~ "Predicted"))
  
  n_obs <- formatC(nrow(dt), format="d", big.mark=",")
  
  if (birth_year_fe == T) {
    coefs_tstsls <- setDT(tidy(lm(rank_child ~ rank_parent_predict + factor(birth_year), dt)))
    coefs_ols <- setDT(tidy(lm(rank_child ~ rank_parent + factor(birth_year), dt)))
  } else {
    coefs_tstsls <- setDT(tidy(lm(rank_child ~ rank_parent_predict, dt)))
    coefs_ols <- setDT(tidy(lm(rank_child ~ rank_parent, dt)))
  }
  
  df <- tibble(
    label = c(paste0("<span style='color:#de6757'>**OLS** (observed): **", round(coefs_ols[term == "(Intercept)"]$estimate + 25 * coefs_ols[term == "rank_parent"]$estimate, 3), "**", "</span>"),
              paste0("<span style='color:#EB9050'>**TSTSLS** (predicted): **", round(coefs_tstsls[term == "(Intercept)"]$estimate + 25 * coefs_tstsls[term == "rank_parent_predict"]$estimate, 3), "**", "</span>"),
              paste0("<span style='color:#78695F; font-size:10pt'>*Number of observations: ", n_obs, "*</span>")),
    rank_parent = c(3, 3, 100),
    mean_rank_child = c(90, 83, 2),
    hjust = c(0, 0, 1)
  )
  
  graph <- dt_long %>% 
    group_by(parent_inc_var, rank_parent) %>% 
    summarise(mean_rank_child = mean(rank_child)) %>% 
    ggplot(aes(x = rank_parent, y = mean_rank_child)) +
    geom_point(aes(color = parent_inc_var)) +
    geom_smooth(aes(color = parent_inc_var), method = "lm", se = FALSE) +
    geom_richtext(data = df, aes(label = label, hjust = hjust), color = "transparent", size = annotation_size) +
    scale_y_continuous(lim = c(0,100), expand = c(0.01, 0.01)) +
    scale_x_continuous(lim = c(0,100), expand = c(0.01, 0.01)) +
    labs(x = "Parent income rank",
         y = "Mean child income rank",
         color = "Parent income:",
         # caption = caption_lab
    ) +
    theme(legend.position = "none",
          text = element_text(size = text_size))
  
  return(graph)
}