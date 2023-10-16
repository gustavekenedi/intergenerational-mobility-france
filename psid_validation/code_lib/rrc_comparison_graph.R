# compare RRC graph
rrc_comparison_graph <- function(dt = fs_predict[[1]],
                                 weights = NULL,
                                 with_aum = F,
                                 region_lab = F,
                                 rank_parent_text = c(1, 1, 100),
                                 mean_rank_child_text = c(90, 75, 5),
                                 text_size = 20,
                                 title_size = 20,
                                 annotation_size = 5,
                                 num_obs_size = 12,
                                 with_caption = T,
                                 caption_lab = caption_fct(fs = fs_predict[[1]][[8]],
                                                           instru_labels = instru_labs[[1]]),
                                 keep_data = F) {
  
  if (!is.null(weights)) {
    dt <- dt |> filter(!is.na(get(weights)))
  }
  
  dt_long <- dt %>% 
    select(ind_id_68, rank_child, rank_parent, rank_parent_predict) %>% 
    pivot_longer(cols = -c(ind_id_68, rank_child),
                 names_to = "parent_inc_var",
                 values_to = "rank_parent") %>% 
    mutate(parent_inc_var = case_when(parent_inc_var == "rank_parent" ~ "Observed",
                                      parent_inc_var == "rank_parent_predict" ~ "Predicted"))
  

  
  n_obs <- formatC(nrow(dt), format = "d", big.mark = ",")
  
  if (is.null(weights)) {
    coefs_tstsls <- setDT(tidy(lm(rank_child ~ rank_parent_predict + factor(birth_year), dt)))
    coefs_ols <- setDT(tidy(lm(rank_child ~ rank_parent + factor(birth_year), dt)))
  } else {
    coefs_tstsls <- setDT(tidy(lm(rank_child ~ rank_parent_predict + factor(birth_year), dt, weights = get(weights))))
    coefs_ols <- setDT(tidy(lm(rank_child ~ rank_parent + factor(birth_year), dt, weights = get(weights))))
  }
  
  int_slope_tstsls = coefs_tstsls |> filter(term %in% c("(Intercept)", "rank_parent_predict")) %>% pull(estimate)
  int_slope_ols = coefs_ols |> filter(term %in% c("(Intercept)", "rank_parent")) %>% pull(estimate)
  
  coefs <- data.frame(x = 1, xend = 100,
                      y = c(int_slope_tstsls[1], int_slope_ols[1]),
                      yend = c(int_slope_tstsls[1] + 100*int_slope_tstsls[2],
                               int_slope_ols[1] + 100*int_slope_ols[2]),
                      parent_inc_var = c("Predicted", "Observed"))
  
  rrc_with_se_ols_clean <- paste0("**", round(coefs_ols %>% filter(term == "rank_parent") %>% select(estimate), 3), "** (", round(coefs_ols %>% filter(term == "rank_parent") %>% select(std.error), 3), ")")
  rrc_with_se_tstsls_clean <- paste0("**", round(coefs_tstsls %>% filter(term == "rank_parent_predict") %>% select(estimate), 3), "** (", round(coefs_tstsls %>% filter(term == "rank_parent_predict") %>% select(std.error), 3), ")")
  
  aum_ols_clean <- round(coefs_ols[term == "(Intercept)"]$estimate + 25 * coefs_ols[term == "rank_parent"]$estimate, 2)
  aum_tstsls_clean <- round(coefs_tstsls[term == "(Intercept)"]$estimate + 25 * coefs_tstsls[term == "rank_parent_predict"]$estimate, 2)
  
  coefs_table_clean <- bind_rows(coefs_ols |> mutate(parent_inc_var = "rank_parent"),
                                 coefs_tstsls |> mutate(parent_inc_var = "rank_parent_predict")) |> 
    filter(str_detect(term, "Intercept|rank_parent")) |>
    mutate(n_obs = nrow(dt),
           term = ifelse(str_detect(term, "rank_parent"), "rank_parent", term)) |> 
    select(parent_inc_var, term, coef = estimate, se = std.error, n_obs) |> 
    pivot_wider(names_from = term,
                values_from = c(coef, se)) |> 
    mutate(rrc = coef_rank_parent,
           aum = `coef_(Intercept)` + 25 * coef_rank_parent) |> 
    select(parent_inc_var, rrc, se_rrc = se_rank_parent, aum, n_obs)
  
  if (with_aum == F) {
    df <- tibble(
      label = c(paste0("<span style='color:#de6757'>**OLS** (observed): ", rrc_with_se_ols_clean, "</span>"),
                paste0("<span style='color:#EB9050'>**TSTSLS** (predicted): ", rrc_with_se_tstsls_clean, "</span>"),
                paste0("<span style='color:#78695F; font-size:", num_obs_size, "pt'>*Number of observations: ", n_obs, "*</span>")),
      rank_parent = rank_parent_text,
      mean_rank_child = mean_rank_child_text,
      hjust = c(0, 0, 1)
    )
  } else {
    df <- tibble(
      label = c("<i style='color:#5d5d5d'>RRC:</i>",
                "<i style='color:#5d5d5d'>AUM:</i>",
                
                paste0("<span style='color:#de6757'>**", ifelse(region_lab == F, "OLS** (observed)", "OLS**"), ":</span>"),
                paste0("<span style='color:#de6757'>", rrc_with_se_ols_clean, "</span>"),
                paste0("<span style='color:#de6757'> **", aum_ols_clean, "** </span>"),
                
                paste0("<span style='color:#EB9050'>**", ifelse(region_lab == F, "TSTSLS** (predicted)", "TSTSLS**"), ":</span>"),
                paste0("<span style='color:#EB9050'>", rrc_with_se_tstsls_clean, "</span>"),
                paste0("<span style='color:#EB9050'> **", aum_tstsls_clean, "** </span>"),
                
                paste0("<span style='color:#78695F; font-size:10pt'>*Number of observations: ", n_obs, "*</span>")),
      rank_parent = rank_parent_text,
      mean_rank_child = mean_rank_child_text,
      hjust = c(rep(0, 8), 1)
    )
  }
  
  if (is.null(weights)) {
    dt_long <- dt_long %>% 
      group_by(parent_inc_var, rank_parent) %>% 
      summarise(mean_rank_child = mean(rank_child))  
  } else {
    dt_long <- dt_long %>%
      left_join(dt |> select(ind_id_68, all_of(weights))) |> 
      group_by(parent_inc_var, rank_parent) %>% 
      summarise(mean_rank_child = weighted.mean(rank_child, get(weights)))
  }
  
  graph <- dt_long %>%
    ggplot(aes(x = rank_parent, y = mean_rank_child)) +
    geom_point(aes(color = parent_inc_var), alpha = .8) +
    # geom_smooth(aes(color = parent_inc_var), method = "lm", se = FALSE) +
    geom_segment(data = coefs, aes(x = x, xend = xend, y = y, yend = yend, colour = parent_inc_var), linewidth = 1) +
    geom_richtext(data = df, aes(label = label, hjust = hjust), color = "transparent", fill = alpha("white", .5), size = annotation_size) +
    scale_y_continuous(lim = c(0,100), expand = c(0.01, 0.01)) +
    scale_x_continuous(lim = c(0,100), expand = c(0.01, 0.01)) +
    labs(x = "Parent income rank",
         y = "Mean child income rank",
         color = "Parent income:",
         title = caption_lab[1]) +
    theme(legend.position = "none",
          text = element_text(size = text_size),
          plot.title = element_text(size = title_size))
  
  if (with_caption == T) {
    graph <- graph +
      labs(caption = caption_lab[2])  
  }
  
  if (keep_data == F){
    return(graph)  
  } else {
    return(coefs_table_clean)
  }
}
