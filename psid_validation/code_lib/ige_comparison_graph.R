# compare IGE graph
ige_comparison_graph <- function(dt = fs_predict[[1]][[1]],
                                 replace_zeros = NA,
                                 weights = NULL,
                                 text_size = 20,
                                 title_size = 20,
                                 annotation_size = 5,
                                 caption_lab = caption_fct(fs = fs_predict[[1]][[8]],
                                                           instru_labels = instru_labs[[1]]),
                                 keep_data = F) {
  
  dt <- dt %>% 
    mutate(across(c("child_inc", "parent_inc", "parent_inc_predict"), ~ ifelse(.x <= 0, replace_zeros, .x))) |> 
    filter(if_all(c("child_inc", "parent_inc", "parent_inc_predict"), ~ !is.na(.)))
  
  if (!is.null(weights)) {
    dt <- dt |> filter(!is.na(get(weights)))
  }
  
  dt_long <- dt %>% 
    select(child_inc, parent_inc, parent_inc_predict) %>% 
    pivot_longer(cols = -child_inc,
                 names_to = "parent_inc_var",
                 values_to = "parent_inc") %>% 
    mutate(parent_inc_var = case_when(parent_inc_var == "parent_inc" ~ "Observed",
                                      parent_inc_var == "parent_inc_predict" ~ "Predicted"))
  
  n_obs <- formatC(nrow(dt), format="d", big.mark=",")
  if (is.null(weights)) {
    coefs_tstsls <- tidy(lm(log(child_inc) ~ log(parent_inc_predict) + factor(birth_year), dt %>% filter(child_inc > 0)))
    coefs_ols <- tidy(lm(log(child_inc) ~ log(parent_inc) + factor(birth_year), dt %>% filter(child_inc > 0)))  
  } else {
    coefs_tstsls <- tidy(lm(log(child_inc) ~ log(parent_inc_predict) + factor(birth_year), dt %>% filter(child_inc > 0), weights = get(weights)))
    coefs_ols <- tidy(lm(log(child_inc) ~ log(parent_inc) + factor(birth_year), dt %>% filter(child_inc > 0), weights = get(weights)))
  }
  
  
  coefs_table_clean <- bind_rows(coefs_ols |> mutate(parent_inc_var = "parent_inc"),
                                 coefs_tstsls |> mutate(parent_inc_var = "parent_inc_predict")) |> 
    filter(str_detect(term, "Intercept|parent_inc")) |>
    mutate(n_obs = nrow(dt),
           term = ifelse(str_detect(term, "parent_inc"), "parent_inc", term)) |> 
    select(parent_inc_var, term, coef = estimate, se = std.error, n_obs) |> 
    pivot_wider(names_from = term,
                values_from = c(coef, se)) |> 
    mutate(ige = coef_parent_inc) |> 
    select(parent_inc_var, ige, se_ige = se_parent_inc, n_obs)
  
  df <- tibble(
    label = c(paste0("<span style='color:#de6757'>**OLS** (observed): **", round(coefs_ols %>% filter(term == "log(parent_inc)") %>% select(estimate), 3), "** (", round(coefs_ols %>% filter(term == "log(parent_inc)") %>% select(std.error), 3), ")", "</span>"),
              paste0("<span style='color:#EB9050'>**TSTSLS** (predicted): **", round(coefs_tstsls %>% filter(term == "log(parent_inc_predict)") %>% select(estimate), 3), "** (", round(coefs_tstsls %>% filter(term == "log(parent_inc_predict)") %>% select(std.error), 3), ")", "</span>"),
              paste0("<span style='color:#78695F; font-size:12pt'>*Number of observations: ", n_obs, "*</span>")),
    mean_parent_inc = c(4, 4, 12.95),
    mean_child_inc = c(12.1, 11.8, 9.8),
    hjust = c(0, 0, 1)
  )
  
  graph <- dt_long %>% 
    filter(child_inc > 0 & parent_inc > 0) %>% 
    group_by(parent_inc_var) %>% 
    arrange(parent_inc) %>% 
    mutate(rank_parent = ceiling(row_number()/n()*100)) %>%
    group_by(parent_inc_var, parent_inc) %>%
    mutate(rank_parent = ceiling(mean(rank_parent))) %>%
    group_by(parent_inc_var, rank_parent) %>% 
    summarise(mean_parent_inc = mean(log(parent_inc)),
              mean_child_inc = mean(log(child_inc))) %>%
    ungroup() %>% 
    ggplot(aes(x = mean_parent_inc, y = mean_child_inc)) +
    geom_point(aes(color = parent_inc_var), alpha = .8) +
    geom_smooth(aes(color = parent_inc_var), method = "lm", se = FALSE, formula = "y ~ poly(x, 3)") +
    geom_richtext(data = df, aes(label = label, hjust = hjust), color = "transparent", fill = "transparent", size = annotation_size) +
    scale_y_continuous(lim = c(9.75, 12.3), breaks = 9:13, expand = c(0.01, 0.01)) +
    scale_x_continuous(lim = c(4, 13), expand = c(0.01, 0.01)) +
    labs(x = "Log parent income",
         y = "Mean log child income",
         color = "Parent income:",
         title = caption_lab[1],
         caption = caption_lab[2]) +
    theme(legend.position = "none",
          text = element_text(size = text_size),
          plot.title = element_text(size = title_size))
  
  if (keep_data == F) {
    return(graph)  
  } else {
    return(coefs_table_clean)
  }
  
}
