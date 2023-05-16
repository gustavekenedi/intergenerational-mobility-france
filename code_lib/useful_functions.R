# count_to_pct
count_to_pct <- function(data, ..., col = n) {
  
  grouping_vars_expr <- quos(...)
  col_expr <- enquo(col)
  
  data %>%
    group_by(!!! grouping_vars_expr) %>%
    mutate(pct = (!! col_expr) / sum(!! col_expr)) %>%
    ungroup()
}


# easily obtain the variable id for a variable name
psid_var_search <- function(variable_name) {
  psid_vars %>% filter(variable == variable_name) %>% count(text, head_wife, var_count, var_id)
}


# easily obtain in long format individual characteristics
var_reshape_fct <- function(dt,
                            set_var_names, # list of vectors of variables
                            names_to_new, # column name for variables
                            values_to_new # column name for values
) {
  
  n_vars <- length(set_var_names)
  
  dt_new <- dt %>% select(ind_id_68)
  
  for (i in 1:n_vars) {
    dt_new <- dt_new %>% left_join(dt %>% 
                                     select(ind_id_68, all_of(set_var_names[[i]])) %>%
                                     pivot_longer(cols = -ind_id_68,
                                                  names_to = names_to_new[i],
                                                  values_to = values_to_new[i]) %>%
                                     mutate(year = as.numeric(rep(psid_vars %>% filter(variable %in% set_var_names[[1]]) %>% pull(year), n()/length(psid_vars %>% filter(variable %in% set_var_names[[1]]) %>% pull(year))))))
  }
  
  return(dt_new %>% 
           select(ind_id_68, year, everything()))
}


# function obtain variables from family datasets
var_fam_fct <- function(df_list = ls(envir = .GlobalEnv, pattern = "fam_\\d"),
                        family_id_vars = fam_id_vars,
                        variables,
                        name) {
  print(variables)
  print(df_list)
  print(family_id_vars)
  map_dfr(df_list,
          ~ get(.x) %>%
            select(any_of(family_id_vars), any_of({{variables}})) %>% 
            mutate(year = as.numeric(str_extract(.x, "\\d+")),
                   across(any_of(family_id_vars), ~ .x, .names = "itw_num"),
                   across(any_of({{variables}}), ~ .x, .names = {{name}}))) %>%
    select(itw_num, year, {{name}})
}