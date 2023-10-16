# obtain parents' education level
parents_edu_fct <- function(dt_parents = parents_sample,
                            vars = edu_vars,
                            age_min = 25) {
  
  parents_edu <- var_reshape_fct(dt_parents, list(vars), "edu_var", "edu_years")
  setDT(parents_edu)
  
  parents_edu %>% count(edu_years) %>% count_to_pct()
  
  parents_edu <- parents_edu %>% 
    left_join(dt_parents %>% select(ind_id_68, birth_year)) %>% 
    mutate(age = year - birth_year)
  
  # take max level
  parents_edu <- parents_edu[!edu_years %in% c(0, 98, 99)] %>% 
    filter(age >= age_min) %>% 
    group_by(ind_id_68) %>% 
    summarise(max_edu = max(edu_years)) %>% 
    ungroup
  setDT(parents_edu)
  
  return(parents_edu)
}