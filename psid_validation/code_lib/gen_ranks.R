# generate rank for a given income variable
gen_ranks <- function(dt, inc) {
  
  dt <- dt %>%
    group_by(birth_year) %>%
    arrange(get(inc)) %>%
    mutate(!!paste0("rank_", inc) := ceiling(row_number()/n()*100)) %>%
    group_by(birth_year, get(inc)) %>%
    mutate(!!paste0("rank_", inc) := ceiling(median(get(paste0("rank_", inc))))) %>%
    ungroup() %>% 
    select(ind_id_68, paste0("rank_", inc))
  
  return(dt)
}