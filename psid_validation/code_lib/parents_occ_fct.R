# function to obtain parents' occupation
parents_occ_fct <- function(dt_parents_long = parents_sample_long,
                            fam_id_vars_arg = fam_id_vars,
                            head_vars = occ_head_vars,
                            name_head_vars = "occupation_head",
                            spouse_vars = occ_spouse_vars,
                            name_spouse_vars = "occupation_spouse",
                            use_emp = T,
                            dt_parents_emp = parents_emp,
                            age_range = 30:50) {
  
  parents_occ <- var_fam_fct(df_list = ls(envir = .GlobalEnv, pattern = "fam_\\d"),
                             family_id_vars = fam_id_vars_arg,
                             variables = head_vars,
                             name = name_head_vars) %>%
    left_join(var_fam_fct(df_list = ls(envir = .GlobalEnv, pattern = "fam_\\d"),
                          family_id_vars = fam_id_vars_arg,
                          variables = spouse_vars,
                          name = name_spouse_vars))
  print(parents_occ)
  
  keep_vars <- c("ind_id_68", "birth_year", "gender", "itw_num", "rel_head", "year")
  
  parents_occ_long <- bind_rows(dt_parents_long %>%
                                  select(all_of(keep_vars)) %>%
                                  filter(substr(as.character(rel_head), 1, 1) == 1) %>%
                                  left_join(parents_occ %>% rename(occupation = occupation_head) %>% select(-occupation_spouse)),
                                dt_parents_long %>%
                                  select(all_of(keep_vars)) %>%
                                  filter(substr(as.character(rel_head), 1, 1) == 2) %>%
                                  left_join(parents_occ %>% rename(occupation = occupation_spouse) %>% select(-occupation_head))) %>% 
    filter(year %in% 1968:2001)
  
  parents_occ_long %>% count(occupation == 0) %>% count_to_pct # 1968-1978: not eligible for retroactive coding, 1979-2019: various inactive 
  parents_occ_long %>% count(occupation, sort = T) %>% count_to_pct()
  
  # add "0"s in front of occupation with only 1 or 2 digits
  parents_occ_long <- parents_occ_long %>% 
    mutate(occupation = as.character(occupation),
           occupation = case_when(nchar(occupation) == 1 ~ paste0("00", occupation),
                                  nchar(occupation) == 2 ~ paste0("0", occupation),
                                  TRUE ~ occupation))
  
  if (use_emp == T) {
    # merge with employment status dt
    parents_occ_long <- parents_occ_long %>% 
      left_join(dt_parents_emp)
    parents_occ_long |> group_by(emp_status_lab) |> summarise(mean(occupation == "000"))
    parents_occ_long %>% filter(occupation == "000") %>% count(emp_status_lab, sort = T) %>% count_to_pct() # 38% of occupation == 0 are housewives; missing emp_status corresponds to 1968-1978 spouses
    parents_occ_long <- parents_occ_long %>% 
      mutate(occupation = ifelse(occupation == "000" & emp_status_new != 1 & !is.na(emp_status_new) & !year %in% 1968:1980, 1000 + emp_status_new, occupation))
    parents_occ_long %>% count(occupation, sort = T)
  }
  
  # generate age variable
  parents_occ_long <- parents_occ_long %>%
    mutate(age = year - birth_year)
  
  # filter out occupations that are missing
  parents_occ <- parents_occ_long %>%
    filter(!as.numeric(occupation) %in% c(000,997:999))
  
  # filter age range
  if(!is.null(age_range)) {
    parents_occ <- parents_occ %>%
      filter(age %in% age_range)
  }
  
  # keep only most common occupation
  parents_occ <- parents_occ %>%
    group_by(ind_id_68, occupation) %>%
    summarise(n = n(),
              mean_age = mean(age)) %>%
    slice_max(order_by = n)
  setDT(parents_occ)
  parents_occ[, .N, by = ind_id_68] %>% count(N) %>% count_to_pct()
  # 87.6% have only one occupation, but the rest have 2 or more with an equal number of observations
  
  # for starters I'll just take the one at the oldest average age
  parents_occ <- parents_occ %>%
    group_by(ind_id_68) %>%
    slice_max(order_by = mean_age)
  setDT(parents_occ)
  parents_occ[, .N, by = ind_id_68] %>% count(N)
  
  # some parents have occupations measured with same mean age
  # take one at random
  parents_occ[, id := 1:.N, by = ind_id_68]
  parents_occ %>% count(id)
  parents_occ <- parents_occ[id == 1]
  
  return(parents_occ)
}