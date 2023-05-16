# obtain individual's income variable for different income definitions and age ranges
# works for both parents and children
inc_fct <- function(dt,
                    inc_var = c("labor income", "family labor income", "family tax income", "family total income"),
                    age_range,
                    zero_to_na = F) {
  
  setDT(dt)
  keep_vars <- c("ind_id_68", "age")
  
  # only keep observations when individual is head or spouse
  dt <- dt %>% filter(substr(rel_head, 1, 1) %in% 1:2)
  
  # exclude unreliable income observations and keep only relevant income variable
  # labor income
  if (inc_var == "labor income") {
    dt <- bind_rows(dt[substr(rel_head, 1, 1) == 1 &
                         (accuracy_head_labor_income != 2 | is.na(accuracy_head_labor_income)) &
                         head_labor_income != 9999999] %>%
                      select(all_of(keep_vars), labor_income_real = head_labor_income_real),
                    dt[substr(rel_head, 1, 1) == 2 &
                         (accuracy_wife_labor_income != 2 | is.na(accuracy_wife_labor_income)) &
                         wife_labor_income != 9999999] %>%
                      select(all_of(keep_vars), labor_income_real = wife_labor_income_real))
  }
  
  # family labor income
  if (inc_var == "family labor income") {
    dt <- dt[((accuracy_head_labor_income != 2 | is.na(accuracy_head_labor_income)) & head_labor_income != 9999999) & 
               ((accuracy_wife_labor_income != 2 | is.na(accuracy_wife_labor_income)) & wife_labor_income != 9999999)] %>% 
      select(all_of(keep_vars), contains("family_labor_income"))
  }
  
  # taxable family income
  if (inc_var == "family tax income") {
    dt <- dt[(accuracy_family_tax_income != 2 | is.na(accuracy_family_tax_income)) &
               family_tax_income != 9999999] %>% 
      select(all_of(keep_vars), contains("family_tax_income_real"))
  }
  
  # total income = taxable family income + transfers
  if (inc_var == "family total income") {
    dt <- dt[(accuracy_family_tax_income != 2 | is.na(accuracy_family_tax_income)) &
               family_tax_income != 9999999 & family_transfers_income != 9999999] %>% 
      select(all_of(keep_vars), contains("family_total_income_real"))
  }
  
  inc_var_name <- paste0(str_replace_all(inc_var, " ", "_"), "_real")
  age_range_name <- paste0(min(age_range), max(age_range))
  
  if (zero_to_na == T) {
    dt <- dt %>% 
      mutate(across(contains(inc_var_name), ~ ifelse(.x == 0, NA, .x)))
  }
  
  inc_vars <- dt %>% select(contains(inc_var_name)) %>% names()
  
  dt <- dt %>%
    filter((age - 1) %in% age_range) %>% # age - 1 because income measured in T refers to income earned in T-1
    group_by(ind_id_68) %>%
    summarise(across(inc_vars, ~ mean(.x, na.rm = T), .names = paste0("mean_{.col}_", age_range_name)),
              across(inc_vars, ~ sum(!is.na(.x)), .names = paste0("n_{.col}_", age_range_name)),
              !!paste0("mean_age_", age_range_name, "_", inc_var_name) := mean(age-1)) %>%
    ungroup()
  setDT(dt)
  
  return(dt)
}
