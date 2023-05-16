# function to obtain predicted parent incomes
tstsls_fct <- function(dt = parents_sample_clean,
                       dt_child = child_sample_clean,
                       dt_child_parents = child_first_parents,
                       instruments = c("edu_level"),
                       constant_instruments = NULL,
                       parent_inc_var = "mean_labor_income_real_3050",
                       replace_0_income = NULL,
                       fs_log_inc = T,
                       parent_inc_pred_def = c("sum", "mean"),
                       child_inc_var = "mean_family_inc_real_3545",
                       cross_valid = T,
                       n_parent_inc_obs = 1,
                       parent_bot_trim_fs = 0,
                       parent_top_trim_fs = 1) {
  
  # useful code at some point perhaps
  # factor_instruments <- instruments[which(sapply(dt_synthetic_parents[, ..instruments], class, simplify = "array") %in% c("factor", "character"))]
  # dt_children_temp <- dt_children[eval(parse(text = paste0(paste0("factor(", fator_instruments, ") %in% levels(factor(dt_synthetic_father$", factor_instruments, "))"), collapse = "&")))]
  
  setDT(dt)
  setDT(dt_child)
  
  dt_stats <- data.frame(call = NA, stat = NA)
  z = 1
  
  # print instruments ----
  print(paste0("Instruments: ", paste(instruments, collapse = ", ")))
  # ----
  
  # save % of parents with missing instrument(s) ----
  dt_stats[z,1] = "% of parents with missing instrument(s)"
  dt_stats[z,2] = paste0(eval(parse(text = paste0("dt %>% summarise(round(mean(", paste0("is.na(", instruments, ")", collapse = " | "), ")*100, 2)) %>% pull()"))), "%")
  z = z + 1
  # print(paste0("% of parents with missing instrument(s): ", eval(parse(text = paste0("dt %>% summarise(round(mean(", paste0("is.na(", instruments, ")", collapse = " | "), ")*100, 2)) %>% pull()"))), "%"))
  
  if (!is.null(constant_instruments)) {
    dt_stats[z,1] = "% of parents with missing full set of instruments"
    dt_stats[z,2] = paste0(eval(parse(text = paste0("dt %>% summarise(round(mean(", paste0("is.na(", constant_instruments, ")", collapse = " | "), ")*100, 2)) %>% pull()"))), "%")
    z = z + 1
    # print(paste0("% of parents with missing full set of instruments: ", eval(parse(text = paste0("dt %>% summarise(round(mean(", paste0("is.na(", constant_instruments, ")", collapse = " | "), ")*100, 2)) %>% pull()"))), "%"))
  }
  # ----
  
  # generate parent income variable ----
  dt[, parent_inc := get(parent_inc_var)]
  # ----
  
  # save % of parents missing income variable, and % missing parent income variable + instrument(s) ----
  dt_stats[z,1] = "% of parents with missing income variable"
  dt_stats[z,2] = paste0(dt %>% summarise(round(mean(is.na(parent_inc))*100, 2)) %>% pull(), "%")
  z = z + 1
  dt_stats[z,1] = "% of parents with missing instrument(s) + income variable"
  dt_stats[z,2] = paste0(eval(parse(text = paste0("dt %>% summarise(round(mean(", paste0("is.na(", instruments, ")", collapse = " | "), " | is.na(parent_inc))*100, 2)) %>% pull()"))), "%")
  z = z + 1
  if (!is.null(constant_instruments)) {
    dt_stats[z,1] = "% of parents with missing full set of instrument(s) + income variable"
    dt_stats[z,2] = paste0(eval(parse(text = paste0("dt %>% summarise(round(mean(", paste0("is.na(", constant_instruments, ")", collapse = " | "), " | is.na(parent_inc))*100, 2)) %>% pull()"))), "%")
    z = z + 1
  }
  # print(paste0("% of parents with missing income variable: ", dt %>% summarise(round(mean(is.na(parent_inc))*100, 2)) %>% pull(), "%"))
  # print(paste0("% of parents with missing instrument(s) and income variable: ", eval(parse(text = paste0("dt %>% summarise(round(mean(", paste0("is.na(", instruments, ")", collapse = " | "), " | is.na(parent_inc))*100, 2)) %>% pull()"))), "%"))
  # ----
  
  # save % parents of parents with 0 incomes ----
  dt_stats[z,1] = "% of parents with negative or 0 income (among those not missing)"
  dt_stats[z,2] = paste0(dt %>% summarise(round(mean(parent_inc <= 0, na.rm = T)*100, 2)) %>% pull(), "%")
  z = z + 1
  # ----
  
  # log parent income if requested ----
  if (fs_log_inc == T) {
    dt[parent_inc > 0, parent_inc := log(parent_inc)]
    dt[parent_inc <= 0, parent_inc := NA]
  }
  
  if (!is.null(replace_0_income)) {
    dt[parent_inc <= 0, parent_inc := replace_0_income]
  }
  # ----
  
  # keep only parents not missing income and with at least n parent income observations ----
  dt_clean <- dt[!is.na(parent_inc) & get(paste0("n_", str_split(parent_inc_var, "mean_", simplify = T)[2])) >= n_parent_inc_obs]
  # ----
  
  # keep only parents with observed instruments ----
  dt_clean <- dt_clean %>% 
    filter(if_all(c("parent_inc", all_of(instruments)), ~ !is.na(.)))
  
  if (!is.null(constant_instruments)) {
    dt_clean <- dt_clean %>% 
      filter(if_all(c("parent_inc", all_of(constant_instruments)), ~ !is.na(.)))
  }
  # ----
  
  # trim income observations ----
  setDT(dt_clean)
  dt_clean[, parent_trim := (parent_inc < quantile(parent_inc, parent_bot_trim_fs))]
  dt_clean[, parent_trim := (parent_inc > quantile(parent_inc, parent_top_trim_fs))]
  # ----
  
  # prediction for fathers ----
  dt_fathers <- dt_clean[gender == "Male"]
  # print(paste0("Number of fathers on which first-stage prediction is done: ", formatC(nrow(dt_fathers[parent_trim == F]), format="d", big.mark=",")))
  
  # first-stage
  if (cross_valid == F) {
    fs_reg_fathers <- lm(formula(paste0("parent_inc ~ ", paste0("factor(", instruments, ")", collapse = " + "))), dt_fathers[parent_trim == F])
    dt_clean[parent_trim == F & gender == "Male", parent_inc_predict := predict(fs_reg_fathers)]
  }
  
  # cross-validation
  if (cross_valid == T) {
    train_control <- trainControl(method = "cv", number = 5, savePredictions = T)
    temp <- dt_fathers[parent_trim == F] %>% select(parent_inc, all_of(instruments))
    fs_reg_fathers <- train(parent_inc ~ .,
                            data = temp,
                            method = "lm",
                            trControl = train_control)
    
    # obtain predicted income for each observation using rowIndex
    dt_fathers <- dt_clean[gender == "Male" & parent_trim == F, rowIndex := 1:.N] %>%
      left_join(fs_reg_fathers$pred %>% rename(parent_inc_predict = pred) %>% select(-intercept)) %>% 
      filter(gender == "Male")
    
    # save quality of fit stats
    fs_fathers_fit <- fs_reg_fathers$resample
    fs_fathers_obs <- nrow(temp)
    dt_stats[z,1] = "Number of fathers on which first-stage prediction is done"
    dt_stats[z,2] = formatC(fs_fathers_obs, format="d", big.mark=",")
    z = z + 1
    dt_stats[z,1] = "Average R2 for fathers"
    dt_stats[z,2] = round(mean(fs_fathers_fit$Rsquared), 4)
    z = z + 1
  }
  # ----
  
  # prediction for mothers ----
  dt_mothers <- dt_clean[gender == "Female"]
  # print(paste0("Number of mothers on which first-stage prediction is done: ", formatC(nrow(dt_mothers[parent_trim == F]), format="d", big.mark=",")))
  
  # first-stage
  if (cross_valid == F) {
    fs_reg_mothers <- lm(formula(paste0("parent_inc ~ ", paste0("factor(", instruments, ")", collapse = " + "))), dt_mothers[parent_trim == F])
    dt_clean[parent_trim == F & gender == "Female", parent_inc_predict := predict(fs_reg_mothers)]
  }
  
  # cross-validation
  if (cross_valid == T) {
    train_control <- trainControl(method = "cv", number = 5, savePredictions = T)
    temp <- dt_mothers[parent_trim == F] %>%  select(parent_inc, all_of(instruments))
    fs_reg_mothers <- train(parent_inc ~ .,
                            data = temp,
                            method = "lm",
                            trControl = train_control)
    
    # obtain predicted income for each observation using rowIndex
    dt_mothers <- dt_clean[gender == "Female" & parent_trim == F, rowIndex := 1:.N] %>%
      left_join(fs_reg_mothers$pred %>% rename(parent_inc_predict = pred) %>%  select(-intercept)) %>% 
      filter(gender == "Female")
    
    # save quality of fit stats
    fs_mothers_fit <- fs_reg_mothers$resample
    fs_mothers_obs <- nrow(temp)
    dt_stats[z,1] = "Number of mothers on which first-stage prediction is done"
    dt_stats[z,2] = formatC(fs_mothers_obs, format="d", big.mark=",")
    z = z + 1
    dt_stats[z,1] = "Average R2 for mothers"
    dt_stats[z,2] = round(mean(fs_mothers_fit$Rsquared), 4)
    z = z + 1
  }
  # ----
  
  # bind dt_fathers and dt_mothers back together ----
  dt_pred <- bind_rows(dt_fathers, dt_mothers) %>% arrange(ind_id_68)
  # ----
  
  # obtain child id ----
  dt <- dt |> 
    left_join(dt_pred |> select(ind_id_68, parent_trim, rowIndex, parent_inc_predict, obs, Resample)) %>%
    left_join(dt_child_parents %>% select(ind_id_68, ind_id_68_child))
  # ----
  
  # for each child id, get the sum or mean of predicted parent income and the sum or mean of actual parent income ----
  # I keep parents for whom predicted income is missing so that when computing parent family income, children for whom at least 1 parent does not have predicted income will have missing predicted parent family labor income (otherwise this could bias results since only 1 of the 2 parents has predicted income)
  dt <- dt %>% 
    group_by(ind_id_68_child)
  if (parent_inc_pred_def == "sum") {
    if (fs_log_inc == T) { # if first-stage is in logs, I need to exponent before summing
      dt_child <- dt_child %>% 
        left_join(dt %>%
                    summarise(parent_inc_predict = sum(exp(parent_inc_predict)),
                              parent_inc = sum(exp(parent_inc))) %>% 
                    rename(ind_id_68 = ind_id_68_child))
    } else {
      dt_child <- dt_child %>% 
        left_join(dt %>%
                    summarise(parent_inc_predict = sum(parent_inc_predict),
                              parent_inc = sum(parent_inc)) %>% 
                    rename(ind_id_68 = ind_id_68_child))
    }
  }
  if (parent_inc_pred_def == "mean") {
    if (fs_log_inc == T) { # if first-stage is in logs, I need to exponent before averaging
      dt_child <- dt_child %>% 
        left_join(dt %>%
                    summarise(parent_inc_predict = mean(exp(parent_inc_predict)),
                              parent_inc = mean(exp(parent_inc))) %>% 
                    rename(ind_id_68 = ind_id_68_child))
    } else {
      dt_child <- dt_child %>%
        left_join(dt %>% 
                    summarise(parent_inc_predict = mean(parent_inc_predict),
                              parent_inc = mean(parent_inc)) %>% 
                    rename(ind_id_68 = ind_id_68_child))
    }
  }
  # ----
  
  # of children with missing parent predicted income ----
  dt_stats[z,1] = "% of children missing predicted parent income"
  dt_stats[z,2] = paste0(round(mean(is.na(dt_child$parent_inc_predict))*100, 2), "%")
  z = z + 1
  # ----
  
  # generate child income variable ----
  setDT(dt_child)
  dt_child[, child_inc := get(child_inc_var)]
  # ----
  
  # % of children with missing income ----
  dt_stats[z,1] = "% of children missing income"
  dt_stats[z,2] = paste0(round(mean(is.na(dt_child$child_inc))*100, 2), "%")
  z = z + 1
  # ----
  
  # % of children with missing income and parent predicted income ----
  dt_stats[z,1] = "% of children missing income and predicted parent income"
  dt_stats[z,2] = paste0(round(mean(is.na(dt_child$child_inc) | is.na(dt_child$parent_inc_predict))*100, 2), "%")
  z = z + 1
  # ----
  
  # % of children with negative or 0 incomes ----
  dt_stats[z,1] = "% of children with negative or 0 income"
  dt_stats[z,2] = paste0(round(mean(dt_child$child_inc <= 0, na.rm = T), 2), "%")
  z = z + 1
  # ----
  
  # keep only children with observed income and predicted parent income ----
  dt_child <- dt_child[!is.na(child_inc) & !is.na(parent_inc_predict)]
  # ----
  
  # number of children on which final estimation is done ----
  dt_stats[z,1] = "Number of children on which final estimation can be done"
  dt_stats[z,2] = formatC(nrow(dt_child), format="d", big.mark=",")
  z = z + 1
  # ----
  
  # compute parents and child ranks ----
  # rank by birth cohort
  dt_child <- dt_child %>%
    group_by(birth_year) %>%
    arrange(child_inc) %>%
    mutate(rank_child = ceiling(row_number()/n()*100)) %>%
    group_by(birth_year, child_inc) %>%
    mutate(rank_child = ceiling(median(rank_child))) %>%
    group_by(birth_year) %>% 
    arrange(parent_inc_predict) %>% 
    mutate(rank_parent_predict = ceiling(row_number()/n()*100)) %>%
    group_by(birth_year, parent_inc_predict) %>%
    mutate(rank_parent_predict = ceiling(median(rank_parent_predict))) %>%
    group_by(birth_year) %>% 
    arrange(parent_inc) %>% 
    mutate(rank_parent = ceiling(row_number()/n()*100)) %>%
    group_by(birth_year, parent_inc) %>%
    mutate(rank_parent = ceiling(median(rank_parent))) %>%
    ungroup()
  # ----
  
  # print dt_stats ----
  print(dt_stats)
  # ----
  
  # return child dataset and first-stage regressions
  return(list(dt_child, fs_reg_fathers, fs_reg_mothers, fs_fathers_fit, fs_mothers_fit, fs_fathers_obs, fs_mothers_obs, dt_stats, dt_pred))
}
