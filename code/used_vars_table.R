library(tidyverse)
library(here)
library(kableExtra)

figure_path <- "/Users/gustave/Dropbox/Apps/Overleaf/Intergenerational Income Mobility/psid_validation/figures/"

var_used <- read_csv(here("data/used_vars_sample.csv")) |> 
  left_join(read_csv(here("data/used_vars_income_data.csv"))) |> 
  left_join(read_csv(here("data/used_vars_variables.csv")))

var_used <- var_used |> rename(year = value)

var_used <- var_used |> 
  mutate(across(everything(), ~ ifelse(is.na(.), "-", .)))

var_used <- var_used |> 
  select(year,
         itw_num_vars, fam_id_vars, seq_num_vars, rel_head_vars,
         age_vars, birth_year_vars,
         edu_vars, occ_head_vars, occ_spouse_vars, emp_head_spouse_7919_vars, race_head_vars_6819, race_spouse_vars_8519,
         parent_state_vars,
         head_lab_inc_vars, accu_head_lab_inc_vars, spouse_lab_inc_vars, accu_spouse_lab_inc_vars, tax_inc_vars, accu_tax_inc_vars, transfers_vars)

kable(var_used, format = "latex",
      booktabs = T, caption = "Detail of Variables Used in Each Year", label = "var_used",
      col.names = c("Year",
                    "Int. # (ind. file)", "Int. # (fam. files)", "Sequence #", "Rel. to ref. person",
                    "Age", "Birth year",
                    "Education", "Ref. person", "Partner", "Employment status", "Ref. person", "Partner",
                    "State of res.",
                    "Ref. person", "Accu.", "Partner", "Accu.", "Tax. inc.", "Accu. tax. inc.", "Total transfers"),
      align = c("l", rep("c", 20))) |> 
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  footnote(general = "Int. = interview. Rel. = relation. Ref. = reference. Res. = residence. Accu. = accuracy. Tax. inc. = taxable income.",
           general_title = "Notes:", title_format = "italic", fixed_small_size = T, footnote_as_chunk = T, threeparttable = T) |> 
  add_header_above(c(rep("", 8), "Occupation" = 2, "", "Race" = 2, "", "Labor income" = 4, rep("", 2))) |> 
  save_kable(paste0(figure_path, "vars_used.tex"))
