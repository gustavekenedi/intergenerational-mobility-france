
library(doParallel) # Version 1.0.17

prj_dir <- "C:/Users/Public/Documents/gustave_k/intergen_income_mob/jpube/revision/"

rpts <- lapply(1:1000, function(x) {
  list(out = paste0(x, ".html"), params = list(iteration = x))
})

do_rpt <- function(r){
  require(rmarkdown) # Version 2.18
  tf <- tempfile()
  dir.create(tf)
  rmarkdown::render(input = paste0(prj_dir, "code/3_bootstrap.Rmd"),
                    output_file = r$out,
                    intermediates_dir = tf,
                    params = r$params,
                    quiet = T)
  unlink(tf)
}

registerDoParallel(cores = 6)

start_time <- Sys.time()
foreach(r = rpts, .combine = c) %dopar% do_rpt(r)
stop_time <- Sys.time()
stop_time - start_time


################################################################################

library(tidyverse)  # Version 1.3.2
library(data.table) # Version 1.14.6

# Baseline results
##################

baseline_boot <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/baseline_", x, ".csv"))}) %>% rbindlist()
baseline_boot <- baseline_boot[, .(boot_se = sd(coef)), by = c("measure", "income", "sample")]

# Figure 1A
fig1A_boot <- fread(paste0(prj_dir, "out/fig/fig1A_cef_log_income.csv")) %>%
  .[, ":=" (boot_se = baseline_boot[measure == "IGE" & income == "sum" & sample == "Full"]$boot_se,
            boot_se_mid = baseline_boot[measure == "IGE" & income == "sum" & sample == "Middle"]$boot_se)]
fwrite(fig1A_boot, paste0(prj_dir, "out/fig/fig1A_cef_log_income_boot.csv"))

ggplot(fig1A_boot, aes(x = family_log_income_n, y = household_log_income)) + 
  geom_point(alpha = .5) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = F) +
  geom_vline(xintercept = c(fig1A_boot$family_log_income_n[10], 
                            fig1A_boot$family_log_income_n[91]), 
             linetype = "dashed") + 
  annotate("text", x = 9.5, y = 10.5, 
           label = paste0("IGE: ", round(unique(fig1A_boot$coef), 2), 
                          " (", round(unique(fig1A_boot$boot_se), 3), ")\n",
                          "IGE[Par. Inc. P10-P90]: ", round(unique(fig1A_boot$coef_mid), 2), 
                          " (", round(unique(fig1A_boot$boot_se_mid), 3), ")\n")) 
ggsave(paste0(prj_dir, "out/fig/fig1A_cef_log_income_boot.png"))

# Figure 1B
fig1B_boot <- fread(paste0(prj_dir, "out/fig/fig1B_cef_rank.csv")) %>%
  .[, ":=" (boot_se = baseline_boot[measure == "RRC" & income == "sum" & sample == "Full"]$boot_se,
            boot_se_mid = baseline_boot[measure == "RRC" & income == "sum" & sample == "Middle"]$boot_se)]
fwrite(fig1B_boot, paste0(prj_dir, "out/fig/fig1B_cef_rank_boot.csv"))

ggplot(fig1B_boot, aes(x = family_rank, y = child_rank)) + 
  geom_point(alpha = .5) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = F) +
  geom_vline(xintercept = c(10, 90), linetype = "dashed") + 
  annotate("text", x = 50, y = 70, 
           label = paste0("IGE: ", round(unique(fig1B_boot$coef), 2), 
                          " (", round(unique(fig1B_boot$boot_se), 3), ")\n",
                          "IGE[Par. Inc. P10-P90]: ", round(unique(fig1B_boot$coef_mid), 2), 
                          " (", round(unique(fig1B_boot$boot_se_mid), 3), ")\n")) 
ggsave(paste0(prj_dir, "out/fig/fig1B_cef_rank_boot.png"))

# Figure 1A - mean
log_cef_n_boot <- fread(paste0(prj_dir, "out/fig/log_cef_n.csv")) %>%
  .[, ":=" (boot_se = baseline_boot[measure == "IGE" & income == "mean" & sample == "Full"]$boot_se,
            boot_se_mid = baseline_boot[measure == "IGE" & income == "mean" & sample == "Middle"]$boot_se)]
fwrite(log_cef_n_boot, paste0(prj_dir, "out/fig/log_cef_n_boot.csv"))

ggplot(log_cef_n_boot, aes(x = family_log_income_n, y = household_log_income)) + 
  geom_point(alpha = .5) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = F) +
  geom_vline(xintercept = c(log_cef_n_boot$family_log_income_n[10], 
                            log_cef_n_boot$family_log_income_n[91]), 
             linetype = "dashed") + 
  annotate("text", x = 9.5, y = 10.5, 
           label = paste0("IGE: ", round(unique(log_cef_n_boot$coef), 2), 
                          " (", round(unique(log_cef_n_boot$boot_se), 3), ")\n",
                          "IGE[Par. Inc. P10-P90]: ", round(unique(log_cef_n_boot$coef_mid), 2), 
                          " (", round(unique(log_cef_n_boot$boot_se_mid), 3), ")\n")) 
ggsave(paste0(prj_dir, "out/fig/log_cef_n_boot.png"))

# Figure 1B - mean
rrc_cef_n_boot <- fread(paste0(prj_dir, "out/fig/rrc_cef_n.csv")) %>%
  .[, ":=" (boot_se = baseline_boot[measure == "RRC" & income == "mean" & sample == "Full"]$boot_se,
            boot_se_mid = baseline_boot[measure == "RRC" & income == "mean" & sample == "Middle"]$boot_se)]
fwrite(rrc_cef_n_boot, paste0(prj_dir, "out/fig/rrc_cef_n_boot.csv"))

ggplot(rrc_cef_n_boot, aes(x = family_rank, y = child_rank)) + 
  geom_point(alpha = .5) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = F) +
  geom_vline(xintercept = c(10, 90), linetype = "dashed") + 
  annotate("text", x = 50, y = 70, 
           label = paste0("IGE: ", round(unique(rrc_cef_n_boot$coef), 2), 
                          " (", round(unique(rrc_cef_n_boot$boot_se), 3), ")\n",
                          "IGE[Par. Inc. P10-P90]: ", round(unique(rrc_cef_n_boot$coef_mid), 2), 
                          " (", round(unique(rrc_cef_n_boot$boot_se_mid), 3), ")\n")) 
ggsave(paste0(prj_dir, "out/fig/rrc_cef_n_boot.png"))

# Transition matrices
tm_boot <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/tm_", x, ".csv"))}) %>% rbindlist()
tm_boot <- tm_boot[, .(boot_se = sd(share)), by = c("family_quintile", "child_quintile", "income")]

# Baseline TM
fig2_tm_boot <- merge.data.table(fread(paste0(prj_dir, "out/fig/fig2_tm.csv")),
                                 tm_boot[income == "sum"][, income := NULL], 
                                 by = c("family_quintile", "child_quintile"), all.x = T)
fwrite(fig2_tm_boot, paste0(prj_dir, "out/fig/fig2_tm_boot.csv"))

ggplot(fig2_tm_boot, aes(x = family_quintile, y = share, fill = factor(-child_quintile), label = pct)) +
  geom_bar(position = "fill", stat = "identity", width = .99) +
  geom_text(position = position_stack(vjust = .5), color = "Black")
ggsave(paste0(prj_dir, "out/fig/fig2_tm_boot.png"))

# Baseline TM - mean
tm_n_boot <- merge.data.table(fread(paste0(prj_dir, "out/fig/tm_n.csv")),
                                 tm_boot[income == "mean"][, income := NULL], 
                                 by = c("family_quintile", "child_quintile"), all.x = T)
fwrite(tm_n_boot, paste0(prj_dir, "out/fig/tm_n_boot.csv"))

ggplot(tm_n_boot, aes(x = family_quintile, y = share, fill = factor(-child_quintile), label = pct)) +
  geom_bar(position = "fill", stat = "identity", width = .99) +
  geom_text(position = position_stack(vjust = .5), color = "Black")
ggsave(paste0(prj_dir, "out/fig/tm_n_boot.png"))

# Top TM
top_tm_boot <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/top_tm_", x, ".csv"))}) %>% rbindlist()
top_tm_boot <- top_tm_boot[, .(boot_se = sd(share)), by = c("family_quantile", "child_quantile", "quantile")]

figC1_top_tm_boot <- merge.data.table(fread(paste0(prj_dir, "out/fig/figC1_top_tm.csv")),
                                      top_tm_boot, all.x = T, 
                                      by = c("family_quantile", "child_quantile", "quantile"))
fwrite(figC1_top_tm_boot, paste0(prj_dir, "out/fig/figC1_top_tm_boot.csv"))

ggplot(figC1_top_tm_boot, aes(x = family_quantile, y = share, label = pct, fill = share)) +
  geom_bar(position = "fill", stat = "identity", width = .99) +
  geom_text(position = position_stack(vjust = .5), color = "white") +
  facet_wrap(~quantile, scales = "free")
ggsave(paste0(prj_dir, "out/fig/figC1_top_tm_boot.png"))

# Robustness
############

# Robustness to instruments - Log CEFs
instru_boot <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/instruments_", x, ".csv"))}) %>% rbindlist()
instru_boot <- instru_boot[, .(boot_se = sd(coef)), by = c("instru", "estimator")]

figB3A_log_cefs_instru_boot <- merge.data.table(fread(paste0(prj_dir, "out/fig/figB3A_log_cefs_instru.csv")),
                                                instru_boot[estimator == "IGE"][, estimator := NULL], 
                                                all.x = T, by = "instru")
fwrite(figB3A_log_cefs_instru_boot, paste0(prj_dir, "out/fig/figB3A_log_cefs_instru_boot.csv"))

ggplot(figB3A_log_cefs_instru_boot %>% mutate(instru = paste0(instru, "\n IGE=", round(coef, 2), " (", round(boot_se, 3), ")")), 
       aes(x = family_log_income_n, y = household_log_income)) + geom_point(alpha = .5) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = F) + facet_wrap(~instru, nrow = 1)
ggsave(paste0(prj_dir, "out/fig/figB3A_log_cefs_instru_boot.png"))

# Robustness to instruments - Rank CEFs
figB3B_rank_cefs_instru_boot <- merge.data.table(fread(paste0(prj_dir, "out/fig/figB3B_rank_cefs_instru.csv")),
                                                instru_boot[estimator == "RRC"][, estimator := NULL], 
                                                all.x = T, by = "instru")
fwrite(figB3B_rank_cefs_instru_boot, paste0(prj_dir, "out/fig/figB3B_rank_cefs_instru_boot.csv"))

ggplot(figB3B_rank_cefs_instru_boot %>% mutate(instru = paste0(instru, "\n RRC=", round(coef, 2), " (", round(boot_se, 3), ")")), 
       aes(x = family_rank, y = child_rank)) + geom_point(alpha = .5) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = F) + facet_wrap(~instru, nrow = 1)
ggsave(paste0(prj_dir, "out/fig/figB3B_rank_cefs_instru_boot.png"))

# Robustness to instruments - Transition matrix
tm_instru_boot <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/tm_instruments_", x, ".csv"))}) %>% rbindlist()
tm_instru_boot <- tm_instru_boot[, .(boot_se = sd(share)), by = c("family_quintile", "child_quintile", "instru")]

figB3C_tms_boot <- merge.data.table(fread(paste0(prj_dir, "out/fig/figB3C_tms.csv")),
                                    tm_instru_boot, all.x = T, 
                                    by = c("family_quintile", "child_quintile", "instru"))
fwrite(figB3C_tms_boot, paste0(prj_dir, "out/fig/figB3C_tms_boot.csv"))

ggplot(figB3C_tms_boot, aes(x = family_quintile, y = share, fill = child_quintile, label = pct)) +
  geom_bar(position = "fill", stat = "identity", width = .99) +
  geom_text(position = position_stack(vjust = .5), color = "white") +
  facet_wrap(~instru, nrow = 1)
ggsave(paste0(prj_dir, "out/fig/figB3C_tms_boot.png"))

# Robustness to income definitions
inc_def_boot <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/inc_def_", x, ".csv"))}) %>% rbindlist()
inc_def_boot <- inc_def_boot[, .(boot_se = sd(coef)), by = c("child_var", "parents_var", "estimator", "subsample", "replace0")]
tm_inc_def_boot <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/tm_inc_def_", x, ".csv"))}) %>% rbindlist()
tm_inc_def_boot <- tm_inc_def_boot[, .(boot_se = sd(share)), by = c("child_quintile", "family_quintile", "variable")]

# Robustness to income definitions - IGE
figE3_IGE_child_inc_var_boot <- merge.data.table(fread(paste0(prj_dir, "out/fig/figE3_IGE_child_inc_var.csv")),
                                                 inc_def_boot %>%
                                                   .[estimator == "IGE" & replace0 == 0] %>%
                                                   .[,  ":=" (estimator = NULL, replace0 = NULL, subsample = gsub("\\|", "", subsample))], 
                                                all.x = T, by = c("child_var", "parents_var", "subsample")) %>%
  .[, ":=" (boot_lb = coef - qt(.975, n-11) * boot_se,
            boot_ub = coef + qt(.975, n-11) * boot_se)]
fwrite(figE3_IGE_child_inc_var_boot, paste0(prj_dir, "out/fig/figE3_IGE_child_inc_var_boot.csv"))

ggplot(figE3_IGE_child_inc_var_boot, aes(x = subsample, y = coef, fill = child_var, label = lab)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = boot_lb, ymax = boot_ub), position = position_dodge()) +
  geom_text() + facet_wrap(~parents_var, scales = "free_x")
ggsave(paste0(prj_dir, "out/fig/figE3_IGE_child_inc_var_boot.png"))

# Robustness to income definitions - RRC
figE4_RRC_child_inc_var_boot <- merge.data.table(fread(paste0(prj_dir, "out/fig/figE4_RRC_child_inc_var.csv")),
                                                 inc_def_boot %>%
                                                   .[estimator == "RRC" & replace0 == 0] %>%
                                                   .[,  ":=" (estimator = NULL, replace0 = NULL, subsample = gsub("\\|", "", subsample))], 
                                                 all.x = T, by = c("child_var", "parents_var", "subsample")) %>%
  .[, ":=" (boot_lb = coef - qt(.975, n-11) * boot_se,
            boot_ub = coef + qt(.975, n-11) * boot_se)]
fwrite(figE4_RRC_child_inc_var_boot, paste0(prj_dir, "out/fig/figE4_RRC_child_inc_var_boot.csv"))

ggplot(figE4_RRC_child_inc_var_boot, aes(x = subsample, y = coef, fill = child_var, label = lab)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = boot_lb, ymax = boot_ub), position = position_dodge()) +
  geom_text() + facet_wrap(~parents_var, scales = "free_x")
ggsave(paste0(prj_dir, "out/fig/figE4_RRC_child_inc_var_boot.png"))

# Robustness to income definitions - TM
figE5_TM_child_inc_var_boot <- merge.data.table(fread(paste0(prj_dir, "out/fig/figE5_TM_child_inc_var.csv")), all.x = T,
                                                tm_inc_def_boot, by = c("child_quintile", "family_quintile", "variable"))
fwrite(figE5_TM_child_inc_var_boot, paste0(prj_dir, "out/fig/figE5_TM_child_inc_var_boot.csv"))

ggplot(figE5_TM_child_inc_var_boot, aes(x = family_quintile, y = share, 
                                        fill = factor(-child_quintile), label = pct)) +
  geom_bar(position = "fill", stat = "identity", width = .99) +
  geom_text(position = position_stack(vjust = .5), color = "white") +
  facet_wrap(~variable)
ggsave(paste0(prj_dir, "out/fig/figE5_TM_child_inc_var_boot.png"))

# Robustness to income definitions - 0-income replacement
figB8_0_replacement_boot <- merge.data.table(
  fread(paste0(prj_dir, "out/fig/figB8_0_replacement.csv")),
  inc_def_boot %>%
    .[child_var != "household_wage" & parents_var != "father_income"] %>%
    .[,  ":=" (parents_var = NULL, subsample = gsub("\\|", "", subsample))],
  by = c("child_var", "estimator", "subsample", "replace0"), all.x = T) %>%
  .[, ":=" (boot_lb = coef - qt(.975, n-11) * boot_se,
            boot_ub = coef + qt(.975, n-11) * boot_se)]
fwrite(figB8_0_replacement_boot, paste0(prj_dir, "out/fig/figB8_0_replacement_boot.csv"))

ggplot(figB8_0_replacement_boot, aes(x = subsample, y = coef, fill = factor(replace0), label = lab)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = boot_lb, ymax = boot_ub), position = "dodge") +
  geom_text() + facet_grid(estimator~child_var, scales = "free_x")
ggsave(paste0(prj_dir, "out/fig/figB8_0_replacement_boot.png"))

# Lifecycle bias
child_lifecycle_AEP <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/child_lifecycle_AEP_", x, ".csv"))}) %>% rbindlist()
child_lifecycle_tax <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/child_lifecycle_tax_", x, ".csv"))}) %>% rbindlist()
child_lifecycle <- rbind(child_lifecycle_AEP, child_lifecycle_tax)
child_lifecycle_boot <- child_lifecycle[, .(boot_se = sd(coef)), by = c("sample", "age", "estimate", "group")]

fig4_boot <- merge.data.table(fread(paste0(prj_dir, "out/fig/fig4_child_lifecycle.csv")),
                              child_lifecycle_boot, all.x = T, by = c("sample", "age", "estimate", "group"))
fwrite(fig4_boot, paste0(prj_dir, "out/fig/fig4_child_lifecycle_boot.csv"))

ggplot(fig4_boot, aes(x = age, y = coef, color = group, linetype = sample, shape = sample)) + 
  geom_point() + geom_line() + facet_wrap(~estimate, nrow = 1, scales = "free")
ggsave(paste0(prj_dir, "out/fig/fig4_child_lifecycle_boot.png"))

# Lifecycle bias - constant
child_lifecycle_constant <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/child_lifecycle_constant_", x, ".csv"))}) %>% rbindlist()
child_lifecycle_constant_boot <- child_lifecycle_constant[, .(boot_se = sd(coef)), by = c("sample", "age", "estimate", "group")]

figB4_boot <- merge.data.table(fread(paste0(prj_dir, "out/fig/figB4_child_lifecycle_constant.csv")),
                               child_lifecycle_constant_boot, all.x = T, by = c("sample", "age", "estimate", "group"))
fwrite(figB4_boot, paste0(prj_dir, "out/fig/figB4_child_lifecycle_constant_boot.csv"))

ggplot(figB4_boot, aes(x = age, y = coef, color = group, linetype = factor(sample), shape = factor(sample))) + 
  geom_point() + geom_line() + facet_wrap(~estimate, nrow = 1, scales = "free")
ggsave(paste0(prj_dir, "out/fig/figB4_child_lifecycle_constant_boot.png"))

# Child number of observations
child_attenuation <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/child_attenuation_", x, ".csv"))}) %>% rbindlist()
child_attenuation_boot <- child_attenuation[, .(boot_se = sd(coef)), by = c("n_obs", "estimate", "group")]
figB6_boot <- merge.data.table(fread(paste0(prj_dir, "out/fig/figB6_child_n_inc_observations.csv")),
                               child_attenuation_boot, all.x = T, by = c("n_obs", "estimate", "group"))
fwrite(figB6_boot, paste0(prj_dir, "out/fig/figB6_child_n_inc_observations_boot.csv"))

ggplot(figB6_boot, aes(x = n_obs, y = coef, color = group)) +
  geom_point() + geom_line() + facet_wrap(~estimate, nrow = 1, scales = "fixed")
ggsave(paste0(prj_dir, "out/fig/figB6_child_n_inc_observations_boot.png"))

# Parents lifecycle bias
parents_lifecycle <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/parents_lifecycle_", x, ".csv"))}) %>% rbindlist()
parents_lifecycle_boot <- parents_lifecycle[, .(boot_se = sd(coef)), by = c("age", "estimate", "group")]
fig5_boot <- merge.data.table(fread(paste0(prj_dir, "out/fig/fig5_parents_lifecycle.csv")),
                              parents_lifecycle_boot, all.x = T, by = c("age", "estimate", "group"))
fwrite(fig5_boot, paste0(prj_dir, "out/fig/fig5_parents_lifecycle_boot.csv"))

ggplot(fig5_boot, aes(x = age, y = coef, color = group)) +
  geom_point() + geom_line() + facet_wrap(~estimate, nrow = 1, scales = "free")
ggsave(paste0(prj_dir, "out/fig/fig5_parents_lifecycle_boot.png"))

# Parents attenuation bias
parents_attenuation <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/parents_attenuation_", x, ".csv"))}) %>% rbindlist()
parents_attenuation_boot <- parents_attenuation[, .(boot_se = sd(coef)), by = c("nobs", "sample", "parent_var", "estimate", "group")]

fig6_boot <- merge.data.table(fread(paste0(prj_dir, "out/fig/fig6_parents_attenuation.csv")),
                              parents_attenuation_boot[sample == "Moving"][, sample := NULL], 
                              all.x = T, by = c("nobs", "parent_var", "estimate", "group"))
fwrite(fig6_boot, paste0(prj_dir, "out/fig/fig6_parents_attenuation_boot.csv"))

ggplot(fig6_boot, aes(x = nobs, y = coef, color = group, linetype = parent_var)) +
  geom_point() + geom_line() + facet_wrap(~estimate, nrow = 1)
ggsave(paste0(prj_dir, "out/fig/fig6_parents_attenuation_boot.png"))

# Parents attenuation bias - constant
figB7_boot <- merge.data.table(fread(paste0(prj_dir, "out/fig/figB7_parents_attenuation_constant.csv")),
                              parents_attenuation_boot[sample == "Constant"][, sample := NULL], 
                              all.x = T, by = c("nobs", "parent_var", "estimate", "group"))
fwrite(figB7_boot, paste0(prj_dir, "out/fig/figB7_parents_attenuation_constant_boot.csv"))

ggplot(figB7_boot, aes(x = nobs, y = coef, color = group, linetype = parent_var)) +
  geom_point() + geom_line() + facet_wrap(~estimate, nrow = 1)
ggsave(paste0(prj_dir, "out/fig/figB7_parents_attenuation_constant_boot.png"))

# Post-88 income
post_88 <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/post_88_", x, ".csv"))}) %>% rbindlist()
post_88_boot <- post_88[, .(boot_se = sd(coef)), by = c("inc_period", "subsample","estimate", "group")]
figB1_boot <- merge.data.table(fread(paste0(prj_dir, "out/fig/figB1_post_88_income.csv")),
                               post_88_boot[, subsample := gsub("\\|", "", subsample)], 
                              all.x = T, by = c("inc_period", "subsample","estimate", "group"))
fwrite(figB1_boot, paste0(prj_dir, "out/fig/figB1_post_88_income_boot.csv"))

ggplot(figB1_boot, aes(x = subsample, y = coef, fill = group, color = inc_period, label = lab)) +
  geom_bar(stat = "identity", position = "dodge") + geom_text() +
  facet_wrap(~estimate, nrow = 1, scales = "free")
ggsave(paste0(prj_dir, "out/fig/figB1_post_88_income_boot.png"))

# Spatial
#########

# Figures ordered dep IGE
tab_F9 <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/tabF9_", x, ".csv"))}) %>% rbindlist()
tab_F9_boot <- tab_F9[, .(boot_se_ige = sd(ige), boot_se_rrc = sd(rrc), boot_se_aum = sd(aum)), by = "dep"]
library(sf) # Version 1.0.9
dep_labs <- read_sf(paste0(prj_dir, "data/dep.shp")) %>% st_drop_geometry()
figE12_boot <- merge.data.table(merge.data.table(fread(paste0(prj_dir, "out/fig/figE12_sorted_ige.csv")), dep_labs),
                           tab_F9_boot[, .(dep, boot_se_ige)], by = "dep", all.x = T, all.y = F) %>%
  .[, ":=" (lb_boot = ige - (boot_se_ige*qt(.975, n - 2)), ub_boot = ige + (boot_se_ige*qt(.975, n - 2)))]
fwrite(figE12_boot, paste0(prj_dir, "out/fig/figE12_sorted_ige_boot.csv"))

ggplot(figE12_boot, aes(x = reorder(dep_lab, ige), y = ige, ymin = lb_boot, ymax = ub_boot)) +
  geom_point() + geom_errorbar() + 
  geom_hline(yintercept = figE12_boot$coef[1], linetype = "dashed") + coord_flip()
ggsave(paste0(prj_dir, "out/fig/figE12_sorted_ige_boot.png"))

# Figures ordered dep RRC
figE13_boot <- merge.data.table(merge.data.table(fread(paste0(prj_dir, "out/fig/figE13_sorted_rrc.csv")), dep_labs),
                                tab_F9_boot[, .(dep, boot_se_rrc)], by = "dep", all.x = T, all.y = F) %>%
  .[, ":=" (lb_boot = rrc - (boot_se_rrc*qt(.975, n - 2)), ub_boot = rrc + (boot_se_rrc*qt(.975, n - 2)))]
fwrite(figE13_boot, paste0(prj_dir, "out/fig/figE13_sorted_rrc_boot.csv"))

ggplot(figE13_boot, aes(x = reorder(dep_lab, rrc), y = rrc, ymin = lb_boot, ymax = ub_boot)) +
  geom_point() + geom_errorbar() + 
  geom_hline(yintercept = figE13_boot$coef[1], linetype = "dashed") + coord_flip()
ggsave(paste0(prj_dir, "out/fig/figE13_sorted_rrc_boot.png"))

# Figures ordered dep AUM
fig_ordered_aum_boot <- merge.data.table(merge.data.table(fread(paste0(prj_dir, "out/tab/tabF9_dep_IGM.csv")), dep_labs),
                 tab_F9_boot[, .(dep, boot_se_aum)], by = "dep", all.x = T, all.y = F) %>%
  .[, ":=" (lb_boot = aum - (boot_se_aum*qt(.975, N - 2)), ub_boot = aum + (boot_se_aum*qt(.975, N - 2)))]
fwrite(fig_ordered_aum_boot, paste0(prj_dir, "out/fig/fig_ordered_aum_boot.csv"))

ggplot(fig_ordered_aum_boot %>% na.omit(), aes(x = reorder(dep_lab, aum), y = aum, ymin = lb_boot, ymax = ub_boot)) +
  geom_point() + geom_errorbar() + coord_flip()
ggsave(paste0(prj_dir, "out/fig/fig_ordered_aum_boot.png"))


# Figure separate regressions
figD1 <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/figD1_", x, ".csv"))}) %>% rbindlist()
figD1_boot <- figD1[, .(boot_se = sd(coef)), by = c("measure", "var")]

figD1_boot <- merge.data.table(fread(paste0(prj_dir, "out/fig/figD1_separate_regressions.csv")),
                figD1_boot, by = c("measure", "var"),  all.x = T) %>%
  .[, ":=" (lb_boot = coef - (boot_se*qt(.975, n - 2)), ub_boot = coef + (boot_se*qt(.975, n - 2)))]
fwrite(figD1_boot, paste0(prj_dir, "out/fig/figD1_separate_regressions_boot.csv"))

ggplot(figD1_boot, aes(x = var, y = coef, ymin = lb_boot, ymax = ub_boot)) +
  geom_point() + geom_errorbar()  + facet_wrap(~measure) +
  geom_hline(yintercept = 0, linetype = "dashed") + coord_flip()
ggsave(paste0(prj_dir, "out/fig/figD1_separate_regressions_boot.png"))

## Redo regression
##################
library(stargazer)  # Version 5.2.3
library(data.table) # Version 1.14.6

set.seed(1)

# Paths
prj_dir <- "C:/Users/Public/Documents/gustave_k/intergen_income_mob/jpube/revision/"

# Clean datasets
children <- fread(paste0(prj_dir, "data/children.csv"), na.strings = "")
syn_parents <- fread(paste0(prj_dir, "data/syn_parents.csv"), na.strings = "")
dep_shp <- read_sf(paste0(prj_dir, "data/dep.shp"))

# Synthetic parents selection
syn_parents_FS <- copy(syn_parents) %>%
  # Keep individuals born in an even year, with positive average income computed
  # over at least 2 income observations
  .[birth_year %% 2 == 0 & avg_netinc_3545 > 0 & n_netinc_3545 >= 2, ] %>%
  # Recode log income, gender, and occupation variables
  .[, ":=" (ln_parents_income_var = log(avg_netinc_3545),
            gender = as.numeric(gender == "F"))] %>%
  .[cs2_90 %in% c(11:13, 31), cs2_90 := NA] %>%
  # Keep identifier, log income, and predictors
  .[, .SD, .SDcols = c("ID_DIFF", "ln_parents_income_var", "gender", "age_90", 
                       "born_french", "unemp_mun", "foreign_mun", "single_mun", 
                       "density_mun", "pop_mun", "cbirth", "edu_90", "cs2_90", 
                       "type_fam")] %>%
  # Drop individuals with missing values and duplicates
  na.omit() %>% distinct()

# One hot encode discrete variables
syn_encoded <- copy(syn_parents_FS)
for (i in c("cbirth", "edu_90", "cs2_90", "type_fam")) {
  for (j in sort(unique(syn_encoded[[i]]))[-1]) {
    syn_encoded[, (paste(i, j, sep = "_")) := lapply(.SD, function(x) {as.numeric(x == j)}), .SDcols = i]
  }
}

# Remove discrete variables
syn_encoded[, (c("cbirth", "edu_90", "cs2_90", "type_fam")) := NULL]

# One hot encoded variables
bin_list <- names(syn_encoded)[!names(syn_encoded) %in% 
                                 c("ID_DIFF", "gender", "ln_parents_income_var")]

# Create folds
syn_encoded <- syn_encoded %>%
  # Sort individuals randomly
  .[order(gender, runif(.N, 0, 1)), ] %>%
  # Divide into 5 folds
  .[, fold := ceiling(5 * (1:.N / .N)), by = gender]

# Predict
for (x in 1:5) {
  for (y in 0:1) {
    syn_encoded[gender == y & fold == x, pred := predict(
      lm(paste("ln_parents_income_var ~", paste0("`", bin_list, "`", collapse = "+")), 
         syn_encoded[gender == y & fold != x]), newdata = syn_encoded[gender == y & fold == x])]
  }
}

# MSE by department
dep_mse <- merge.data.table(syn_encoded[, .(ID_DIFF, gender, ln_parents_income_var, pred)],
                            distinct(syn_parents[, .(ID_DIFF, gender = as.numeric(gender == "F"), dep)]), 
                            all.x = T, by = c("ID_DIFF", "gender")) %>%
  .[, .(mse = mean((pred - ln_parents_income_var)^2)), by = dep]

first_stage <- function(
    # Binary and continuous instruments
  numeric_instru = c("gender", "age_90", "born_french", "unemp_mun", 
                     "foreign_mun", "single_mun", "density_mun", "pop_mun"),
  # Categorical instruments
  factor_instru = c("cbirth", "edu_90", "cs2_90", "type_fam"),
  # Parent income variable to predict
  parents_inc = "avg_netinc_3545",
  # Bottom and top trimming
  remove_bottom = 0,
  remove_top = 0,
  # Minimum number of income observations
  min_inc_obs_var = "n_netinc_3545",
  min_inc_obs = 2) {
  
  # Synthetic parents selection
  syn_parents_FS <- copy(syn_parents) %>%
    # Keep individuals born in an even year, with positive average income computed
    # over at least 2 income observations
    .[birth_year %% 2 == 0 & get(parents_inc) > 0 & 
        get(min_inc_obs_var) >= min_inc_obs, ] %>%
    # Recode log income, gender, and occupation variables
    .[, ":=" (ln_parents_income_var = log(get(parents_inc)),
              gender = as.numeric(gender == "F"))] %>%
    .[cs2_90 %in% c(11:13, 31), cs2_90 := NA] %>%
    # Keep identifier, log income, and predictors
    .[, .SD, .SDcols = c("ID_DIFF", "ln_parents_income_var", numeric_instru, factor_instru)] %>%
    # Drop individuals with missing values and duplicates
    na.omit() %>% distinct()
  
  # Same on actual mothers and fathers
  chl_fathers <- children[family_link == 3, ] %>%
    .[father_cs2_90 %in% c(11:13, 31), father_cs2_90 := NA] %>%
    .[, gender := 0] %>% 
    .[, .SD, .SDcols = c("ID_DIFF", names(children)[names(children) %in% c(numeric_instru, factor_instru, 
                                                                           paste0("father_", c(numeric_instru, factor_instru)))])] 
  
  chl_mothers <- children[family_link == 3, ] %>%
    .[mother_cs2_90 %in% c(11:13, 31), mother_cs2_90 := NA] %>%
    .[, gender := 1] %>% 
    .[, .SD, .SDcols = c("ID_DIFF", names(children)[names(children) %in% c(numeric_instru, factor_instru, 
                                                                           paste0("mother_", c(numeric_instru, factor_instru)))])] 
  
  names(chl_fathers) <- sub("^father_", "", names(chl_fathers))
  names(chl_mothers) <- sub("^mother_", "", names(chl_mothers))
  
  for (i in factor_instru) {
    chl_fathers <- chl_fathers[get(i) %in% unique(syn_parents_FS[gender == 0, get(i)]), ]
    chl_mothers <- chl_mothers[get(i) %in% unique(syn_parents_FS[gender == 1, get(i)]), ]
  }
  
  chl_encoded <- rbind(chl_mothers, chl_fathers)
  
  # One hot encode discrete variables
  syn_encoded <- copy(syn_parents_FS)
  for (i in factor_instru) {
    # Synthetic parents 
    for (j in sort(unique(syn_encoded[[i]]))[-1]) {
      syn_encoded[, (paste(i, j, sep = "_")) := lapply(.SD, function(x) {as.numeric(x == j)}), .SDcols = i]
    }
    # Actual parents
    for (j in sort(unique(chl_encoded[[i]]))[-1]) {
      chl_encoded[, (paste(i, j, sep = "_")) := lapply(.SD, function(x) {as.numeric(x == j)}), .SDcols = i]
    }
  }
  
  # Remove discrete variables
  syn_encoded[, (factor_instru) := NULL]
  chl_encoded[, (factor_instru) := NULL]
  
  # One hot encoded variables
  bin_list <- names(syn_encoded)[!names(syn_encoded) %in% 
                                   c("ID_DIFF", "gender", "ln_parents_income_var")]
  
  
  mother_fstage_dt <- syn_encoded[gender == 1, ] %>%
    .[ln_parents_income_var >= quantile(ln_parents_income_var, remove_bottom, na.rm = TRUE) & 
        ln_parents_income_var <= quantile(ln_parents_income_var, 1 - remove_top, na.rm = TRUE)]
  # Prediction models by gender
  mother_fstage <- lm(paste("ln_parents_income_var ~", paste0("`", bin_list, "`", collapse = "+")), 
                      mother_fstage_dt)
  
  father_fstage_dt <- syn_encoded[gender == 0, ] %>%
    .[ln_parents_income_var >= quantile(ln_parents_income_var, remove_bottom, na.rm = TRUE) & 
        ln_parents_income_var <= quantile(ln_parents_income_var, 1 - remove_top, na.rm = TRUE)]
  
  father_fstage <- lm(paste("ln_parents_income_var ~", paste0("`", bin_list, "`", collapse = "+")), 
                      father_fstage_dt)
  
  pooled_fstage_dt <- syn_encoded %>%
    .[ln_parents_income_var >= quantile(ln_parents_income_var, remove_bottom, na.rm = TRUE) & 
        ln_parents_income_var <= quantile(ln_parents_income_var, 1 - remove_top, na.rm = TRUE)]
  
  pooled_fstage <- lm(paste("ln_parents_income_var ~ gender +", paste0("`", bin_list, "`", collapse = "+"), 
                            "+", paste0("gender*`", bin_list, "`", collapse = "+")), pooled_fstage_dt)
  
  # Merge with mother and father predicted income
  predictions <- chl_encoded %>%
    .[gender == 1, .(ID_DIFF)] %>% 
    # Mother predicted income
    .[, mother_log_income := 
        predict(mother_fstage, newdata = chl_encoded %>%
                  .[gender == 1] %>%
                  .[, ":=" (gender = NULL, ID_DIFF = NULL)])] %>%
    merge.data.table(chl_encoded %>%
                       .[gender == 0, .(ID_DIFF)] %>% 
                       # Father predicted income
                       .[, father_log_income := 
                           predict(father_fstage, newdata = chl_encoded %>%
                                     .[gender == 0] %>%
                                     .[, ":=" (gender = NULL, ID_DIFF = NULL)])],
                     by = "ID_DIFF", all.x = T, all.y = T)
  
  return(list(predictions, data.table(Label = c("Mother", "Father", "Pooled"),
                                      Figure = c(summary(mother_fstage)$r.squared,
                                                 summary(father_fstage)$r.squared,
                                                 summary(pooled_fstage)$r.squared),
                                      N = c(nrow(mother_fstage_dt), 
                                            nrow(father_fstage_dt), 
                                            nrow(pooled_fstage_dt)))))
}

# Add predictions to children
baseline_fs <- first_stage()

children <- children %>%
  merge.data.table(baseline_fs[[1]], by = "ID_DIFF", all.x = T, all.y = T)

# Compute child and family log income
children <- children %>%
  .[, family_log_income := log(ifelse(is.na(mother_log_income), 
                                      0, exp(mother_log_income)) +
                                 ifelse(is.na(father_log_income), 
                                        0, exp(father_log_income)))] %>%
  .[, family_income := exp(family_log_income)] %>%
  .[, family_log_income_n := log((ifelse(is.na(mother_log_income), 
                                         0, exp(mother_log_income)) +
                                    ifelse(is.na(father_log_income), 
                                           0, exp(father_log_income))) /
                                   (as.numeric(!is.na(mother_log_income)) +
                                      as.numeric(!is.na(father_log_income))))] %>%
  .[ , ":=" (family_income_n = exp(family_log_income_n),
             household_income = avg_revtotnonpsocm_real_2010_16_3545,
             household_income_n = avg_revtotnonpsocm_real_n_2010_16_3545)] %>%
  .[, ":=" (household_log_income_n = ifelse(household_income_n > 0, log(household_income_n), NA),
            household_log_income = ifelse(household_income > 0, log(household_income), NA))]

children <- children %>%
  # Identify incoherent family structures
  .[ , missing_parent := ifelse((is.na(father_log_income) & !type_fam %in% c("Both parents (mother active only)", "Mother only")) | 
                                  (is.na(mother_log_income) & !type_fam %in% c("Both parents (father active only)", "Father only")), 1, 0)] %>%
  # Rename main income variables
  .[, ":=" (household_wage_n = avg_revsalm_real_n_2010_16_3545,
            household_wage = avg_revsalm_real_2010_16_3545,
            individual_income = avg_ytot_real_2010_16_3545,
            labor_income = avg_ysali_real_2010_16_3545)] %>%
  .[!is.na(father_log_income), father_income := exp(father_log_income)] %>%
  .[, constant_sample := family_link == 3 & !is.na(avg_ysali_real_2010_16_3545) & !is.na(avg_revtotnonpsocm_real_2010_16_3545) & 
      ((mother_cs2_90 > 20 & mother_cs2_90 != 31 | is.na(mother_cs2_90)) &
         (father_cs2_90 > 20 & father_cs2_90 != 31 | is.na(father_cs2_90)))]

gen_ranks <- function(data, child_inc_var, parents_inc_var) {
  
  if (grepl("^family", parents_inc_var)) {
    dat <- data %>%
      .[, rank_parents_income := ifelse(missing_parent == 1, NA, get(parents_inc_var))] %>%
      .[rank_parents_income < 0, rank_parents_income := 0] %>%
      .[, rank_child_income := ifelse(get(child_inc_var) < 0, 0, get(child_inc_var))]
  } else {
    dat <- data[, rank_parents_income := get(parents_inc_var)] %>%
      .[rank_parents_income < 0, rank_parents_income := 0] %>%
      .[, rank_child_income := ifelse(get(child_inc_var) < 0, 0, get(child_inc_var))]
  }
  
  # Name of the variables
  child_rank_var <- paste(child_inc_var, parents_inc_var, "rank", sep = "_")
  parents_rank_var <- paste(parents_inc_var, child_inc_var, "rank", sep = "_")
  
  return(dat %>%
           .[, missing := (!constant_sample) | is.na(get(child_inc_var)) | is.na(rank_parents_income) | is.na(get(parents_inc_var))] %>%
           # Rank child
           .[order(missing, birth_year, get(child_inc_var), ID_DIFF), ] %>% 
           # Compute percentile
           .[, (child_rank_var) := ceiling(100 * (1:.N / .N)), by = c("missing", "birth_year")]  %>%
           # Give the median rank to individuals with same income but different ranks
           .[, (child_rank_var) := ceiling(median(get(child_rank_var))), 
             by = c("missing", "birth_year", "rank_child_income")] %>%
           # Replace rank by missing if income missing
           .[, (child_rank_var) := ifelse(missing, NA, get(child_rank_var))] %>%
           # Rank parents
           .[order(missing, birth_year, get(parents_inc_var), ID_DIFF), ] %>%
           # Compute percentile
           .[, (parents_rank_var) := ceiling(100 * (1:.N / .N)), by = c("missing", "birth_year")] %>%
           # Give the median rank to individuals with same income but different ranks
           .[, (parents_rank_var) := ceiling(median(get(parents_rank_var))), 
             by = c("missing", "birth_year", "rank_parents_income")] %>%
           # Replace rank by missing if income missing
           .[, (parents_rank_var) := ifelse(missing, NA, get(parents_rank_var))] %>%
           # Remove temporary variables
           .[, ":=" (rank_parents_income = NULL, missing = NULL)])
}

child_inc_vars <- c("household_income", "household_wage", "individual_income", "labor_income") 
parents_inc_vars <- c("family_income_n", "father_income")

for (i in child_inc_vars) { 
  for (j in parents_inc_vars) { 
    children <- gen_ranks(children, i, j)
  }
}

for (i in c("household_income_n", "household_wage_n")) {
  for (j in c("family_income_n", "father_income")) {
    children <- gen_ranks(children, i, j)
  }
}

children <- gen_ranks(children, "household_income", "family_income")


geo_fs <- first_stage(numeric_instru = c("gender", "age_90", "born_french"))

children <- children %>%
  # Compute parent income without municipality characteristics in first stage
  merge.data.table(geo_fs[[1]] %>%
                     .[, .(ID_DIFF, mother_log_income_spatial = mother_log_income, 
                           father_log_income_spatial = father_log_income)], 
                   by = "ID_DIFF", all.x = T, all.y = T) %>%
  # Compute family income
  .[, family_log_income_n_spatial := log((ifelse(is.na(mother_log_income_spatial), 
                                                 0, exp(mother_log_income_spatial)) +
                                            ifelse(is.na(father_log_income_spatial), 
                                                   0, exp(father_log_income_spatial))) /
                                           (as.numeric(!is.na(mother_log_income_spatial)) +
                                              as.numeric(!is.na(father_log_income_spatial))))] %>%
  .[ , family_income_n_spatial := exp(family_log_income_n_spatial)] %>%
  # Identify incoherent family structures
  .[ , missing_parent_spatial := ifelse((is.na(father_log_income_spatial) & !type_fam %in% c("Both parents (mother active only)", "Mother only")) | 
                                          (is.na(mother_log_income_spatial) & !type_fam %in% c("Both parents (father active only)", "Father only")), 1, 0)] 

# Generate ranks
for (i in child_inc_vars) {children <- gen_ranks(children, i, "family_income_n_spatial")}

# Department results
dep_igm <- lapply(1:95, function(x) {
  
  lapply(c("household_income", "individual_income", "labor_income"), function(i) {
    
    ige <- summary(lm(paste0("log(", i, ") ~ family_log_income_n_spatial"),
                      children[get(i) > 0 & missing_parent_spatial == 0 & constant_sample & dep == x]))
    
    ige_with_geo <- summary(lm(paste0("log(", i, ") ~ family_log_income_n"),
                               children[get(i) > 0 & missing_parent == 0 & constant_sample & dep == x]))
    
    rrc <- summary(lm(paste0(i, "_family_income_n_spatial_rank ~ family_income_n_spatial_",
                             i, "_rank"), children[dep == x]))
    
    rrc_with_geo <- summary(lm(paste0(i, "_family_income_n_rank ~ family_income_n_",
                                      i, "_rank"), children[dep == x]))
    
    return(data.table(dep = x, child_var = i,
                      ige = ige$coefficients[2, 1],
                      ige_with_geo = ige_with_geo$coefficients[2, 1],
                      se_ige = ige$coefficients[2, 2],
                      n_ige = length(ige$residuals),
                      rrc = rrc$coefficients[2, 1],
                      rrc_with_geo = rrc_with_geo$coefficients[2, 1],
                      se_rrc = rrc$coefficients[2, 2],
                      n_rrc = length(rrc$residuals), 
                      aum = rrc$coefficients[1, 1] + 25*rrc$coefficients[2, 1],
                      aum_with_geo = rrc_with_geo$coefficients[1, 1] + 
                        25*rrc_with_geo$coefficients[2, 1]))
  }) %>% rbindlist() %>% return()
  
}) %>% rbindlist()

dep_igm <- merge.data.table(dep_igm, dep_mse, by = "dep", all.x = T)



# Dep MSE
cor_mse <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/corr_mse_", x, ".csv"))}) %>% rbindlist()
cor_mse_boot <- cor_mse[, .(boot_se_ige = sd(ige), boot_se_rrc = sd(rrc), boot_se_aum = sd(aum)), by = "row"]

tabF8 <- list(lm(ige ~ mse, dep_igm[child_var == "household_income" & n_rrc >= 200]),
              lm(rrc ~ mse, dep_igm[child_var == "household_income" & n_rrc >= 200]),
              lm(aum ~ mse, dep_igm[child_var == "household_income" & n_rrc >= 200])) %>% 
  stargazer(type = "latex", keep.stat = c("N", "rsq"), model.numbers = F,
            se = list(cor_mse_boot$boot_se_ige, cor_mse_boot$boot_se_rrc, cor_mse_boot$boot_se_aum))

writeLines(tabF8, paste0(prj_dir, "out/tab/tabF8_cor_local_MSE_IGM_boot.tex"))
###



dep_vars <- c("density_dep", "single_dep", "foreign_dep", "unemp_dep", "lmean_wage_dep", 
              "dist_edu_dep", "highschool_dep", "higher_edu_dep", "theil_dep", "sharetop1_dep", 
              "gini_dep", "amenities_dep", "pct_crimes_dep", "participation_dep")

dep_chars <- distinct(children[, .SD, .SDcols = c("dep", dep_vars)]) %>% na.omit() 

dep_regs <- dep_igm[child_var == "household_income", .(dep, n_rrc, ige, rrc, aum)] %>%
  left_join(dep_chars)

dep_regs <- dep_regs %>%
  .[n_rrc >= 200, ] %>%
  #.[n_rrc < 200, ":=" (ige = NA, rrc = NA, aum = NA)] %>% 
  .[, (names(dep_regs)[-(1:2)]) := lapply(.SD, function(x){x/sd(x, na.rm = T)}), .SDcols = names(dep_regs)[-(1:2)]] 

dep_res <- lapply(dep_vars, function(x) {
  return(list(lm(paste("ige ~", x), dep_regs),
              lm(paste("rrc ~", x), dep_regs),
              lm(paste("aum ~", x), dep_regs)))
})

# Separate regressions
dep_separate <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/dep_separate_regs_", x, ".csv"))}) %>% rbindlist()
dep_separate_boot <- dep_separate[, .(boot_se_ige = sd(ige), boot_se_rrc = sd(rrc), boot_se_aum = sd(aum)), by = c("n", "row")]

tabD2 <- lapply(1:14, function(x){dep_res[[x]][[1]]}) %>%
  stargazer(type = "latex", keep.stat = c("N", "rsq"),
            se = lapply(1:14, function(x){dep_separate_boot %>% 
                filter(row_number() %in% (2*x-1):(2*x)) %>% pull(boot_se_ige)}))

writeLines(tabD2, paste0(prj_dir, "out/tab/tabD2_separate_regs_ige_boot.tex"))


tabD3 <- lapply(1:14, function(x){dep_res[[x]][[2]]}) %>%
  stargazer(type = "latex", keep.stat = c("N", "rsq"),
            se = lapply(1:14, function(x){dep_separate_boot %>% 
                filter(row_number() %in% (2*x-1):(2*x)) %>% pull(boot_se_rrc)}))

writeLines(tabD3, paste0(prj_dir, "out/tab/tabD3_separate_regs_rrc_boot.tex"))

tabD4 <- lapply(1:14, function(x){dep_res[[x]][[3]]}) %>%
  stargazer(type = "latex", keep.stat = c("N", "rsq"),
            se = lapply(1:14, function(x){dep_separate_boot %>% 
                filter(row_number() %in% (2*x-1):(2*x)) %>% pull(boot_se_aum)}))

writeLines(tabD4, paste0(prj_dir, "out/tab/tabD4_separate_regs_aum_boot.tex"))

# Joint regressions
dep_joint <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/dep_joint_reg_", x, ".csv"))}) %>% rbindlist()
dep_joint_boot <- dep_joint[, .(boot_se_ige = sd(ige), boot_se_rrc = sd(rrc), boot_se_aum = sd(aum)), by = c("row")]

tabF11 <- list(lm(ige ~ density_dep + unemp_dep + gini_dep + highschool_dep + amenities_dep, dep_regs),
               lm(rrc ~ density_dep + unemp_dep + gini_dep + highschool_dep + amenities_dep, dep_regs),
               lm(aum ~ density_dep + unemp_dep + gini_dep + highschool_dep + amenities_dep, dep_regs)) %>%
  stargazer(type = "latex", keep.stat = c("N", "rsq"),
            se = list(dep_joint_boot$boot_se_ige, dep_joint_boot$boot_se_rrc, dep_joint_boot$boot_se_aum))

writeLines(tabF11, paste0(prj_dir, "out/tab/tabF11_joint_regressions_boot.tex"))

####

setDT(children)
children[, mobility := as.numeric(dep != adult_dep)] 
children[, ":=" (father_cs_nm = as.factor(ifelse(is.na(father_cs2_90), 0, father_cs2_90)),
                 mother_cs_nm = as.factor(ifelse(is.na(mother_cs2_90), 0, mother_cs2_90)),
                 father_edu_nm = as.factor(ifelse(is.na(father_edu_90), 0, father_edu_90)),
                 mother_edu_nm = as.factor(ifelse(is.na(mother_edu_90), 0, mother_edu_90)))]

# Geographic mobility - National ranks
geo_mob_national <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/geo_mob_national_", x, ".csv"))}) %>% rbindlist()
geo_mob_national_boot <- geo_mob_national[, .(boot_se = sd(coef)), by = c("reg", "row")]

tab3 <- list(lm(household_income_family_income_n_spatial_rank ~ 
                  family_income_n_spatial_household_income_rank*mobility + 
                  factor(birth_year), children), 
             lm(household_income_family_income_n_spatial_rank ~ 
                  family_income_n_spatial_household_income_rank*mobility + 
                  factor(birth_year) + factor(gender), children), 
             lm(household_income_family_income_n_spatial_rank ~ 
                  family_income_n_spatial_household_income_rank*mobility + 
                  factor(birth_year) + factor(gender) + factor(dep), children), 
             lm(household_income_family_income_n_spatial_rank ~ 
                  family_income_n_spatial_household_income_rank*mobility + 
                  factor(birth_year) + factor(gender) + factor(dep) + 
                  factor(mother_edu_nm) + factor(father_edu_nm), children), 
             lm(household_income_family_income_n_spatial_rank ~ 
                  family_income_n_spatial_household_income_rank*mobility + 
                  factor(birth_year) + factor(gender) + factor(dep) + 
                  factor(mother_edu_nm) + factor(father_edu_nm) +
                  factor(mother_cs_nm) + factor(father_cs_nm), children)) %>%
  stargazer(type = "latex", keep.stat = c("N", "rsq"),
            se = lapply(1:5, function(x){geo_mob_national_boot %>% filter(reg == x) %>% pull(boot_se)}))

writeLines(tab3, paste0(prj_dir, "out/tab/tab3_mobility_nat_rank_boot.tex"))

####


# Rank within department
children <- children %>%
  .[, missing := constant_sample == F | is.na(household_income) | is.na(family_income_n_spatial) | 
      missing_parent_spatial == 1 | constant_sample == F] %>%
  .[, rank_child_income := ifelse(household_income < 0, 0, household_income)] %>%
  # Rank child
  .[order(missing, adult_dep, birth_year, household_income, ID_DIFF), ] %>% 
  # Compute percentile
  .[, child_dep_rank := ceiling(100 * (1:.N / .N)), by = c("missing", "adult_dep", "birth_year")] %>%
  # Give the median rank to individuals with same income but different ranks
  .[, child_dep_rank := ceiling(median(child_dep_rank)), 
    by = c("missing", "adult_dep", "birth_year", "rank_child_income")] %>%
  # Replace rank by missing if income missing
  .[, child_dep_rank := ifelse(missing, NA, child_dep_rank)] %>%
  # Rank parents
  .[order(missing, dep, birth_year, family_income_n_spatial, ID_DIFF), ] %>%
  # Compute percentile
  .[, family_dep_rank := ceiling(100 * (1:.N / .N)), by = c("missing", "dep", "birth_year")] %>%
  # Give the median rank to individuals with same income but different ranks
  .[, family_dep_rank := ceiling(median(family_dep_rank)), 
    by = c("missing", "dep", "birth_year", "family_income_n_spatial")] %>%
  # Replace rank by missing if income missing
  .[, family_dep_rank := ifelse(missing, NA, family_dep_rank)] %>%
  # Remove temporary variables
  .[, ":=" (missing = NULL, rank_child_income = NULL)]

# Geographic mobility - Local ranks
geo_mob_local <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/geo_mob_local_", x, ".csv"))}) %>% rbindlist()
geo_mob_local_boot <- geo_mob_local[, .(boot_se = sd(coef)), by = c("reg", "row")]

tabF12 <- list(lm(child_dep_rank ~ 
                    family_dep_rank*mobility + 
                    factor(birth_year), children), 
               lm(child_dep_rank ~ 
                    family_dep_rank*mobility + 
                    factor(birth_year) + factor(gender), children), 
               lm(child_dep_rank ~ 
                    family_dep_rank*mobility + 
                    factor(birth_year) + factor(gender) + factor(dep), children), 
               lm(child_dep_rank ~ 
                    family_dep_rank*mobility + 
                    factor(birth_year) + factor(gender) + factor(dep) + 
                    factor(mother_edu_nm) + factor(father_edu_nm), children), 
               lm(child_dep_rank ~ 
                    family_dep_rank*mobility + 
                    factor(birth_year) + factor(gender) + factor(dep) + 
                    factor(mother_edu_nm) + factor(father_edu_nm) +
                    factor(mother_cs_nm) + factor(father_cs_nm), children)) %>%
  stargazer(type = "latex", keep.stat = c("N", "rsq"),
            se = lapply(1:5, function(x){geo_mob_local_boot %>% filter(reg == x) %>% pull(boot_se)}))

writeLines(tabF12, paste0(prj_dir, "out/tab/tabF12_mobility_dep_rank_boot.tex"))

###

mean_dep_rank <- copy(children) %>%
  .[!is.na(family_income_n_spatial_household_income_rank)] %>%
  .[, .(mean_dep_rank = mean(household_income_family_income_n_spatial_rank, na.rm = T),
        n_movers = sum(mobility == 1)), by = "adult_dep"]  %>%
  .[, dep_type := fcase(mean_dep_rank > 60, "High",
                        mean_dep_rank <= 60 & mean_dep_rank > 50, "Medium",
                        mean_dep_rank <= 50, "Low")]

# Geographic mobility - Destination department
geo_mob_destination <- lapply(1:1000, function(x){fread(paste0(prj_dir, "out/fig/bootstrap/geo_mob_destination_", x, ".csv"))}) %>% rbindlist()
geo_mob_destination_boot <- geo_mob_destination[, .(boot_se = sd(coef)), by = c("reg", "row")]

tabF13 <- list(lm(household_income_family_income_n_spatial_rank ~ 
                    family_income_n_spatial_household_income_rank*dep_type + 
                    factor(birth_year), children %>%
                    merge.data.table(mean_dep_rank) %>%
                    .[mobility  == 0, dep_type := "Stayers"] %>%
                    .[, dep_type := relevel(factor(dep_type), "Stayers")]), 
               lm(household_income_family_income_n_spatial_rank ~ 
                    family_income_n_spatial_household_income_rank*dep_type + 
                    factor(birth_year) + factor(gender), children %>%
                    merge.data.table(mean_dep_rank) %>%
                    .[mobility  == 0, dep_type := "Stayers"] %>%
                    .[, dep_type := relevel(factor(dep_type), "Stayers")]), 
               lm(household_income_family_income_n_spatial_rank ~ 
                    family_income_n_spatial_household_income_rank*dep_type + 
                    factor(birth_year) + factor(gender) + factor(dep), children %>%
                    merge.data.table(mean_dep_rank) %>%
                    .[mobility  == 0, dep_type := "Stayers"] %>%
                    .[, dep_type := relevel(factor(dep_type), "Stayers")]), 
               lm(household_income_family_income_n_spatial_rank ~ 
                    family_income_n_spatial_household_income_rank*dep_type + 
                    factor(birth_year) + factor(gender) + factor(dep) + 
                    factor(mother_edu_nm) + factor(father_edu_nm), children %>%
                    merge.data.table(mean_dep_rank) %>%
                    .[mobility  == 0, dep_type := "Stayers"] %>%
                    .[, dep_type := relevel(factor(dep_type), "Stayers")]), 
               lm(household_income_family_income_n_spatial_rank ~ 
                    family_income_n_spatial_household_income_rank*dep_type + 
                    factor(birth_year) + factor(gender) + factor(dep) + 
                    factor(mother_edu_nm) + factor(father_edu_nm) +
                    factor(mother_cs_nm) + factor(father_cs_nm), children %>%
                    merge.data.table(mean_dep_rank) %>%
                    .[mobility  == 0, dep_type := "Stayers"] %>%
                    .[, dep_type := relevel(factor(dep_type), "Stayers")])) %>%
  stargazer(type = "latex", keep.stat = c("N", "rsq"),
            se = lapply(1:5, function(x){geo_mob_destination_boot %>% filter(reg == x) %>% pull(boot_se)}))

writeLines(tabF13, paste0(prj_dir, "out/tab/tabF13_destination_type_boot.tex"))
