library(dplyr)
library(readxl)
library(splines)
library(writexl)
library(zoo)

wd = ""
setwd(wd)

control_wdi = read_excel('Data/Long Panel Control WDI.xlsx')
control_wdi2 = read_excel('Data/Long Panel Control WDI versi kedua.xlsx')

hetero_mou = read_excel('Data/Peserta BRI dan keberagaman MoU clean.xlsx')

lpi = read_excel('Data/Long Panel LPI and Control.xlsx')
country_lpi = unique(lpi$code)
control_wdi_selected = control_wdi[control_wdi$code %in% country_lpi,]
control_wdi2_selected = control_wdi2[control_wdi2$code %in% country_lpi,]

sorted_wdi = control_wdi_selected[order(control_wdi_selected$code, control_wdi_selected$year), ]
sorted_wdi2 = control_wdi2_selected[order(control_wdi2_selected$code, control_wdi2_selected$year), ]


for (i in 1:length(country_lpi)) {
  
  c = country_lpi[i]
  
  # c = 'SGP'
  if (c=='QAT' | c=='PAN' | c=='VEN' | c=='NGA' | c=='PNG' | c=='KGZ' | c=='LBR' | c=='GUY' | c=='DJI' | c=='AFG') {
    next
  }
  
  # c = 'SDN'
  sorted_wdi_country = sorted_wdi %>%
    filter(code %in% c)
  sorted_wdi_country$gdp_per_capita = na.locf(sorted_wdi_country$gdp_per_capita, na.rm=FALSE)
  
  sorted_wdi2_country = sorted_wdi2 %>%
    filter(code %in% c)
  sorted_wdi2_country$import_pct_usd = na.locf(sorted_wdi2_country$import_pct_usd, na.rm = FALSE)
  sorted_wdi2_country$import_pct_lcu = na.locf(sorted_wdi2_country$import_pct_lcu, na.rm = FALSE)
  sorted_wdi2_country$export_pct_usd = na.locf(sorted_wdi2_country$export_pct_usd, na.rm = FALSE)
  sorted_wdi2_country$export_pct_lcu = na.locf(sorted_wdi2_country$export_pct_lcu, na.rm = FALSE)
  sorted_wdi2_country$manufac_pct_usd = na.locf(sorted_wdi2_country$manufac_pct_usd, na.rm = FALSE)
  sorted_wdi2_country$manufac_pct_lcu = na.locf(sorted_wdi2_country$manufac_pct_lcu, na.rm = FALSE)
  sorted_wdi2_country$corruption_control = na.locf(sorted_wdi2_country$corruption_control, na.rm = FALSE)
  
  
  if (c==country_lpi[1]) {
    append_df = sorted_wdi_country
    append_df2 = sorted_wdi2_country
  } else {
    append_df = bind_rows(append_df, sorted_wdi_country)
    append_df2 = bind_rows(append_df2, sorted_wdi2_country)
  }
  
}

# Merged dataframe
append_df_selected = append_df %>%
  select(code, year, gdp_per_capita)

append_df2_selected = append_df2 %>%
  select(code, year, import_pct_usd, import_pct_lcu, export_pct_usd, export_pct_lcu, manufac_pct_usd, manufac_pct_lcu,
         corruption_control)

merged_lpi = inner_join(lpi, append_df_selected, by=c('code', 'year')) # Data siap diregresi
merged_lpi2 = inner_join(merged_lpi, append_df2_selected, by=c('code', 'year'))

# Filling missing values with 0 for import and export pct
merged_lpi2$import_pct_usd = ifelse(is.na(merged_lpi2$import_pct_usd), 0, merged_lpi2$import_pct_usd)
merged_lpi2$import_pct_lcu = ifelse(is.na(merged_lpi2$import_pct_lcu), 0, merged_lpi2$import_pct_lcu)
merged_lpi2$export_pct_usd = ifelse(is.na(merged_lpi2$export_pct_usd), 0, merged_lpi2$export_pct_usd)
merged_lpi2$export_pct_lcu = ifelse(is.na(merged_lpi2$export_pct_lcu), 0, merged_lpi2$export_pct_lcu)
merged_lpi2$manufac_pct_lcu = ifelse(is.na(merged_lpi2$manufac_pct_lcu), 0, merged_lpi2$manufac_pct_lcu)

# Merge with hetero_mou
merged_lpi3 = inner_join(merged_lpi2, hetero_mou, by='code')
merged_lpi3$priority = factor(merged_lpi3$priority)

merged_lpi3$priority_infra = ifelse(merged_lpi3$priority=='Infrastructure Development', 1, 0)
merged_lpi3$interact_infra = merged_lpi3$post * merged_lpi3$priority_infra
merged_lpi3$priority_trade = ifelse(merged_lpi3$priority=='Trade and Investments', 1, 0)
merged_lpi3$interact_trade = merged_lpi3$post * merged_lpi3$priority_trade
merged_lpi3$priority_policy = ifelse(merged_lpi3$priority=='Policy coordination', 1, 0)
merged_lpi3$interact_policy = merged_lpi3$post * merged_lpi3$priority_policy
merged_lpi3$priority_people = ifelse(merged_lpi3$priority=='People to people exchange', 1, 0)
merged_lpi3$interact_people = merged_lpi3$post * merged_lpi3$priority_people
merged_lpi3$priority_finance = ifelse(merged_lpi3$priority=='Financial Cooperation', 1, 0)
merged_lpi3$interact_finance = merged_lpi3$post * merged_lpi3$priority_finance

# Two-Way Fixed Effect
library(plm)
library(lmtest)
library(sandwich)
library(clubSandwich)

twfe_ol_wo = plm(overall_lpi ~ interact_infra + interact_people + interact_policy + interact_finance + interact_trade,
                 data=merged_lpi3,
                 index=c('code', 'year'),
                 model='within',
                 effect = 'twoway')
cse_ol_wo = coef_test(twfe_ol_wo, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_ol_controls = plm(overall_lpi ~ interact_infra + interact_people + interact_policy + interact_finance + interact_trade + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
                       data=merged_lpi3,
                       index=c('code', 'year'),
                       model='within',
                       effect = 'twoway')
cse_ol_controls = coef_test(twfe_ol_controls, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_customs_wo = plm(customs ~ interact_infra + interact_people + interact_policy + interact_finance + interact_trade,
                      data=merged_lpi3,
                      index=c('code', 'year'),
                      model='within',
                      effect = 'twoway')
cse_customs_wo = coef_test(twfe_customs_wo, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_customs_controls = plm(customs ~ interact_infra + interact_people + interact_policy + interact_finance + interact_trade + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
                            data=merged_lpi3,
                            index=c('code', 'year'),
                            model='within',
                            effect = 'twoway')
cse_customs_controls = coef_test(twfe_customs_controls, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_infra_wo = plm(infra ~ interact_infra + interact_people + interact_policy + interact_finance + interact_trade,
                    data=merged_lpi3,
                    index=c('code', 'year'),
                    model='within',
                    effect = 'twoway')
cse_infra_wo = coef_test(twfe_infra_wo, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_infra_controls = plm(infra ~ interact_infra + interact_people + interact_policy + interact_finance + interact_trade + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
                          data=merged_lpi3,
                          index=c('code', 'year'),
                          model='within',
                          effect = 'twoway')
cse_infra_controls = coef_test(twfe_infra_controls, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_ls_wo = plm(logistics_services ~ interact_infra + interact_people + interact_policy + interact_finance + interact_trade,
                 data=merged_lpi3,
                 index=c('code', 'year'),
                 model='within',
                 effect = 'twoway')
cse_ls_wo = coef_test(twfe_ls_wo, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_ls_controls = plm(logistics_services ~ interact_infra + interact_people + interact_policy + interact_finance + interact_trade + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
                       data=merged_lpi3,
                       index=c('code', 'year'),
                       model='within',
                       effect = 'twoway')
cse_ls_controls = coef_test(twfe_ls_controls, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_et_wo = plm(ease_tracking ~ interact_infra + interact_people + interact_policy + interact_finance + interact_trade,
                 data=merged_lpi3,
                 index=c('code', 'year'),
                 model='within',
                 effect = 'twoway')
cse_et_wo = coef_test(twfe_et_wo, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_et_controls = plm(ease_tracking ~ interact_infra + interact_people + interact_policy + interact_finance + interact_trade + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
                       data=merged_lpi3,
                       index=c('code', 'year'),
                       model='within',
                       effect = 'twoway')
cse_et_controls = coef_test(twfe_et_controls, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_timeliness_wo = plm(timeliness ~ interact_infra + interact_people + interact_policy + interact_finance + interact_trade,
                         data=merged_lpi3,
                         index=c('code', 'year'),
                         model='within',
                         effect = 'twoway')
cse_timeliness_wo = coef_test(twfe_timeliness_wo, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_timeliness_controls = plm(timeliness ~ interact_infra + interact_people + interact_policy + interact_finance + interact_trade + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
                               data=merged_lpi3,
                               index=c('code', 'year'),
                               model='within',
                               effect = 'twoway')
cse_timeliness_controls = coef_test(twfe_timeliness_controls, vcov = "CR1", cluster = "code", test = "naive-t")$SE


# coeftest(twfe_plm, vcov = vcovHC, type = "HC1") # Robust standard error
# cov = vcovHC(twfe_plm_controls, type = "HC1")
# robust.se = sqrt(diag(cov)) # robust standard error

library(stargazer)
stargazer(twfe_ol_wo, twfe_ol_controls, twfe_customs_wo, twfe_customs_controls, 
          se=list(cse_ol_wo, cse_ol_controls, cse_customs_wo, cse_customs_controls), 
          type = 'html',
          out = 'Laporan/Tabel Regresi Interaction Part 1.html', no.space = TRUE, align = TRUE,
          omit.stat=c("LL","ser","f"))

stargazer(twfe_infra_wo, twfe_infra_controls, twfe_ls_wo, twfe_ls_controls,
          se=list(cse_infra_wo, cse_infra_controls, cse_ls_wo, cse_ls_controls), 
          type = 'html',
          out = 'Laporan/Tabel Regresi Interaction Part 2.html', no.space = TRUE, align = TRUE,
          omit.stat=c("LL","ser","f"))

stargazer(twfe_et_wo, twfe_et_controls, twfe_timeliness_wo, twfe_timeliness_controls,
          se=list(cse_et_wo, cse_et_controls, cse_timeliness_wo, cse_timeliness_controls), 
          type = 'html',
          out = 'Laporan/Tabel Regresi Interaction Part 3.html', no.space = TRUE, align = TRUE,
          omit.stat=c("LL","ser","f"))


# Bacon-Goodman Decomposition
library(bacondecomp)
library(ggplot2)
library(gridExtra)


# Hanya Infratructure yang ditest karena BRI punya dampak signifikan ke infratrusture
df_bacon_lpi = bacon(
  infra ~ post + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
  data = merged_lpi2,
  id_var = "code",
  time_var = "year"
)

# weighted average of the decomposition if I include control variables
beta_hat_w_lpi = df_bacon_lpi$beta_hat_w
beta_hat_b_lpi = weighted.mean(df_bacon_lpi$two_by_twos$estimate, 
                               df_bacon_lpi$two_by_twos$weight)
Omega_lpi = df_bacon_lpi$Omega
bacon_coef_cont_lpi = Omega_lpi*beta_hat_w_lpi + (1 - Omega_lpi)*beta_hat_b_lpi
print(paste("Weighted sum of decomposition =", round(bacon_coef_cont_lpi, 4)))

# Plot
ggplot(df_bacon_lpi$two_by_twos) +
  aes(x = weight,
      y = estimate,
      shape = factor(type),
      colour = factor(type)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, 
             linetype = "longdash") +
  labs(x = "Weight",
       y = "Estimate",
       shape = "Type",
       colour = "Type",
       title = "Goodman-Bacon decomposition of estimates vs weights") +
  theme_minimal() +
  theme(legend.position = "bottom")

df_bacon_lpi$two_by_twos %>% 
  mutate(subgroup = paste0(treated, "_", untreated),
         subgroup = factor(subgroup),
         subgroup = forcats::fct_reorder(subgroup, estimate)) %>% 
  ggplot(aes(x = estimate, 
             y = subgroup,
             size = weight)) +
  geom_point() +
  geom_vline(xintercept = bacon_coef_cont_lpi,
             linetype = "longdash") +
  theme_minimal() +
  labs(size = "Weight",
       y = "Subgroup",
       x = "Estimate",
       title = "Goodman-Bacon decomposition",
       subtitle = "Dotted line shows two-way fixed effect estimate.",
       caption = "Subgroups 99999 is the never treated groups")

