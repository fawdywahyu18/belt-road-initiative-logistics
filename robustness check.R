library(dplyr)
library(readxl)
library(splines)
library(writexl)
library(zoo)

wd = ""
setwd(wd)

control_wdi = read_excel('Data/Long Panel Control WDI.xlsx')
control_wdi2 = read_excel('Data/Long Panel Control WDI versi kedua.xlsx')

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

# Balance testing before 2013 "treated" vs "untreated"
df_dummy_mou = merged_lpi2 %>%
  mutate(dummy_mou = ifelse(!is.na(year_mou), 1, 0))
  
before_2013 = df_dummy_mou %>%
  filter(year<2013) %>%
  select(code, country, dummy_mou, archipelagic, land_border, distance_to_china_km, inv_distance_to_china, gdp_per_capita,
         import_pct_lcu, export_pct_lcu, corruption_control, manufac_pct_lcu) %>%
  group_by(code) %>%
  summarize(country = first(country),
            archipelagic = first(archipelagic),
            land_border = first(land_border),
            distance_to_china_km = first(distance_to_china_km),
            dummy_mou = first(dummy_mou),
            gdp_per_capita = mean(gdp_per_capita),
            import_pct_lcu = mean(import_pct_lcu),
            export_pct_lcu = mean(export_pct_lcu),
            corruption_control = mean(corruption_control),
            manufac_pct_lcu = mean(manufac_pct_lcu))

library(MatchIt)
model_test = dummy_mou ~ log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu
m.out = matchit(model_test,
                data = before_2013, method = 'cardinality', ratio = 1,
                estimand = 'ATT', solver = 'highs')
summary(m.out)

summary_text = summary(m.out)
list_excel = list(before_matching = data.frame(summary_text$sum.all, 
                                               row_names = rownames(summary_text$sum.all)),
                  after_matching = data.frame(summary_text$sum.matched,
                                              row_names = rownames(summary_text$sum.matched)),
                  matching_summary = data.frame(summary_text$nn,
                                                row_names = rownames(summary_text$nn)))

# Filename to export as an excel file
words = c('Table and Graphs/Balance Testing Before and After Matching', '.xlsx')
file_name_export = paste(words, collapse = "")

write_xlsx(list_excel,
           file_name_export)

plot(m.out, type = "density")
plot(summary(m.out, interactions = FALSE),
     var.order = "unmatched")


# Extract the match data and weight
match_data = match.data(m.out)
data_weight = match_data %>%
  select(code, weights)

data_reg = inner_join(data_weight, merged_lpi2, by='code')
length(unique(data_reg$country))

# Jumlah negara yang mendapatkan treatment vs tidak mendapatkan treatment
treated = data_reg %>%
  filter(!is.na(year_mou))

length(unique(treated$country))

never_treated = data_reg %>%
  filter(is.na(year_mou))

length(unique(never_treated$country))


# Two-Way Fixed Effect
library(plm)
library(lmtest)
library(sandwich)
library(clubSandwich)
twfe_ol_wo = plm(overall_lpi ~ post,
                 data=data_reg,
                 index=c('code', 'year'),
                 model='within',
                 effect = 'twoway',
                 weights = weights)
cse_ol_wo = coef_test(twfe_ol_wo, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_ol_controls = plm(overall_lpi ~ post + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
                       data=data_reg,
                       index=c('code', 'year'),
                       model='within',
                       effect = 'twoway',
                       weights = weights)
cse_ol_controls = coef_test(twfe_ol_controls, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_customs_wo = plm(customs ~ post,
                      data=data_reg,
                      index=c('code', 'year'),
                      model='within',
                      effect = 'twoway',
                      weights = weights)
cse_customs_wo = coef_test(twfe_customs_wo, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_customs_controls = plm(customs ~ post + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
                            data=data_reg,
                            index=c('code', 'year'),
                            model='within',
                            effect = 'twoway',
                            weights = weights)
cse_customs_controls = coef_test(twfe_customs_controls, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_infra_wo = plm(infra ~ post,
                    data=data_reg,
                    index=c('code', 'year'),
                    model='within',
                    effect = 'twoway',
                    weights = weights)
cse_infra_wo = coef_test(twfe_infra_wo, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_infra_controls = plm(infra ~ post + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
                          data=data_reg,
                          index=c('code', 'year'),
                          model='within',
                          effect = 'twoway',
                          weights = weights)
cse_infra_controls = coef_test(twfe_infra_controls, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_ls_wo = plm(logistics_services ~ post,
                 data=data_reg,
                 index=c('code', 'year'),
                 model='within',
                 effect = 'twoway',
                 weights = weights)
cse_ls_wo = coef_test(twfe_ls_wo, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_ls_controls = plm(logistics_services ~ post + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
                          data=data_reg,
                          index=c('code', 'year'),
                          model='within',
                          effect = 'twoway',
                          weights = weights)
cse_ls_controls = coef_test(twfe_ls_controls, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_et_wo = plm(ease_tracking ~ post,
                 data=data_reg,
                 index=c('code', 'year'),
                 model='within',
                 effect = 'twoway',
                 weights = weights)
cse_et_wo = coef_test(twfe_et_wo, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_et_controls = plm(ease_tracking ~ post + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
                       data=data_reg,
                       index=c('code', 'year'),
                       model='within',
                       effect = 'twoway',
                       weights = weights)
cse_et_controls = coef_test(twfe_et_controls, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_timeliness_wo = plm(timeliness ~ post,
                         data=data_reg,
                         index=c('code', 'year'),
                         model='within',
                         effect = 'twoway',
                         weights = weights)
cse_timeliness_wo = coef_test(twfe_timeliness_wo, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twfe_timeliness_controls = plm(timeliness ~ post + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
                               data=data_reg,
                               index=c('code', 'year'),
                               model='within',
                               effect = 'twoway',
                               weights = weights)
cse_timeliness_controls = coef_test(twfe_timeliness_controls, vcov = "CR1", cluster = "code", test = "naive-t")$SE

# Two-Way Random Effect without weight
twre_ol_wo = plm(overall_lpi ~ post,
                 data=merged_lpi2,
                 index=c('code', 'year'),
                 model='random')
cse_ol_wo_re = coef_test(twre_ol_wo, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twre_ol_controls = plm(overall_lpi ~ post + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
                       data=merged_lpi2,
                       index=c('code', 'year'),
                       model='random')
cse_ol_controls_re = coef_test(twre_ol_controls, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twre_customs_wo = plm(customs ~ post,
                        data=merged_lpi2,
                        index=c('code', 'year'),
                        model='random')
cse_customs_wo_re = coef_test(twre_customs_wo, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twre_customs_controls = plm(customs ~ post + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
                            data=merged_lpi2,
                            index=c('code', 'year'),
                            model='random')
cse_customs_controls_re = coef_test(twre_customs_controls, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twre_infra_wo = plm(infra ~ post,
                    data=merged_lpi2,
                    index=c('code', 'year'),
                    model='random')
cse_infra_wo_re = coef_test(twre_infra_wo, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twre_infra_controls = plm(infra ~ post + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
                          data=merged_lpi2,
                          index=c('code', 'year'),
                          model='random')
cse_infra_controls_re = coef_test(twre_infra_controls, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twre_ls_wo = plm(logistics_services ~ post,
                 data=merged_lpi2,
                 index=c('code', 'year'),
                 model='random')
cse_ls_wo_re = coef_test(twre_ls_wo, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twre_ls_controls = plm(logistics_services ~ post + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
                       data=merged_lpi2,
                       index=c('code', 'year'),
                       model='random')
cse_ls_controls_re = coef_test(twre_ls_controls, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twre_et_wo = plm(ease_tracking ~ post,
                 data=merged_lpi2,
                 index=c('code', 'year'),
                 model='random')
cse_et_wo_re = coef_test(twre_et_wo, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twre_et_controls = plm(ease_tracking ~ post + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
                       data=merged_lpi2,
                       index=c('code', 'year'),
                       model='random')
cse_et_controls_re = coef_test(twre_et_controls, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twre_timeliness_wo = plm(timeliness ~ post,
                         data=merged_lpi2,
                         index=c('code', 'year'),
                         model='random')
cse_timeliness_wo_re = coef_test(twre_timeliness_wo, vcov = "CR1", cluster = "code", test = "naive-t")$SE

twre_timeliness_controls = plm(timeliness ~ post + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
                               data=merged_lpi2,
                               index=c('code', 'year'),
                               model='random')
cse_timeliness_controls_re = coef_test(twre_timeliness_controls, vcov = "CR1", cluster = "code", test = "naive-t")$SE


# coeftest(twfe_plm, vcov = vcovHC, type = "HC1") # Robust standard error
# cov = vcovHC(twfe_plm_controls, type = "HC1")
# robust.se = sqrt(diag(cov)) # robust standard error

library(stargazer)
stargazer(twre_ol_wo, twre_ol_controls, twre_customs_wo, twre_customs_controls, 
          se=list(cse_ol_wo_re, cse_ol_controls_re, cse_customs_wo_re, cse_customs_controls_re), 
          type = 'html',
          out = 'Laporan/Tabel Regresi Random Effect Part 1.html', no.space = TRUE, align = TRUE,
          omit.stat=c("LL","ser","f"))

stargazer(twre_infra_wo, twre_infra_controls, twre_ls_wo, twre_ls_controls,
          se=list(cse_infra_wo_re, cse_infra_controls_re, cse_ls_wo_re, cse_ls_controls_re), 
          type = 'html',
          out = 'Laporan/Tabel Regresi Random Effect Part 2.html', no.space = TRUE, align = TRUE,
          omit.stat=c("LL","ser","f"))

stargazer(twre_et_wo, twre_et_controls, twre_timeliness_wo, twre_timeliness_controls,
          se=list(cse_et_wo_re, cse_et_controls_re, cse_timeliness_wo_re, cse_timeliness_controls_re), 
          type = 'html',
          out = 'Laporan/Tabel Regresi Random Effect Part 3.html', no.space = TRUE, align = TRUE,
          omit.stat=c("LL","ser","f"))


# Bacon-Goodman Decomposition
library(bacondecomp)
library(ggplot2)
library(gridExtra)


# Hanya Infratructure yang ditest karena BRI punya dampak signifikan ke infratrusture
df_bacon_lpi = bacon(
  infra ~ post + log(gdp_per_capita) + import_pct_lcu + export_pct_lcu + corruption_control + manufac_pct_lcu,
  data = data_reg,
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
       title = "Goodman-Bacon decomposition (after Matching) of estimates vs weights") +
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
       title = "Goodman-Bacon decomposition (after Matching)",
       subtitle = "Dotted line shows two-way fixed effect estimate.",
       caption = "Subgroups 99999 is the never treated groups")
