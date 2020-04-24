source('gather.R')

hs <- schools_ec %>%
  filter(type == 'Stredná odborná škola' | type == 'Gymnázium')

pr <- schools_ec %>%
  filter(type == 'Základná škola')

regional_pr <- pr %>%
  group_by(region, county) %>%
  summarise(
    overall = mean(overall_rating, na.rm = T),
    testovanie9 = mean(testovanie9, na.rm = T),
    t9_sj = mean(t9_sj, na.rm = T),
    t9_m = mean(t9_m, na.rm = T),
    t9_mj = mean(t9_mj, na.rm = T),
    t9_s_ja_sl = mean(t9_s_ja_sl, na.rm = T),
    pop = mean(pop_total),
    dens = mean(pop_density),
    income = mean(avg_wage),
    unempl = mean(unemployment_rate)
  )

regional_hs <- hs %>%
  group_by(region, county) %>%
  summarise(
    overall = mean(overall_rating, na.rm = T),
    maturity = mean(maturity, na.rm = T),
    mat_sj = mean(mat_sj, na.rm = T),
    mat_m = mean(mat_m, na.rm = T),
    mat_mj = mean(mat_mj, na.rm = T),
    mat_s_ja_sl = mean(mat_s_ja_sl, na.rm = T),
    mat_ajb1 = mean(mat_ajb1, na.rm = T),
    mat_ajb2 = mean(mat_ajb2, na.rm = T),
    mat_ajc1 = mean(mat_ajc1, na.rm = T),
    pop = mean(pop_total),
    dens = mean(pop_density),
    income = mean(avg_wage),
    unempl = mean(unemployment_rate),
  )