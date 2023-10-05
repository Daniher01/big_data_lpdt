premier = read_xlsx("data/players_stats_season_21_22_england_premier_league.xlsx", col_types = "text") %>% clean_names() %>%
  mutate(across(c(ends_with("percent"), "chances_percent_of_conversion"), ~as.numeric(str_replace(.x, "%", ""))))

# ----- EJERCICIO 01 ---------

premier$shots = as.numeric(premier$shots)

top20_shot_on_target_permier = premier %>%
          select(player_name, team, shots, shots_on_target_percent) %>%
          mutate(across('shots_on_target_percent', ~round(percent_rank(.x)*100, 1), .names = "{.col}_percentil")) %>%
          filter(shots > 30) %>%
          arrange(desc(shots_on_target_percent_percentil)) %>%
          head(20)

quantile(premier$shots, seq(0, 1, 0.1), na.rm = TRUE)
write_csv(top20_shot_on_target_permier, "ejercicio_M04/top20_shot_on_target_permier.csv")

# ---------------- EJERCICIO 2 ---------------------------------------------
premier_metricas = read_xlsx("data/players_stats_season_21_22_england_premier_league.xlsx", col_types = "text") %>% clean_names() %>%
  mutate(across(metricas, ~replace_na(as.numeric(str_replace(.x, "NA", "0")), 0)))
  


target = premier_metricas %>% filter(player_name == "Heung-Min Son")
metricas = c("expected_assists", "x_g_expected_goals", "attacking_challenges", "air_challenges", "dribbles", "crosses", 
             "passes", "lost_balls", "ball_interceptions", "ball_recoveries")

sim = simil(x = premier_metricas %>% select(metricas),
          y = target %>% select(metricas),
          method = "cosine")

jugadores_houng_min_son = premier_metricas %>% 
  mutate(sim_percentile_cosine = as.numeric(sim)) %>% 
  select(c(player_name, metricas, sim_percentile_cosine)) %>%
  arrange(desc(sim_percentile_cosine)) %>% 
  mutate(player_name_ = player_name) %>%
  head(5)

write_csv(jugadores_houng_min_son, "ejercicio_M04/jugadores_houng_min_son.csv")

# -----------------------------

            
            
            
