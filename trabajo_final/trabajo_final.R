premier = read_xlsx("data/players_stats_season_21_22_england_premier_league.xlsx", col_types = "text") %>% janitor::clean_names()

umbral_minimo_MJ = max(as.numeric(premier$minutes_played))* 0.25

player = "Richarlison"

columnas_de_texto = c("player_num", "player_name", "position", "nationality", "team", "national_team", 
                      "foot", "national_team_last_match_date_mm_yy", "youth_national_team_last_match_date_mm_yy")

otras_columnas_a_remover = c("minutes_played", "age", "weight", "height", "matches_played", "in_stat_index",
                             "starting_lineup_appearances", "substitute_out", "substitutes_in")

premier_clean =  premier %>%
  mutate(across(c(ends_with("percent"), "chances_percent_of_conversion"), ~as.numeric(str_replace(.x, "%", "")))) %>%
  mutate(across(-columnas_de_texto, ~as.numeric(str_replace(.x, "-", "0"))))

# -------------------------------- OBTENER DATOS P90 Y PERCENTILES DE JUGADORES DE LA PREMIER 

premier_p90 = premier_clean %>%
              mutate(across(-c(columnas_de_texto, otras_columnas_a_remover), ~(.x/minutes_played*90), .names = "{.col}_p90"))

premier_p90_precentil = premier_p90 %>%
            filter(minutes_played >= umbral_minimo_MJ) %>%
            group_by(position) %>%
            mutate(across(ends_with("p90"), ~round(percent_rank(.x)*100,1), .names = "{col}_percentil"))
          

# ------------------------------ FILTRAR JUGADORES SEGUN NECESIDADES DEL CLUB

dt_player = premier_p90_precentil %>% filter(player_name == player)

players_filtrados = premier_p90_precentil %>% 
            filter(as.numeric(age) >= 23 & as.numeric(age) <= 28,
                   as.numeric(minutes_played) >= as.numeric(umbral_minimo_MJ),
                   position == "F",
                   player_name != player)

players_filtrados = players_filtrados %>% mutate(valor_millones_euros = case_when(player_name == "Gabriel Jesus" ~ 75,
                                                              player_name == "Harry Kane" ~ 90,
                                                              player_name == "Ivan Toney" ~ 35,
                                                              player_name == "E. Dennis"~ 7,
                                                              player_name == "Timo Werner"~ 25,
                                                              player_name == "Ollie Watkins" ~ 55,
                                                              player_name == "Odsonne Edouard" ~ 20,
                                                              player_name == "K. Iheanacho" ~ 18,
                                                              player_name == "Che Adams" ~ 20,
                                                              player_name == "Neal Maupay" ~ 10,
                                                              player_name == "Patson Daka" ~ 18,
                                                              player_name == "Jean-Philippe Mateta" ~ 7,
                                                              player_name == "Tyler Roberts" ~ 3,
                                                              player_name == "Adam Armstrong" ~ 10))

players_filtrados = players_filtrados %>% filter(valor_millones_euros < 20)

players_filtrados = rbind(players_filtrados, dt_player)


# ------------------------------ SIMILITUD DE JUGADORES

metricas = c("expected_assists_p90_percentil", "goals_p90_percentil", "x_g_expected_goals_p90_percentil",
             "attacking_challenges_won_p90_percentil", "air_challenges_won_p90_percentil", "dribbles_successful_p90_percentil",
             "chances_successful_p90_percentil", "crosses_accurate_p90_percentil", "passes_p90_percentil",
             "ball_recoveries_in_opponents_half_p90_percentil", "ball_interceptions_p90_percentil")

target = premier_p90_precentil %>% filter(player_name == player)

data = players_filtrados %>%
        ungroup() %>%
        select(player_name, position,valor_millones_euros, minutes_played, age, metricas)
  
sim = simil(x = data %>% select(-c(player_name, position,minutes_played, valor_millones_euros, age)),
            y = target %>% ungroup() %>% select(metricas),
            method = "cosine")

output = data %>% 
  mutate(sim_percentile_cosine = as.numeric(sim)) %>% 
  arrange(desc(sim_percentile_cosine)) %>% 
  mutate(player_name_ = player_name)


# ------------------------------



