##### M4

library(readr)
library(readxl)
library(dplyr)
library(janitor)
library(stringr)
euro_2020_events = read_csv("data/statsbomb_eventing_data_euro_2020.csv")
euro_2020_games = read_csv("data/statsbomb_info_partidos_euro_2020.csv")


#### Unión (join) de tablas

## Es importante tener al menos una llave (columna) común entre las tablas a unir
# algunas funciones del paquete {dplyr} son: left_join(), inner_join(), full_join(), etc. 
names(euro_2020_events)
names(euro_2020_games)

euro_2020_events_with_date = left_join(euro_2020_events, euro_2020_games, by = "match_id")

euro_2020_events_with_date = left_join(euro_2020_events, 
                                       euro_2020_games %>% select(match_id, competition_stage.name), 
                                       by = "match_id")
# nomenclatura recomendada
euro_2020_events_with_date = euro_2020_events %>% 
                             left_join(euro_2020_games %>% select(match_id, competition_stage.name), 
                                       by = "match_id")


## ejemplo de p90 ahora a nivel de equipos de la Euro 2020
  
minutos_jugados_teams <- euro_2020_events %>%
  group_by(team.name, match_id) %>%
  summarise(minutos_jugados = max(ElapsedTime)/60) %>%
  group_by(team.name) %>%
  summarise(n_games = n(),
            minutos_totales = sum(minutos_jugados)) %>% 
  arrange(desc(minutos_totales))

  
table(euro_2020_events$type.name)
pressure_p90 = euro_2020_events %>%
               filter(type.name == "Pressure") %>%   
               group_by(team.name) %>% 
               summarise(n_pressure = n()) %>% 
               arrange(desc(n_pressure)) %>% 
               left_join(minutos_jugados_teams, by = "team.name") %>% 
               mutate(pressure_p90 = n_pressure/minutos_totales*90) %>% 
               arrange(desc(pressure_p90))

 

#### across() + cálculo de percentiles

# se usa en conjunto con mutate() o summarise() para indicar que la función objetivo se aplique "a lo largo de" múltiples columnas que cumplen una condición específica

## para quitar el símbolo % de las columnas que lo tienen 
premier_per = read_xlsx("data/players_stats_season_21_22_england_premier_league.xlsx", col_types = "text") %>%
  clean_names() %>%
  mutate(across(c(ends_with("percent"), "chances_percent_of_conversion"), ~as.numeric(str_replace(.x, "%", ""))))

## para cambiar todos los símbolos "-" de las variables que lo tengan y pasarlo a 0
names(premier_per)
columnas_de_texto = c("player_num", "player_name", "position", "nationality", "team", "national_team", 
                      "foot", "national_team_last_match_date_mm_yy", "youth_national_team_last_match_date_mm_yy")

premier_numeric = read_xlsx("data/players_stats_season_21_22_england_premier_league.xlsx", col_types = "text") %>%
  clean_names() %>%
  mutate(across(c(ends_with("percent"), "chances_percent_of_conversion"), ~as.numeric(str_replace(.x, "%", "")))) %>%
  mutate(across(-columnas_de_texto, ~as.numeric(str_replace(.x, "-", "0"))))

##--------------------------- OPCION PARA OBTENER LOS DATOS P90 A TODAS LAS COLUMNAS NECESARIAS (NUMERICAS) -----------------------------

## convertir todas las stats numéricas a p90
otras_columnas_a_remover = c("minutes_played", "age", "weight", "height", "matches_played", "in_stat_index",
                             "starting_lineup_appearances", "substitute_out", "substitutes_in")

premier_p90 = read_xlsx("data/players_stats_season_21_22_england_premier_league.xlsx", col_types = "text") %>%
  clean_names() %>%
  mutate(across(c(ends_with("percent"), "chances_percent_of_conversion"), ~as.numeric(str_replace(.x, "%", "")))) %>%
  mutate(across(-columnas_de_texto, ~as.numeric(str_replace(.x, "-", "0")))) %>% 
  mutate(across(-c(columnas_de_texto, otras_columnas_a_remover), ~(.x/minutes_played*90), .names = "{.col}_p90"))

check_p90 = premier_p90 %>% 
            select(player_name, minutes_played, goals, goals_p90)       


## convertir todas las variables numéricas a percentil
umbral_minimo_MJ = max(premier_p90$minutes_played)*0.3 #umbral minimo de minutos jugador (RECOMENDADO)
  
premier_p90_percentil = premier_p90 %>% 
  filter(minutes_played >= umbral_minimo_MJ) %>% 
  mutate(across(ends_with("_p90"), ~round(percent_rank(.x)*100, 1), .names = "{.col}_percentil"))

check_p90_percentil = premier_p90_percentil %>% 
  select(player_name, team, minutes_played, goals, goals_p90, goals_p90_percentil) %>% 
  arrange(desc(goals_p90_percentil))


## pero los percentiles deberían calcularse de manera agrupada por otras variables como por ejemplo Posición!
# group_by() también es útil al usar mutate(), no solo con summarise()
# aqui el percentil se calcula tambien tomando en cuenta las posiciones de los jugadores
premier_p90_percentil = premier_p90 %>% 
  filter(minutes_played >= umbral_minimo_MJ) %>%
  group_by(position) %>% 
  mutate(across(ends_with("p90"), ~round(percent_rank(.x)*100, 1), .names = "{.col}_percentil"))

check_p90_percentil = premier_p90_percentil %>% 
  select(player_name, team, minutes_played, goals, goals_p90, goals_p90_percentil) %>% 
  arrange(desc(goals_p90_percentil))



#### ejemplo de similitud de jugadores aplicando los criterios hasta ahora aprendidos

## jugadores más similares a Salah
target = premier_p90_percentil %>% filter(player_name == "Mohamed Salah")
pos = target$position

names(target)
metricas = c("expected_assists_p90_percentil", "goals_p90_percentil", "x_g_expected_goals_p90_percentil",
             "attacking_challenges_won_p90_percentil", "air_challenges_won_p90_percentil", "dribbles_successful_p90_percentil",
             "chances_successful_p90_percentil", "crosses_accurate_p90_percentil", "passes_p90_percentil",
             "ball_recoveries_in_opponents_half_p90_percentil", "ball_interceptions_p90_percentil")

# percentiles de valores p90 [OK]
# posición [OK]
# metricas [OK]

data = premier_p90_percentil %>% 
       filter(position == pos) %>%
       ungroup() %>% 
       select(player_name, metricas)

library(proxy)
sim = simil(x = data %>% select(-player_name), 
            y = target %>% ungroup() %>% select(metricas), 
            method = "cosine")

output = data %>% 
         mutate(sim_percentile_cosine = as.numeric(sim)) %>% 
         arrange(desc(sim_percentile_cosine)) %>% 
         mutate(player_name_ = player_name)




#### Corrección por posesión

## calcular stats de posesión por partido
passes = euro_2020_events %>% 
         filter(type.name == "Pass")

pass_stats = passes %>% 
             group_by(team.name, match_id) %>% 
             summarise(n_pases = n(),
                       n_pases_correctos = sum(ifelse(is.na(pass.outcome.name), 1, 0)),
                       median_pass_length = median(pass.length)) %>% 
             mutate(precision_pases = n_pases_correctos/n_pases*100)

poss_data <- passes %>%
             mutate(TimeInPoss = ifelse(TimeInPoss > 200, 200, TimeInPoss)) %>% 
             group_by(team = possession_team.name, possession, match_id) %>%
             summarise(n_pass = n(),
                       poss_time = max(TimeInPoss)) %>% 

quantile(passes$TimeInPoss, seq(0, 1, 0.001))

poss_stats <- poss_data %>%
              group_by(team, match_id) %>%
              summarise(n_poss = n(),
                        total_pass = sum(n_pass),
                        total_poss_time_min = sum(poss_time)/60)

poss_stats_full = poss_stats %>% 
                  left_join(poss_stats %>% 
                              select(match_id, opp_team = team, opp_total_poss_time_min = total_poss_time_min), 
                            by = c("match_id")) %>% 
                  filter(team != opp_team) %>% 
                  left_join(euro_2020_games %>% select(match_id, competition_stage.name), by = "match_id") %>% 
                  mutate(tiempo_efectivo_juego = total_poss_time_min + opp_total_poss_time_min,
                         poss_percent = total_poss_time_min/tiempo_efectivo_juego*100)

check_percent_time = poss_stats_full %>% 
                     group_by(match_id) %>% 
                     summarise(total_per = sum(poss_percent))



## ajustar una métrica por posesión

# minutos jugados a nivel de jugadores
library(StatsBombR)
competitions <- FreeCompetitions()
partidos = FreeMatches(competitions %>%
                         filter(competition_name == "UEFA Euro"))

eventos <- StatsBombFreeEvents(MatchesDF = partidos)
eventos_clean = allclean(eventos)
names(eventos_clean)

min_jugados = get.minutesplayed(eventos_clean)


# recuperaciones (métrica defensiva) ajustadas por posesion a nivel de partidos
table(eventos_clean$type.name)
recuperaciones = eventos_clean %>% 
               filter(type.name == "Ball Recovery") %>% 
               group_by(match_id, player.name, player.id, team.name) %>% 
               summarise(n_recoveries = n()) %>%
               left_join(min_jugados %>% select(match_id, player.id, MinutesPlayed), 
                         by = c("match_id", "player.id")) %>% 
               mutate(n_recoveries_p90 = n_recoveries/MinutesPlayed*90) %>% 
               left_join(poss_stats_full %>% 
                           select(match_id, team, opp_total_poss_time_min, poss_percent), 
                         by = c("match_id", "team.name" = "team")) %>% 
               mutate(min_poss = MinutesPlayed*poss_percent/100,
                      opp_min_poss = MinutesPlayed - min_poss,
                      n_recoveries_adj = n_recoveries/opp_min_poss*45) %>% 
               relocate(n_recoveries_p90, .before = n_recoveries_adj)
