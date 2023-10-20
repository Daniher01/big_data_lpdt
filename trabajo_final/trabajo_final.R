library(readxl)
library(janitor)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(proxy)
library(ggplot2)
library(glue)
library(ggtext)

premier = read_xlsx("data/players_stats_season_21_22_england_premier_league.xlsx", col_types = "text") %>% janitor::clean_names()

premier$minutes_played = (as.numeric(premier$minutes_played))

umbral_minimo_MJ = max(premier$minutes_played)* 0.25

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
            mutate(across(ends_with("p90"), ~round(percent_rank(.x),2), .names = "{col}_percentil"))
          

# ------------------------------ FILTRAR JUGADORES SEGUN NECESIDADES DEL CLUB

dt_player = premier_p90_precentil %>% filter(player_name == player)

players_filtrados = premier_p90_precentil %>% 
            filter(as.numeric(age) >= 23 & as.numeric(age) <= 28,
                   minutes_played >= as.numeric(umbral_minimo_MJ),
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

#-------------------------------- PREPARACION PARA EL GRAFICO

players_selected = c('Richarlison', 'E. Dennis', 'Neal Maupay', 'Patson Daka')

# metricas p90

players_selected_p90 = premier_p90 %>% filter(player_name %in% players_selected)

metricas_p90 = players_selected_p90 %>% select(ends_with("p90")) %>% names()

players_selected_p90_long = players_selected_p90 %>% 
  pivot_longer(cols = metricas_p90, names_to = "metric", values_to = "p90")

# metricas percentiles
players_selected_p90_percentil = premier_p90_precentil %>% ungroup() %>% filter(player_name %in% players_selected)

metricas_percentile = players_selected_p90_percentil %>% select(ends_with("percentil")) %>% names()

players_selected_p90_percentil_long = players_selected_p90_percentil %>% 
  pivot_longer(cols = metricas_percentile, names_to = "metric", values_to = "percentil")


df_selected = players_selected_p90_long %>%
  bind_cols(players_selected_p90_percentil_long %>% select(percentil)) %>% #une solo la colmuna "percentile"W
  mutate(metric = case_when(metric == "x_g_expected_goals_p90" ~ "xG",
                            metric == "expected_assists_p90" ~ "xA",
                            metric == "air_challenges_won_p90" ~ "duelos aéreos\nganados",
                            metric == "attacking_challenges_won_p90" ~ "duelos ofensivos\nganados",
                            metric == "dribbles_successful_p90" ~ "regates exitosos",
                            metric == "goals_p90" ~ "goles",
                            metric == "chances_successful_p90" ~ "ocasiones exitosas",
                            metric == "crosses_accurate_p90" ~ "centros precisos",
                            metric == "passes_p90" ~ "pases",
                            metric == "ball_recoveries_in_opponents_half_p90" ~ "recuperaciones\ncampo rival",
                            metric == "ball_interceptions_p90" ~ "intercepciones")) %>% 
  select(player_name, team, metric, p90, percentil)

df_selected = na.omit(df_selected)

df_selected$metric = factor(df_selected$metric, 
                            levels = c("intercepciones", "recuperaciones\ncampo rival", "duelos aéreos\nganados", 
                                       "pases", "centros precisos", "xA", 
                                       "duelos ofensivos\nganados", "ocasiones exitosas", "regates exitosos", 
                                       "xG", "goles"))

# TODO
target = df_selected %>% filter(player_name == players_selected[1])
color_target = "#023e8a"
player_1 = df_selected %>% filter(player_name == players_selected[2])
color_player_1 = "#d95f0e"
player_2 = df_selected %>% filter(player_name == players_selected[3])
color_player_2 = "#31a354"
player_3 = df_selected %>% filter(player_name == players_selected[4])
color_player_3 = "#8856a7"


generar_grafico <- function(df = df_selected, player, color_player ){

  ggplot(df, aes(x = metric, y = percentil)) +
    #geom_bar(data = player, fill = color_player, stat = "identity", width = 1,  alpha = 0.3) +
    
    geom_bar(aes(y = 1), fill = color_player, stat = "identity", 
             width = 1, colour = "white", alpha = 0.3, linetype = "dashed") +                                                                          
    geom_bar(data = player, fill = color_player, stat = "identity", width = 1,  alpha = 0.8) +
    
    geom_hline(yintercept = 0.25, colour = "white", linetype = "longdash", alpha = 0.5) +
    geom_hline(yintercept = 0.50, colour = "white", linetype = "longdash", alpha = 0.5) +
    geom_hline(yintercept = 0.75, colour = "white", linetype = "longdash", alpha = 0.5) + 
    geom_hline(yintercept = 1,    colour = "white", alpha = 0.5) +
    scale_y_continuous(limits = c(-0.1, 1)) +    
    coord_polar() +
    
    geom_label(data = player, aes(label = round(p90, 2)), fill = "#e9d8a6", size = 2,color= "black", show.legend = FALSE) +
    
    labs(fill = "",   
         caption = glue("Percentiles respecto a jugadores de la misma posición con al menos {round(max(premier_clean$minutes_played, na.rm = T)*0.3, 0)} min. jugados\n\nViz: Daniel Hernandez & LPDT  |  Data: Instat"),     
         title = glue("{player$player_name[1]} ({player$team[1]})"),
         subtitle = glue("Premier League 21/22 | Estadísticas cada 90 min.")) +
    theme_minimal() +                                                                     
    theme(plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_rect(fill = "white", color = "white"),
          legend.position = "top",
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 12), # las etiquetas del eje X van a tener un angulo (REVISAR)
          plot.title = element_markdown(hjust = 0.5, size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          plot.caption = element_text(size = 10),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.margin = margin(5, 2, 2, 2))
}

generar_grafico(player = target, color_player = color_target)
generar_grafico(player = player_1, color_player = color_player_1)
generar_grafico(player = player_2, color_player = color_player_2)
generar_grafico(player = player_3, color_player = color_player_3)

