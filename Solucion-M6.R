library(readr)
library(janitor)
library(dplyr)
library(RColorBrewer)
library(glue)
library(ggplot2)
library(ggrepel)
library(ggtext)
library(cowplot)


## 1) xG Timeline con autogol

# leemos el CSV con la info de partidos
info_games_euro = read_csv("statsbomb_info_partidos_euro_2020.csv") %>% clean_names()

# leemos el eventing
events_euro_2020 = read_csv("statsbomb_eventing_data_euro_2020.csv") %>% clean_names() %>% filter(period < 5)

# cantidad de autogoles por partido
table(events_euro_2020$type_name)
names(info_games_euro)

own_goals_games = events_euro_2020 %>%
                  filter(type_name == "Own Goal Against") %>% 
                  group_by(match_id) %>% 
                  summarise(n_own_goals = n()) %>%
                  arrange(desc(n_own_goals)) %>% 
                  left_join(info_games_euro %>% select(match_id, home_team_home_team_name, away_team_away_team_name), 
                            by = "match_id") 

# elegimos match_id = 3788764 Portugal vs Alemania el cual tuvo 2 autogoles
target_game = events_euro_2020 %>%
              # filtramos los tiros no bloqueados y los autogoles del partido objetivo
              filter(match_id == 3788764 & (type_name == "Own Goal Against" | (type_name == "Shot" & shot_outcome_name != "Blocked"))) %>% 
              # join con la info de partidos para tener el nombre de la etapa del torneo y los equipos (home & away)
              left_join(info_games_euro %>% 
                        select(match_id, competition_stage_name, home_team_home_team_name, away_team_away_team_name), by = "match_id") %>% 
              # editamos la info de los autogoles: asignamos xg = 0 (vienen vacíos), el redultado = "Goal" y 
              # al nombre del jugador le agregamos de manera explícita que fue autogol ("\n" sirve para saltarse una línea en el texto)
              mutate(xg = ifelse(type_name == "Own Goal Against", 0, shot_statsbomb_xg),
                     shot_outcome_name = ifelse(type_name == "Own Goal Against", "Goal", shot_outcome_name),
                     player_name = ifelse(type_name == "Own Goal Against", 
                                          paste0(player_name, "\n(", team_name," Own Goal)"), player_name)) %>%
              select(match_id, competition_stage_name, elapsed_time, type_name, team = team_name, 
                     home_team = home_team_home_team_name, away_team = away_team_away_team_name, 
                     player_name, xg, shot_outcome_name) %>% 
              # pasamos el tiempo a minutos y editamos la columna "team" para invertir el equipo en caso de los autogoles
              # (esto último es necesario para que los autogoles sumen como gol del equipo rival)
              mutate(time_min = round(elapsed_time/60, 0),
                     team = case_when(type_name == "Own Goal Against" & team == home_team ~ away_team, 
                                      type_name == "Own Goal Against" & team == away_team ~ home_team,
                                      T ~ team))

teams_game = unique(target_game$team)

# xG acumulado
xg_cum = target_game %>%
        # ordenamos según tiempo para que el xg acumulado se genere correctamente
         arrange(elapsed_time) %>% 
         group_by(team, competition_stage_name) %>% 
         mutate(xg_cum = cumsum(xg),
                goals_cum = cumsum(ifelse(shot_outcome_name == "Goal", 1, 0)))

xg_cum = xg_cum %>%
         # agregamos dos filas (una para acada equipo) con datos time = 0 y xg = 0
         # con esto forzamos que las líneas del gráfico empiecen en 0,0 y no desde el momento del primer tiro
         bind_rows(xg_cum %>% filter(team == teams_game[1] & shot_outcome_name != "Goal") %>% head(1) %>% mutate(time_min = 0, xg_cum = 0)) %>%
         bind_rows(xg_cum %>% filter(team == teams_game[2] & shot_outcome_name != "Goal") %>% head(1) %>% mutate(time_min = 0, xg_cum = 0))

# dataframe solo con los goles (solo en ellos se agregará etiqueta y puntos)
goals = xg_cum %>% filter(shot_outcome_name == "Goal")

# creamos un título personalizado (color y valores de goles y xG específicos para cada equipo)
# en este caso el título cumplirá la función de leyenda de colores también
team1 = xg_cum %>% filter(team == teams_game[1])
team2 = xg_cum %>% filter(team == teams_game[2])

title_text = glue("<b style = 'color:#E41A1C'>{unique(team1$team)} {max(team1$goals_cum)} (xG = {round(max(team1$xg_cum), 3)}) <br> <b style='color:#377EB8'> {unique(team2$team)} {max(team2$goals_cum)} (xG = {round(max(team2$xg_cum), 3)})<br>")

# forzamos que los equipos siempre tengan el mismo orden (para que coincida el color del título con el de las líneas y puntos)
xg_cum$team = factor(xg_cum$team, levels = c(unique(team1$team), unique(team2$team)))

# color de fondo y otros elementos
col_text_and_lines = "grey90"


p1 = ggplot(xg_cum, aes(x = time_min, y = xg_cum, col = team)) +
  # geometría de línea tipo "step" con grosor fijo
  geom_step(size = 1) +
  # puntos exclusivamente para los goles
  geom_point(data = goals, aes(fill = team), size = 3, pch = 21, col = "black") +
  # líneas verticales punteadas para marcar los minutos 45 y 90
  geom_vline(xintercept = 45, linetype = "dotted", size = 1, col = "white") +
  geom_vline(xintercept = 90, linetype = "dotted", size = 1, col = "white") +
  theme_bw() +
  # etiquetas solo par los goles
  geom_label_repel(data = goals, aes(label = player_name), size = 2) +
  # escalas para los ejes (el máximo del eje Y se de define según los valores de xG del partido objetivo)
  scale_y_continuous(breaks = seq(0, max(xg_cum$xg_cum) +0.1, 0.2), 
                     expand = c(0.01, 0.01), limits = c(0, max(xg_cum$xg_cum) +0.1)) +
  scale_x_continuous(breaks = seq(0, 100, 10), expand = c(0.01, 0.01), limits = c(0, 100)) +
  labs(title = title_text,
       caption = "Data: Statsbomb",
       subtitle = paste0(unique(xg_cum$competition_stage_name), " - Euro 2020"),
       x = "\nTime (minutes)", y = "Cummulated xG\n") +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none", 
        plot.margin = margin(1.2, 1, 0.5, 0.5, "cm"),
        # element_markdown() permite leer el título personalizado
        plot.title = element_markdown(size = 18),
        plot.subtitle = element_text(face = "italic"),
        panel.background = element_rect(fill = "#252525", colour = col_text_and_lines),
        plot.background = element_rect(fill = "#252525", colour = "transparent"),
        panel.grid = element_line(colour = "grey30", size = 0.1),
        text = element_text(colour = col_text_and_lines, size = 10),
        axis.text.x = element_text(colour = col_text_and_lines),
        axis.text.y = element_text(colour = col_text_and_lines))

# unimos dos elementos: el gráfico + el logo
p2 = ggdraw() +
     draw_plot(p1) +
     draw_image("logo_euro2020.png",  x = 0.4, y = 0.42, scale = 0.1)


ggsave(paste0("xg_timeline_", teams_game[1], "_vs_", teams_game[2], ".png"), width = 12, height = 8)





## Radar para Heung-Min Son y para el jugador más similar a él
library(tidyr)

## Se utiliza un código similar al del M4

premier = read_csv("instat_players_stats_season_21_22_england_premier_league.csv") %>% janitor::clean_names()
columnas_de_texto = c("player_num", "player_name", "position", "nationality", "team", "national_team", "foot")

premier_clean = premier %>%
                mutate(across(everything(), ~gsub("[%]", "", .x))) %>%
                mutate(across(-c(columnas_de_texto), ~gsub("-", "0", .x))) %>%
                mutate(across(-c(columnas_de_texto), as.numeric)) %>%
                filter(minutes_played >= max(minutes_played)*0.3) %>%
                mutate(label = paste0(player_name, "\n(", team, ")"))

metricas = c("expected_assists", "x_g_expected_goals",
             "attacking_challenges_won", "air_challenges_won", "dribbles_successful",
             "crosses_accurate", "passes", "lost_balls",
             "ball_recoveries_in_opponents_half", "ball_interceptions")

players_p90 = premier_clean %>%
              select(columnas_de_texto, minutes_played, metricas, label) %>% 
              mutate(across(metricas, ~(.x/minutes_played*90), .names = "{.col}_p90"))

players_percentile = players_p90 %>%
                    group_by(position) %>% 
                    mutate(across(ends_with("p90"), ~round(percent_rank(.), 2), .names = "{.col}_percentile")) %>% 
                    ungroup()

# lo nuevo acá es el cambio a formato largo tanto para las métricas p90 como para los percentiles
# esto es necesario para usar el radara de ggplot
metricas_p90 = players_p90 %>% select(ends_with("p90")) %>% names()
players_p90_long_full = players_p90 %>% 
                        pivot_longer(cols = metricas_p90, names_to = "metric", values_to = "p90")

metricas_percentile = players_percentile %>% select(ends_with("percentile")) %>% names()
players_percentile_long_full = players_percentile %>% 
                               pivot_longer(cols = metricas_percentile, names_to = "metric", values_to = "percentile")


data_radar = players_p90_long_full %>%
             # se agregan los valores percentil a los p90
             bind_cols(players_percentile_long_full %>% select(percentile)) %>% 
             # se editan los nombres de las métricas pensando en como estas serán mostradas en el gráfico
              mutate(metric = case_when(metric == "expected_assists_p90" ~ "xA",
                                        metric == "x_g_expected_goals_p90" ~ "xG",
                                        metric == "attacking_challenges_won_p90" ~ "duelos ofensivos\nganados",
                                        metric == "air_challenges_won_p90" ~ "duelos aéreos\nganados",
                                        metric == "dribbles_successful_p90" ~ "regates exitosos",
                                        metric == "crosses_accurate_p90" ~ "centros precisos",
                                        metric == "passes_p90" ~ "pases",
                                        metric == "lost_balls_p90" ~ "balones\nperdidos",
                                        metric == "ball_recoveries_in_opponents_half_p90" ~ "recuperaciones\ncampo rival",
                                        metric == "ball_interceptions_p90" ~ "intercepciones")) %>% 
              select(player_name, team, label, metric, p90, percentile)

# se genera un vector (ang) que estima los ángulos óptimos que tomarán las etiquetas de las métricas según la cantidad de ellas
n_metrics = length(unique(data_radar$metric))
temp <- 360/n_metrics/2                                      
myAng <- seq(-temp, -360 + temp, length.out = n_metrics)     
ang <- ifelse(myAng < -90, myAng+180, myAng)                 
ang <- ifelse(ang < -90, ang+180, ang)

# acá se define el orden que tomarán las métricas
# esto debe ser definido con antelación y mantenerse en el tiempo, permitiendo la comparación de distintos radares
data_radar$metric = factor(data_radar$metric, 
                           levels = c("intercepciones", "recuperaciones\ncampo rival", "balones\nperdidos",
                                       "pases", "centros precisos", "regates exitosos", "duelos aéreos\nganados","xA", 
                                       "duelos ofensivos\nganados","xG"))



# caso 1: "Heung-Min Son"
target = players_percentile %>% filter(player_name == "Heung-Min Son")
pos = target$position
player_radar = data_radar %>% filter(player_name == target$player_name)

ggplot(player_radar, aes(x = metric, y = percentile)) +                      
    geom_bar(aes(y = 1), fill = "#023e8a", stat = "identity", 
             width = 1, colour = "white", alpha = 0.6, linetype = "dashed") +                                                                          
    geom_bar(stat = "identity", width = 1, fill = "#001219", colour = "white", alpha = 0.8) +
    
    geom_hline(yintercept = 0.25, colour = "white", linetype = "longdash", alpha = 0.5)+
    geom_hline(yintercept = 0.50, colour = "white", linetype = "longdash", alpha = 0.5)+
    geom_hline(yintercept = 0.75, colour = "white", linetype = "longdash", alpha = 0.5)+ 
    geom_hline(yintercept = 1,    colour = "white", alpha = 0.5) +
    scale_y_continuous(limits = c(-0.1, 1)) +    
    coord_polar() +                                                                     
    
    geom_label(aes(label = round(p90, 2)), fill = "#e9d8a6", size = 2, color = "black", show.legend = FALSE) +     
    
    labs(fill = "",   
         caption = glue("Percentiles respecto a jugadores de la misma posición con al menos {round(max(premier_clean$minutes_played, na.rm = T)*0.3, 0)} min. jugados\n\nViz: LPDT  |  Data: Instat"),     
         title = glue("{player_radar$player_name[1]} ({player_radar$team[1]})"),
         subtitle = glue("Premier League 21/22 | Estadísticas cada 90 min.")) +
    theme_minimal() +                                                                     
    theme(plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_rect(fill = "white", color = "white"),
          legend.position = "top",
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 12, angle = ang),
          plot.title = element_markdown(hjust = 0.5, size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          plot.caption = element_text(size = 10),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.margin = margin(5, 2, 2, 2)) 

ggsave(paste0("radar_", target$player_name,".png"), height = 7, width = 7)


# caso 2: el más similar a Son

son_similarity = read_csv("top_5_jugadores_similares_son_premier_21_22_instat.csv")
 
target = players_percentile %>% filter(player_name == son_similarity$player_name[2])
pos = target$position
player_radar = data_radar %>% filter(player_name == target$player_name)

ggplot(player_radar, aes(x = metric, y = percentile)) +                      
  geom_bar(aes(y = 1), fill = "#023e8a", stat = "identity", 
           width = 1, colour = "white", alpha = 0.6, linetype = "dashed") +                                                                          
  geom_bar(stat = "identity", width = 1, fill = "#001219", colour = "white", alpha = 0.8) +
  
  geom_hline(yintercept = 0.25, colour = "white", linetype = "longdash", alpha = 0.5)+
  geom_hline(yintercept = 0.50, colour = "white", linetype = "longdash", alpha = 0.5)+
  geom_hline(yintercept = 0.75, colour = "white", linetype = "longdash", alpha = 0.5)+ 
  geom_hline(yintercept = 1,    colour = "white", alpha = 0.5) +
  scale_y_continuous(limits = c(-0.1, 1)) +    
  coord_polar() +                                                                     
  
  geom_label(aes(label = round(p90, 2)), fill = "#e9d8a6", size = 2, color = "black", show.legend = FALSE) +     
  
  labs(fill = "",   
       caption = glue("Percentiles respecto a jugadores de la misma posición con al menos {round(max(premier_clean$minutes_played, na.rm = T)*0.3, 0)} min. jugados\n\nViz: LPDT  |  Data: Instat"),     
       title = glue("{player_radar$player_name[1]} ({player_radar$team[1]})"),
       subtitle = glue("Premier League 21/22 | Estadísticas cada 90 min.")) +
  theme_minimal() +                                                                     
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, angle = ang),
        plot.title = element_markdown(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(size = 10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(5, 2, 2, 2)) 

ggsave(paste0("radar_", target$player_name,".png"), height = 7, width = 7)  

