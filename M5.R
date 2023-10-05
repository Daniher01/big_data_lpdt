##### M5

library(readxl)
library(readr)
library(janitor)
library(dplyr)

library(ggplot2)
library(forcats)
library(ggrepel)

player_stats_pipe = read_xlsx("data/players_stats_season_21_22_england_premier_league.xlsx", col_types = "text") %>%
  clean_names() %>% 
  #rename("player_num" = "x1", "player_name" = "x2") %>% 
  select(-c(player_num, position, nationality, height, weight, foot)) %>% 
  mutate(minutes_played = as.numeric(minutes_played),
         goals = as.numeric(goals)) %>% 
  arrange(desc(goals))

goles_p90 = player_stats_pipe %>% 
  select(player_name, team, minutes_played, goals) %>% 
  mutate(goals_p90 = goals/minutes_played*90) %>% 
  arrange(desc(goals_p90))

umbral_minimo_MJ = max(player_stats_pipe$minutes_played)*0.3
goles_p90_filtrado = goles_p90 %>% 
                     filter(minutes_played >= umbral_minimo_MJ)

### Barras

## Tradicional
g1 = ggplot(data = goles_p90_filtrado %>% head(10), 
            aes(x = player_name, y = goals_p90)) +
     geom_bar(stat = "identity")
g1

## Horizontal
g2 = g1 + coord_flip()
g2

# limpia el fondo
g3 = g2 + theme_bw()
g3

# ordena de mayor a menor
top10_goles_p90 = goles_p90_filtrado %>% 
                  head(10) %>% 
                  mutate(player_name = as.factor(player_name))


g4 = ggplot(top10_goles_p90, 
             aes(x = fct_reorder(player_name, goals_p90), y = goals_p90)) +
             geom_bar(stat = "identity") +
             coord_flip() +
             theme_bw()
g4

# títulos
g5 = g4 +
     labs(x = "Player name\n",
          y = "\nGoals p90",
          title = "Top 10 players based on Goals p90",
          subtitle = paste0("Premier League 21/22. Players who played at least ", round(umbral_minimo_MJ, 0), " minutes."))
g5


# configurar eje numérico
g6 = g5 + scale_y_continuous(breaks = seq(0, 0.8, 0.1), labels = seq(0, 0.8, 0.1), limits = c(0, 0.8))
g6


# color de relleno y borde de barras
base = ggplot(goles_p90_filtrado %>% head(10), aes(x = fct_reorder(player_name, goals_p90), y = goals_p90)) +
     scale_y_continuous(breaks = seq(0, 0.8, 0.1), labels = seq(0, 0.8, 0.1), limits = c(0, 0.8)) +
     coord_flip() +
     theme_bw() +
     labs(x = "Player name\n",
          y = "\nGoals p90",
          title = "Top 10 players based on Goals p90",
          subtitle = paste0("Premier League 21/22. Players who played at least ", round(umbral_minimo_MJ, 0), " minutes"))

base + geom_bar(stat = "identity", fill = "#457b9d", col = "black")
  

# transparencia
g7 = base + geom_bar(stat = "identity", fill = "#457b9d", col = "black", alpha = 0.7)
g7


# label o texto 
g8 = g7 + geom_label(aes(label = round(goals_p90, 3)))
g8


### Exportar gráfico
ggsave("data/barras_top10_goals_p90_PL2122.png")
ggsave("data/barras_top10_goals_p90_PL2122.png", width = 12, height = 8)



### Histogram + Density
ggplot(player_stats_pipe, aes(x = age)) + geom_histogram(stat = "count")

euro_2020_events = read_csv("data/statsbomb_eventing_data_euro_2020.csv")
ggplot(euro_2020_events, aes(x = shot.statsbomb_xg)) + geom_density()
ggplot(euro_2020_events, aes(x = location.x)) + geom_density()
ggplot(euro_2020_events, aes(x = location.y)) + geom_density()



### Scatterplot

# datos: xG a favor y en contra p90 de cada equipo por partido de la Euro 2020
euro_2020_games = read_csv("data/statsbomb_info_partidos_euro_2020.csv")
xg_favor = euro_2020_events %>% 
           group_by(team.name, match_id) %>% 
           summarise(minutes_played = max(ElapsedTime)/60,
                     xGF = sum(shot.statsbomb_xg, na.rm = T))

xg_total = euro_2020_events %>% 
  group_by(match_id) %>% 
  summarise(xGT = sum(shot.statsbomb_xg, na.rm = T))

xg_stats = xg_favor %>% 
           left_join(xg_total, by = "match_id") %>% 
           mutate(xGA = xGT - xGF,
                  xGF_p90 = xGF/minutes_played*90,
                  xGA_p90 = xGA/minutes_played*90,
                  xG_Net_p90 = xGF_p90 - xGA_p90) %>% 
           left_join(euro_2020_games %>% select(match_id, competition_stage.name), by = "match_id")

# normal
ggplot(xg_stats, aes(x = xGF_p90, y = xGA_p90)) +
  geom_point()

# con las mejoras que ya conocemos y algunos atributos de geom_point()
base2 = ggplot(xg_stats, aes(x = xGF_p90, y = xGA_p90)) +
        theme_bw() +
        labs(title = "xGF vs xGA p90 per team and game", subtitle = "Euro 2020", x = "\nxGF p90", y = "xGA p90\n",
             caption = "Data: Statsbomb") +
        geom_abline(slope = 1)

base2 + geom_point(size = 3, shape = 21, fill = "darkblue", alpha = 0.4)

# mapear una variable a un aes de ggplot2
base2 + geom_point(aes(fill = competition_stage.name), size = 3, shape = 21, alpha = 0.7)


# cambiar escala de colores y título de leyenda
base2 +
  geom_point(aes(fill = competition_stage.name), size = 3, shape = 21, alpha = 0.7) +
  scale_fill_brewer(palette = "Set1") +
  #scale_fill_manual(values = c("#9b5de5", "#f15bb5", "#fee440", "#00bbf9", "#00f5d4")) +
  labs(fill = "Stage Name")


# mapear una variable numérica con el color del punto y la etapa del torneo con la forma
base2 +
  geom_point(aes(shape = competition_stage.name, fill = xG_Net_p90), size = 3, alpha = 0.7) +
  scale_fill_distiller(palette = "Spectral", direction = 1) +
  scale_shape_manual(values= c(21:24, 25)) +
  labs(fill = "xG Net p90", shape = "Stage Name")


# mapear una variable numérica con el tamaño del punto y la etapa del torneo con el color de relleno
p1 = base2 +
     geom_point(aes(fill = competition_stage.name, size = xG_Net_p90), shape = 21, alpha = 0.7) +
     scale_fill_brewer(palette = "Set1") +
     #scale_size_continuous()
     labs(size = "xG Net p90", fill = "Stage Name")
p1


# agregar texto en cada punto
p1 + geom_text(aes(label = team.name))


# ggrepel
p1 + geom_text_repel(aes(label = team.name))


# facetas
p1 + facet_wrap(~team.name)

ggsave("scatter_xgF_xgA_p90_per_team_Euro2020.png", width = 12, height = 12)
