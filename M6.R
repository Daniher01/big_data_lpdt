##### M6

library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(readxl)
library(tidyr)

### Líneas

## xG Timeline- 
euro_2020_events = read_csv("data/statsbomb_eventing_data_euro_2020.csv")
euro_2020_games = read_csv("data/statsbomb_info_partidos_euro_2020.csv")

target_game = euro_2020_events %>% 
              filter(match_id == "3794685" & type.name == "Shot" & shot.outcome.name != "Blocked") %>% 
              select(match_id, ElapsedTime, type.name, Team = team.name, player.name, xG = shot.statsbomb_xg, shot.outcome.name) %>% 
              mutate(time_min = round(ElapsedTime/60, 0)) 

# primer intento
ggplot(target_game, aes(x = time_min, y = xG, colour = Team)) +
  geom_line()

# xG acumulado!
xg_cum = target_game %>%
         arrange(ElapsedTime) %>% 
         group_by(Team) %>% 
         mutate(xG_cum = cumsum(xG),
                Goals_cum = cumsum(ifelse(shot.outcome.name == "Goal", 1, 0)))

xg_cum = xg_cum %>% 
         bind_rows(xg_cum %>% filter(Team == "Austria") %>% head(1) %>% mutate(time_min = 0, xG_cum = 0)) %>%
         bind_rows(xg_cum %>% filter(Team == "Italy") %>% head(1) %>% mutate(time_min = 0, xG_cum = 0))

goals = xg_cum %>% filter(shot.outcome.name == "Goal")

p1 = ggplot(xg_cum, aes(x = time_min, y = xG_cum, col = Team)) +
  geom_step(size = 1) +
  geom_point(data = goals, aes(fill = Team), size = 3, pch = 21, col = "black") +
  theme_bw() +
  geom_vline(xintercept = 45, linetype = "dotted", size = 1) +
  geom_vline(xintercept = 90, linetype = "dotted", size = 1) +
  geom_vline(xintercept = 105, linetype = "dotted", size = 1) +
  geom_label_repel(data = goals, aes(label = player.name), size = 3) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), expand = c(0.01, 0.01), limits = c(0, 1.4)) +
  scale_x_continuous(breaks = seq(0, 125, 15), expand = c(0.01, 0.01), limits = c(0, 125)) +
  labs(x = "\nTime (minutes)", y = "Cummulated xG\n") +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

p1


## otras mejoras

# ubicación de leyenda
p1 +
  theme(legend.position = "none") #c(0.15, 0.85)) # "none"
    
# título personalizado
library(glue) #PERMITE AGREGAR CODIGO HTML
library(ggtext)

library(RColorBrewer)
brewer.pal(2, "Set1") # obtener los codigos de 2 colores de esa paleta

austria = xg_cum %>% filter(Team == 'Austria')
italy = xg_cum %>% filter(Team == 'Italy')
title_text = glue("<b style = 'color: #E41A1C'>Austria {max(austria$Goals_cum)} (xG = {round(max(austria$xG_cum), 3)}) <br> <b style='color: #377EB8'> Italy {max(italy$Goals_cum)} (xG = {round(max(italy$xG_cum), 3)})<br>")

p2 = p1 +        
  labs(title = title_text,
       caption = "Data: Statsbomb",
       subtitle = "Round of 16 - Euro 2020") +
  theme(plot.title = element_markdown(size = 18), # permite reconocer el texto HTML
        plot.subtitle = element_text(face = "italic"),
        legend.position = "none")

p2

# color de fondo y otros elementos
col_text_and_lines = "grey80"

p3 = p2 +
  theme(panel.background = element_rect(fill = "#252525", colour = col_text_and_lines),
        plot.background = element_rect(fill = "#252525", colour = "transparent"),
        panel.grid = element_line(colour = "grey50", size = 0.1),
        text = element_text(colour = col_text_and_lines, size = 10),
        axis.text.x = element_text(colour = col_text_and_lines),
        axis.text.y = element_text(colour = col_text_and_lines))
p3

ggsave("xg_timeline.png", width = 12, height = 8)


# márgenes
p4 = p3 + theme(plot.margin = margin(1.2, 1, 0.5, 0.5, "cm"))
p4

# agregar logo
library(cowplot)
p5 <- ggdraw() +
      draw_plot(p4) +
      draw_image("logo_euro2020.png",  x = 0.4, y = 0.42, scale = 0.1) # de 0 (el centro) al 5
p5

ggsave("xg_timeline.png", width = 12, height = 8)



### Boxplot

# normal
np_unblocked_shots = euro_2020_events %>% 
                     filter(type.name == "Shot" & shot.outcome.name != "Blocked" & shot.type.name != "Penalty") %>% 
                     mutate(is_goal = ifelse(shot.outcome.name == "Goal", "Sí", "No"))

library(forcats) # trabajar con factores
ggplot(np_unblocked_shots, aes(x = fct_reorder(team.name, shot.statsbomb_xg, .fun = median), y = shot.statsbomb_xg)) +
  geom_boxplot() +
  coord_flip()

# otras variantes
library(ggbeeswarm)

b1 = ggplot(np_unblocked_shots, aes(x = fct_reorder(team.name, shot.statsbomb_xg, .fun = median), y = shot.statsbomb_xg)) +
  #geom_quasirandom(aes(col = is_goal), bandwidth = 1.5, size = 2, alpha = 0.5, varwidth = 2, dodge.width = 0.5) +
  #geom_violin() +
  geom_jitter(aes(fill = is_goal), width = 0.3, height = 0.01, alpha = 0.6, col = "black", pch = 21) +
  geom_boxplot(fill = "transparent", outlier.shape = NA) +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  coord_flip() +
  labs(fill = "¿Fue gol?", x = "Equipo\n", y = "\nxG") +
  scale_y_continuous(breaks = seq(0, 1, 0.05)) +
  theme(legend.position = "top")

b1

ggsave("boxplot.png", width = 12, height = 8)


# anotaciones
table(np_unblocked_shots$shot.statsbomb_xg > 0.5)

b1 +
  annotate("text", x = 2, y = 0.51, label = "Hubo 28 tiros no penales con xG > 0.5", col = "darkgreen", hjust = 0, size = 5) +
  annotate("segment", x = 0, xend = 25, y = 0.5, yend = 0.5, col = "darkgreen")
  
library(ggforce)
target = np_unblocked_shots %>% 
         filter(is_goal == "No") %>% 
         arrange(desc(shot.statsbomb_xg)) %>% 
         head(1) %>%
         select(id, team.name, shot.statsbomb_xg, shot.outcome.name, player.name)

text = glue("{target$player.name} de {target$team.name} no convirtió un tiro con valor xG = {round(target$shot.statsbomb_xg, 2)}")

b2 = b1 +
  geom_mark_circle(
    aes(x = team.name, y = shot.statsbomb_xg, 
        filter = id == target$id,
        label = "Lo que se perdió!", 
        description = text), 
    alpha = 0.8, # nitides del color
    inherit.aes = F, # no buscar configuraciones previas (del b1) sino del aes() que se le esta pasando actualmente
    expand = unit(10, "mm"), 
    label.width = unit(65, 'mm'),
    label.fontsize = c(11, 10), 
    label.colour = c("grey20", "grey20"))

b2

ggsave("boxplot.png", width = 12, height = 8)



### Radar
premier = read_xlsx("data/players_stats_season_21_22_england_premier_league.xlsx", col_types = "text") %>% janitor::clean_names()
names(premier)

columnas_de_texto = c("player_num", "player_name", "position", "nationality", "team", "national_team", "foot")

# preprocess
premier_clean = premier %>%
  #rename("player_name" = "x2", "player_num" = "x1") %>%
  mutate(across(everything(), ~gsub("[%]", "", .x))) %>% # alternativa para quitar los %
  mutate(across(-c(columnas_de_texto), as.numeric)) %>%
  mutate(across(where(is.double), ~replace_na(.x, 0))) %>%
  filter(minutes_played >= max(minutes_played)*0.3) %>%
  mutate(label = paste0(player_name, "\n(", team, ")"))

metricas = c("expected_assists", "goals", "x_g_expected_goals",
             "attacking_challenges_won", "air_challenges_won", "dribbles_successful",
             "chances_successful", "crosses_accurate", "passes",
             "ball_recoveries_in_opponents_half", "ball_interceptions")

players_p90 = premier_clean %>%
  select(columnas_de_texto, minutes_played, metricas, label) %>% 
  mutate(across(metricas, ~(.x/minutes_played*90), .names = "{.col}_p90"))

players_percentile = players_p90 %>%
  group_by(position) %>% 
  mutate(across(ends_with("p90"), ~round(percent_rank(.), 2), .names = "{.col}_percentile")) %>% 
  ungroup()

metricas_p90 = players_p90 %>% select(ends_with("p90")) %>% names()
players_p90_long = players_p90 %>% 
                   pivot_longer(cols = metricas_p90, names_to = "metric", values_to = "p90") # convierte "metricas_p90" a formato largo -> esas columnas las convirte en filas

metricas_percentile = players_percentile %>% select(ends_with("percentile")) %>% names()
players_percentile_long = players_percentile %>% 
                          pivot_longer(cols = metricas_percentile, names_to = "metric", values_to = "percentile")

df_selected = players_p90_long %>%
              bind_cols(players_percentile_long %>% select(percentile)) %>% #une solo la colmuna "percentile"
              filter(player_name == "Mohamed Salah") %>% 
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
  select(player_name, team, label, metric, p90, percentile)

#ALTERNATIVA PARA CALCULAR LOS ANGULOS DE LAS ETIQUETAS DE LAS METRICAS (un grafico 360°)
# viz
n_metrics = length(df_selected$metric)
temp <- 360/n_metrics/2                                      #find the difference in angle between to labels and divide by two.
myAng <- seq(-temp, -360 + temp, length.out = n_metrics)     #get the angle for every label
ang <- ifelse(myAng < -90, myAng+180, myAng)                 #rotate label by 180 in some places for readability
ang <- ifelse(ang < -90, ang+180, ang)



# importante definir: cantidad, tipo de metrica, orden
df_selected$metric = factor(df_selected$metric, 
                            levels = c("intercepciones", "recuperaciones\ncampo rival", "duelos aéreos\nganados", 
                                       "pases", "centros precisos", "xA", 
                                       "duelos ofensivos\nganados", "ocasiones exitosas", "regates exitosos", 
                                       "xG", "goles"))

ggplot(df_selected, aes(x = metric, y = percentile)) +                      
  geom_bar(aes(y = 1), fill = "#023e8a", stat = "identity", 
           width = 1, colour = "white", alpha = 0.6, linetype = "dashed") +                                                                          
  geom_bar(stat = "identity", width = 1, fill = "#001219", colour = "white", alpha = 0.8) +
  
  geom_hline(yintercept = 0.25, colour = "white", linetype = "longdash", alpha = 0.5)+
  geom_hline(yintercept = 0.50, colour = "white", linetype = "longdash", alpha = 0.5)+
  geom_hline(yintercept = 0.75, colour = "white", linetype = "longdash", alpha = 0.5)+ 
  geom_hline(yintercept = 1,    colour = "white", alpha = 0.5) +
  scale_y_continuous(limits = c(-0.1, 1)) +    
  coord_polar() + # el que transofmra en grafico de barra en grafico de radas                                                              
  
  geom_label(aes(label = round(p90, 2)), fill = "#e9d8a6", size = 2, color = "black", show.legend = FALSE) +    

  
  labs(fill = "",   
       caption = glue("Percentiles respecto a jugadores de la misma posición con al menos {round(max(premier_clean$minutes_played, na.rm = T)*0.3, 0)} min. jugados\n\nViz: LPDT  |  Data: Instat"),     
       title = glue("{df_selected$player_name[1]} ({df_selected$team[1]})"),
       subtitle = glue("Premier League 21/22 | Estadísticas cada 90 min.")) +
  theme_minimal() +                                                                     
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, angle = mean(ang)), # las etiquetas del eje X van a tener un angulo (REVISAR)
        plot.title = element_markdown(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(size = 10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(5, 2, 2, 2)) 


ggsave("radar_Salah.png", height = 7, width = 7)

