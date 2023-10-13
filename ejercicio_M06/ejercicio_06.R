# ------------------------------ EJERCICIO 1 --------------------------------
euro_2020_events = read_csv("data/statsbomb_eventing_data_euro_2020.csv")
euro_2020_games = read_csv("data/statsbomb_info_partidos_euro_2020.csv")

target_game = euro_2020_events %>% 
  filter(match_id == "3794686" & type.name == "Own Goal For"  | match_id == "3794686" & type.name == "Shot" & shot.outcome.name != "Blocked") %>%
  select(match_id, ElapsedTime, type.name, Team = team.name, player.name, xG = shot.statsbomb_xg, shot.outcome.name) %>% 
  mutate(time_min = round(ElapsedTime/60, 0),
         shot.outcome.name = ifelse(type.name == "Own Goal For", "Goal", shot.outcome.name),
         xG = ifelse(type.name == "Own Goal For", 0, xG),
         player.name = ifelse(type.name == "Own Goal For", "Own Goal", player.name))


xg_cum = target_game %>%
  arrange(ElapsedTime) %>% 
  group_by(Team) %>% 
  mutate(xG_cum = cumsum(xG),
         Goals_cum = cumsum(ifelse(shot.outcome.name == "Goal", 1, 0))) %>%
  bind_rows(xg_cum %>% filter(Team == "Spain") %>% head(1) %>% mutate(time_min = 0, xG_cum = 0)) %>%
  bind_rows(xg_cum %>% filter(Team == "Croatia" & type.name != "Own Goal For") %>% head(1) %>% mutate(time_min = 0, xG_cum = 0))

goals = xg_cum %>% filter(shot.outcome.name == "Goal")

p1 = ggplot(xg_cum, aes(x = time_min, y = xG_cum, col = Team)) +
  geom_step(size = 1) +
  geom_point(data = goals, aes(fill = Team), size = 3, pch = 21, col = "black") +
  theme_bw() +
  geom_vline(xintercept = 45, linetype = "dotted", size = 1) +
  geom_vline(xintercept = 95, linetype = "dotted", size = 1) +
  geom_vline(xintercept = 105, linetype = "dotted", size = 1) +
  geom_label_repel(data = goals, aes(label = player.name), size = 3) +
  #scale_y_continuous(breaks = seq(0, 1.4, 0.2), expand = c(0.01, 0.01), limits = c(0, 1.4)) +
  scale_x_continuous(breaks = seq(0, 125, 15), expand = c(0.01, 0.01), limits = c(0, 125)) +
  labs(x = "\nTime (minutes)", y = "Cummulated xG\n") +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

p1 +
  theme(legend.position = "none")


spain = xg_cum %>% filter(Team == 'Spain')
croatia = xg_cum %>% filter(Team == 'Croatia')
title_text = glue("<b style = 'color: #E41A1C'>Spain {max(spain$Goals_cum)} (xG = {round(max(spain$xG_cum), 3)}) <br> <b style='color: #377EB8'> Croatia {max(croatia$Goals_cum)} (xG = {round(max(croatia$xG_cum), 3)})<br>")


p2 = p1 +        
  labs(title = title_text,
       caption = "Data: Statsbomb",
       subtitle = "Round of 16 - Euro 2020") +
  theme(plot.title = element_markdown(size = 18), # permite reconocer el texto HTML
        plot.subtitle = element_text(face = "italic"),
        legend.position = "none")

p2

ggsave("ejercicio_m06/timeline_match_own_Goal.png", height = 7, width = 10)

# ---------------------------- EJERCICIO 2 ------------------------
premier = read_xlsx("data/players_stats_season_21_22_england_premier_league.xlsx", col_types = "text") %>% clean_names()


metricas = c("expected_assists", "x_g_expected_goals", "attacking_challenges", "air_challenges", "dribbles", "crosses", 
             "passes", "lost_balls", "ball_interceptions", "ball_recoveries")

columnas_de_texto = c("player_num", "player_name", "position", "nationality", "team", "national_team", "foot")

target = "Heung-Min Son"
color_target = "#023e8a"

target_2 = "Leandro Trossard"
color_target_2 = "red"


premier_clean  = premier %>%
  mutate(across(everything(), ~gsub("[%]", "", .x))) %>%
  mutate(across(-c(columnas_de_texto), as.numeric)) %>%
  mutate(across(where(is.double), ~replace_na(.x, 0))) %>%
  filter(minutes_played >= max(minutes_played)*0.3) %>%
  mutate(label = paste0(player_name, "\n(", team, ")"))

players_p90 = premier_clean %>%
  select(columnas_de_texto, minutes_played, metricas, label) %>% 
  mutate(across(metricas, ~(.x/minutes_played*90), .names = "{.col}_p90"))

players_percentile = players_p90 %>%
  group_by(position) %>% 
  mutate(across(ends_with("p90"), ~round(percent_rank(.), 2), .names = "{.col}_percentile")) %>% 
  ungroup()
    
metricas_p90 = players_p90 %>% select(ends_with("p90")) %>% names()
players_p90_long = players_p90 %>% 
  pivot_longer(cols = metricas_p90, names_to = "metric", values_to = "p90") 

metricas_percentile = players_percentile %>% select(ends_with("percentile")) %>% names()
players_percentile_long = players_percentile %>% 
  pivot_longer(cols = metricas_percentile, names_to = "metric", values_to = "percentile")


df_selected = players_p90_long %>%
  bind_cols(players_percentile_long %>% select(percentile)) %>% #une solo la colmuna "percentile"
  mutate(metric = case_when(metric == "x_g_expected_goals_p90" ~ "xG",
                            metric == "expected_assists_p90" ~ "xA",
                            metric == "air_challenges_p90" ~ "duelos aéreos\nganados",
                            metric == "attacking_challenges_p90" ~ "duelos ofensivos\nganados",
                            metric == "dribbles_p90" ~ "regates exitosos",
                            metric == "chances_successful_p90" ~ "ocasiones exitosas",
                            metric == "crosses_p90" ~ "centros precisos",
                            metric == "passes_p90" ~ "pases",
                            metric == "lost_balls_p90" ~ "balones\nperdidos",
                            metric == "ball_interceptions_p90" ~ "intercepciones",
                            metric == "ball_recoveries_p90" ~ "recuperaciones")) %>% 
  select(player_name, team, position, label, metric, p90, percentile)

player_1 = df_selected %>% filter(player_name == target)
player_2 = df_selected %>% filter(player_name == target_2)

df_filtered = df_selected %>% filter(player_name == target | player_name == target_2)

# viz
n_metrics = length(df_selected$metric)
temp <- 360/n_metrics/2                                      #find the difference in angle between to labels and divide by two.
myAng <- seq(-temp, -360 + temp, length.out = n_metrics)     #get the angle for every label
ang <- ifelse(myAng < -90, myAng+180, myAng)                 #rotate label by 180 in some places for readability
ang <- ifelse(ang < -90, ang+180, ang)


df_selected$metric = factor(df_selected$metric, 
                            levels = c("balones\nperdidos", "intercepciones", "recuperaciones", "duelos aéreos\nganados", 
                                       "pases", "centros precisos", "xA", 
                                       "duelos ofensivos\nganados", "ocasiones exitosas", "regates exitosos", 
                                       "xG", "goles"))


ggplot(df_selected, aes(x = metric, y = percentile)) +                      
  geom_bar(data = player_1, fill = color_target, stat = "identity", width = 1,  alpha = 0.2) +                                                                          
  geom_bar(data = player_2, fill = color_target_2, stat = "identity", width = 1, alpha = 0.2) +
  
  geom_hline(yintercept = 0.25, colour = "white", linetype = "longdash", alpha = 0.5)+
  geom_hline(yintercept = 0.50, colour = "white", linetype = "longdash", alpha = 0.5)+
  geom_hline(yintercept = 0.75, colour = "white", linetype = "longdash", alpha = 0.5)+ 
  geom_hline(yintercept = 1,    colour = "white", alpha = 0.5) +
  scale_y_continuous(limits = c(-0.1, 1)) +    
  coord_polar() + # el que transofmra en grafico de barra en grafico de radas                                                              
  
  geom_label(data = player_1, aes(label = round(p90, 2)), fill = color_target, size = 2,alpha = 0.5, show.legend = FALSE) + 
  geom_label(data = player_2, aes(label = round(p90, 2)), fill = color_target_2, size = 2, alpha = 0.5) +
  
  
  labs(fill = "",   
       caption = glue("Percentiles respecto a jugadores de la misma posición con al menos {round(max(premier_clean$minutes_played, na.rm = T)*0.3, 0)} min. jugados\n\nViz: LPDT  |  Data: Instat"),     
       title = glue("<b style = 'color: {color_target}'>{player_1$player_name[1]} ({player_1$team[1]}) <b style = 'color: black'>vs <b style = 'color: {color_target_2}'>{player_2$player_name[1]} ({player_2$team[1]})"),
       
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

ggsave("ejercicio_m06/radar_heung-min_son_compare.png", height = 7, width = 10)
