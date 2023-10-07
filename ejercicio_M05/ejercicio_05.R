# ------------- EJERCICIOS 1 Y 2 ----------------------
top10_euro2020 = read_csv("ejercicio_M03/top_20_premier_precision_opponent_half.csv")

g1 = ggplot(data = top10_euro2020, 
            aes(x = fct_reorder(team.name, precision_pases), y = precision_pases)) +
            geom_bar(stat = "identity", fill = "#457b9d", col = "black") +
            scale_y_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10),limits = c(0, 90)) +
            coord_flip() +
            theme_bw() +
            labs(x = "\nTeam", y = "% precisión de pases\n",
                 title = "Top 10 equipos de la euro 2020 con mejor % de precisión de pases\n") +
            geom_label(aes(label = precision_pases))
g1

ggsave("ejercicio_M05/barras_top10_equipos_precision_pases_euro2020.png", width = 12, height = 8)

#---------------------- EJERCICIO 3 -------------------------------------
euro_events = read_csv("data/statsbomb_eventing_data_euro_2020.csv")
euro_2020_games = read_csv("data/statsbomb_info_partidos_euro_2020.csv")

goles = euro_events %>%
          filter(shot.outcome.name == "Goal") %>%
          mutate(minute_goal = round(ElapsedTime/60, 0))

goles_por_minuto = goles %>%
              group_by(minute_goal) %>%
              mutate(n_goals = n(),
                     mean_xg = mean(shot.statsbomb_xg)) %>%
              filter(minute_goal <= 120)

goles_euro_por_minuto = goles_por_minuto %>% 
          select(match_id,team.name, minute_goal, n_goals, mean_xg) %>%
          left_join(euro_2020_games %>% select(match_id, competition_stage.name), by = "match_id")



g2 = ggplot(goles_euro_por_minuto, aes(x = minute_goal, y = n_goals)) +
      theme_bw() +
  scale_x_continuous(breaks = seq(0, 120, 10), labels = seq(0, 120, 10),limits = c(0, 120)) +
  geom_point(aes(size = mean_xg, fill = competition_stage.name), shape = 21, alpha = 0.4) +
  labs(title = "Cantidad que goles en cada minuto del torneo",
       subtitle = "Goles tomado en cuenta el tiempo extra, excluyendo las tandas de penales",
       x = "\n Minuto", y = "Goles\n",
       caption = "Data: Statsbomb",
       size = "xG promedio",
       fill = "Faceta del torneo") 

g2

ggsave("ejercicio_M05/grafico_dispersión_goles_por_minuto_euro2020.png", width = 12, height = 8)
        