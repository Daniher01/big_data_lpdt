# 

#--------------- EJERCICIO 1 -------------------------
premier = read_xlsx("data/players_stats_season_21_22_england_premier_league.xlsx", col_types = "text") %>% clean_names()

top_20_premier = premier %>%
            filter(minutes_played > 90)  %>%
            mutate(xG_xA = as.numeric(ifelse(x_g_expected_goals == "-", 0, x_g_expected_goals)) + 
                          as.numeric(ifelse(expected_assists == "-", 0, expected_assists)),
                   xG_xA_p90 = xG_xA/as.numeric(minutes_played)*90) %>%
            select(player_name, expected_assists, x_g_expected_goals, minutes_played, xG_xA, xG_xA_p90) %>%
            arrange(desc(xG_xA_p90)) %>%
            head(20)

write_csv(top_20_premier, "ejercicio_M03/top20_premier_xG_xA")

#------------EJERCICIO 2-----------------------------
euro_events = read_csv("data/statsbomb_eventing_data_euro_2020.csv")

top_10_euro = euro_events %>%
              filter(type.name == "Pass" & location.x > 60) %>%
              group_by(team.name) %>%
              summarise(n = n(),
                        n_pases_correctos = sum(ifelse(is.na(pass.outcome.name), 1, 0))) %>%
              mutate(precision_pases = round(n_pases_correctos/n*100,2)) %>%
              arrange(desc(precision_pases)) %>%
              head(10)

xG_tiros = euro_events %>%
          filter(type.name == "Shot" & shot.outcome.name == "Saved to Post") %>%
          arrange(location.x)

table(euro_events$shot.outcome.name)

write_csv(top_20_premier, "ejercicio_M03/top_20_premier_precision_opponent_half")
