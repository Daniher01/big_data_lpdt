premier = read_xlsx("data/players_stats_season_21_22_england_premier_league.xlsx", col_types = "text") %>% janitor::clean_names() %>%
                        mutate(across(c(ends_with("percent"), "chances_percent_of_conversion"), ~as.numeric(str_replace(.x, "%", ""))))

target = "Richarlison"
umbral_minimo_MJ = max(as.numeric(premier$minutes_played))* 0.25

players = premier %>% 
            filter(as.numeric(age) >= 23 & as.numeric(age) <= 28,
                   as.numeric(minutes_played) >= as.numeric(umbral_minimo_MJ),
                   position == "F",
                   player_name != target,
                   team != "Everton")

players = players %>%
                      mutate(valor_millones_euros = case_when(player_name == "Gabriel Jesus" ~ 75,
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
