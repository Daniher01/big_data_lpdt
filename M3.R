##### M3

#### Importación de datos

### formato CSV

## paquete {readr}

#install.packages("readr")
library(readr)

# función read_csv()
premier = read_csv("data/instat_players_stats_season_21_22_england_premier_league.csv")
ruta = "data/"
premier = read_csv(paste0(ruta, "instat_players_stats_season_21_22_england_premier_league.csv"))

euro2020_games = read_csv(paste0(ruta, "statsbomb_info_partidos_euro_2020.csv"))
euro2020_events = read_csv(paste0(ruta, "statsbomb_eventing_data_euro_2020.csv"))


# otros argumentos de la función son:
# col_names = TRUE (T) por defecto: lee la primera fila como nombres de columnas
# n_max: cantidad de filas a leer
# skip = 0 por defecto. (skip: "cantidad de filas a saltarse, sin leer")
# siempre es posible revisar esto en la ayuda: ?read_csv()


### formato XLSX

## paquete {readxl}
#install.packages("readxl")

library(readxl)
premier_xlsx = read_xlsx("data/players_stats_season_21_22_england_premier_league.xlsx")

# también tiene los parámetros n_max, skip, col_names
# na = "" por defecto: los valores NA los lee como vacíos. Se puede configurar para cambiarlo por 0, "-" o algo similar 


## Es útil complementar la lectura del archivo con la función clean_names() del paquete {janitor}
## Limpia los nombres de las columnas por ejemplo -> quitando los espeacios

# install.packages("janitor")
library(janitor)
premier_xlsx_clean = clean_names(premier_xlsx)
names(premier_xlsx)
names(premier_xlsx_clean)


## Editar nombres de columnas
names(premier_xlsx_clean)[1:2] = c("player_num", "player_name")
# cambia nombre de las columnas 1 y 2 -> [1:2] y coloca lo que esta en c("columna 1", "columna 2")



#### breve EDA [Exploratory Data Analysis]

### Clases o tipos de variables

library(dplyr)
glimpse(premier_xlsx)
glimpse(euro2020_events)
glimpse(euro2020_games)

## argumento "col_types" = NULL por defecto: trata de adivinar qué clase según los valores que trae el archivo. 

# un solo valor como input: se aplica lo mismo para todas las columnas
premier_xlsx = read_xlsx("data/players_stats_season_21_22_england_premier_league.xlsx", col_types = "text") 
glimpse(premier_xlsx)

premier_xlsx = read_xlsx("data/players_stats_season_21_22_england_premier_league.xlsx", 
                         col_types = c(rep("text", 2), "numeric", "text", rep("numeric", 77), rep("text", 2)))

# convierte las 2 primeras columnas en texto
# convierte la siguiente columna (la tercera) en numerico
# convierte la siguiente columna (la cuarta) en texto
# convierte las siguientes 77 columnas en numericos
# convierte la 2 ultimas (78, 79) en texto

glimpse(premier_xlsx)


## edición de columnas usando la nomenclatura base (antigua) de R:
## convierte la columna "expected_assists" a numerico
premier_xlsx_clean$expected_assists = as.numeric(premier_xlsx_clean$expected_assists) # de texto (caracter) a numérica
glimpse(premier_xlsx_clean)

# as.character() para transformar a texto
# as.factor() para transformar a factor (categórica en R)
# as.double() para transformar a numérica con muchos decimales
# as.logical() para transformar a booleana TRUE/FALSE

# cuando pasamos de factor a número se sugiere pasar primero a caracter: as.numeric(as.character(...))


### EDA para variables numéricas

# summary()
summary(premier_xlsx_clean$minutes_played)
summary(premier_xlsx_clean$age)
summary(premier_xlsx_clean$goals)
summary(as.numeric(premier_xlsx_clean$goals))
summary(euro2020_events$shot.statsbomb_xg)

# quantile() 
quantile(euro2020_events$shot.statsbomb_xg, na.rm = T) # True (T) para remover los Nulos  (NA)
quantile(euro2020_events$shot.statsbomb_xg, seq(0, 1, 0.1), na.rm = T)
quantile(euro2020_events$location.x, na.rm = T)
quantile(euro2020_events$location.y, na.rm = T)


### EDA para variables categóricas (texto) o booleanas

# table()
table(premier$Position)
table(premier$Nationality)
table(premier$Foot)
table(euro2020_games$competition_stage.name)
table(euro2020_games$competition.competition_name)

# usar el argumeto "useNA = 'ifany' " para que muestre los casos NA en caso de que haya
table(euro2020_events$type.name, useNA = 'ifany')
table(euro2020_events$position.name, useNA = 'ifany')



#### Proceso de datos con {dplyr}

### Ejemplos aislados con: select, filter, mutate, rename, arrange

# select() para elegir o descartar columnas específicas
names(premier_xlsx_clean)
player_profile = select(premier_xlsx_clean, player_name, player_num, team, position, minutes_played, nationality, age, height, weight, foot)
player_stats = select(premier_xlsx_clean, -c(player_num, team, position, nationality, age, height, weight, foot)) # todas menos las que estan dentro de -c()

# filter() para elegir filas que cumplen una condición específica
table(premier_xlsx_clean$team)
liverpool_players = filter(premier_xlsx_clean, team == "Liverpool")
liverpool_players_profile = filter(player_profile, team == "Liverpool")

player_profile$age = as.numeric(as.character(player_profile$age))
liverpool_players_profile_sub23 = filter(player_profile, team == "Liverpool" & age <= 23) 

# mutate() para crear nuevas columnas o editar una existente 
player_profile = mutate(player_profile, height = as.numeric(as.character(height)))
player_profile = mutate(player_profile, height_mts = height/100)

# rename() para cambiar el nombre de columnas
premier_xlsx_clean = janitor::clean_names(premier_xlsx)
premier_xlsx_clean = rename(premier_xlsx_clean, "x1" = "player_num", "x2" = "player_name")

# arrange() para ordenar filas según los valores de columna(s)
player_profile = arrange(player_profile, desc(minutes_played))


### pipe: %>% 
player_profile_pipe = read_xlsx("data/players_stats_season_21_22_england_premier_league.xlsx", col_types = "text") %>%
                      clean_names() %>% 
                      rename("x1" = "player_num", "x2" = "player_name") %>% 
                      select(x2, x1, team, position, minutes_played, nationality, age, height, weight, foot) %>% 
                      mutate(age = as.numeric(age),
                             minutes_played = as.numeric(minutes_played),
                             height = as.numeric(height),
                             height_mts = height/100) %>% 
                      arrange(desc(minutes_played))

liverpool_players_profile_sub23_pipe = player_profile_pipe %>% 
                                       filter(team == "Liverpool" & age <= 23)

player_stats_pipe = read_xlsx("data/players_stats_season_21_22_england_premier_league.xlsx", col_types = "text") %>%
  clean_names() %>% 
  select(-c(player_num, position, nationality, age, height, weight, foot)) %>% 
  mutate(minutes_played = as.numeric(minutes_played),
         goals = as.numeric(goals)) %>% 
  arrange(desc(goals))


### ejemplos enfocados en cálculo de métricas según contenidos de módulos 1 y 2 (esto incluye a la funciones group_by() y summarise())

## ranking de goals p90 a nivel de jugadores en Premier 21/22
goles_p90 = player_stats_pipe %>% 
            select(player_name, team, minutes_played, goals) %>% 
            mutate(goals_p90 = goals/minutes_played*90) %>% 
            arrange(desc(goals_p90)) 

# aplicando un filtro como valor mínimo
umbral_minimo_MJ = max(player_stats_pipe$minutes_played)*0.3 # 30% del total de los minutos
goles_p90_filtrado = goles_p90 %>% 
                     filter(minutes_played >= umbral_minimo_MJ)


## promedio de edad y altura por equipo en Premier
# (acá descubrimos que la info de la columna team no es precisa)
promedio_edad_y_altura_equipos = player_profile_pipe %>%
                        filter(team != "-") %>% 
                        group_by(team) %>% 
                        summarise(number_players = n(),
                                  promedio_edad = mean(age),
                                  promedio_altura = mean(height_mts, na.rm = T)) %>% 
                        arrange(desc(number_players)) %>% 
                        head(20) %>% 
                        arrange(desc(promedio_edad))



## distancia promedio de los tiros de cada jugador en la Euro 2020
table(euro2020_events$type.name)
table(euro2020_events$shot.outcome.name)
dist_promedio_tiros = euro2020_events %>% 
                     filter(type.name == "Shot") %>% 
                     group_by(player.name, team.name) %>% 
                     summarise(n_tiros = n(),
                               n_goals = sum(ifelse(shot.outcome.name == "Goal", 1, 0)),
                               xg_sum = sum(shot.statsbomb_xg),
                               dist_prom = mean(DistToGoal),
                               dist_median = median(DistToGoal)) %>%
                     filter(n_tiros > 2) %>% 
                     arrange(dist_prom) %>% 
                     mutate(xg_dif = round(n_goals - xg_sum, 2)) %>% 
                     arrange(desc(xg_dif))

quantile(dist_promedio_tiros$n_tiros, seq(0, 1, 0.1))



## precision de pases por equipo en Euro 2020
table(euro2020_events$type.name)
table(euro2020_events$pass.outcome.name, useNA = "ifany")

precision_pases = euro2020_events %>% 
  filter(type.name %in% c("Pass")) %>% 
  group_by(team.name) %>% 
  summarise(n_pases = n(),
            n_pases_correctos = sum(ifelse(is.na(pass.outcome.name), 1, 0))) %>%
  mutate(precision_pases = round(n_pases_correctos / n_pases*100, 1)) %>% 
  arrange(desc(precision_pases))


## creando una variable que segmenta el campo de juego en 2 mitades (en sentido longitudinal) y luego agrupar un cálculo en ella
cantidad_perdidas_balon_por_mitad = euro2020_events %>% 
  filter(type.name %in% c("Dispossessed", "Miscrontrol")) %>%
  mutate(half = ifelse(location.x > 60, "opponent half", "own half")) %>%
  group_by(team.name, half) %>% 
  summarise(n_perdidas = n()) %>%
  arrange(desc(n_perdidas))




### Otros casos complementarios

## paquete {stringr} para editar texto
library(stringr)

# str_replace() para reemplazar un patrón de caracteres por otro
# str_detect() para detectar si hay un patrón o no (devuelve TRUE o FALSE)
premier_xlsx_clean = read_xlsx("data/players_stats_season_21_22_england_premier_league.xlsx", col_types = "text") %>% clean_names()
names(premier_xlsx_clean)
table(premier_xlsx_clean$successful_dribbles_percent)
table(premier_xlsx_clean$nationality)

string_fix = premier_xlsx_clean %>%
             #rename("player_num" = "x1", "player_name" = "x2") %>% 
             mutate(successful_dribbles_percent_fixed = str_replace(successful_dribbles_percent, "%", ""),
                    apellido = str_replace(player_name, ".* ", ""),
                    first_nationality = str_replace(nationality, ",.*", ""),
                    second_nationality = ifelse(str_detect(nationality, ","), str_replace(nationality, ".*, ", ""), "")) %>% 
             select(player_name, apellido, successful_dribbles_percent, successful_dribbles_percent_fixed, nationality, first_nationality, second_nationality)

table(string_fix$successful_dribbles_percent_fixed)                 



## paquete {lubridate} para trabajar con fechas
glimpse(euro2020_games)
#install.packages("lubridate")
library(lubridate)

# funciones para extraer info específica de una fecha
month(euro2020_games$match_date)
table(month(euro2020_games$match_date))
year(euro2020_games$match_date)
day(euro2020_games$match_date)
weekdays(euro2020_games$match_date)

# convertir una variable de texto a fecha
glimpse(premier_xlsx_clean)

fechas = premier_xlsx_clean %>% 
  #rename("player_num" = "x1", "player_name" = "x2") %>% 
  mutate(national_team_last_match_date_mm_yy = as.character(national_team_last_match_date_mm_yy),
         national_team_last_match_date_mm = str_replace(national_team_last_match_date_mm_yy, "[.].*", ""),
         national_team_last_match_date_yy = str_replace(national_team_last_match_date_mm_yy, ".*[.]", ""),
         national_team_last_match_date_mm_yy_fix = my(paste0(national_team_last_match_date_mm, "/", national_team_last_match_date_yy))) %>% 
  select(player_name, national_team_last_match_date_mm_yy, national_team_last_match_date_mm, 
         national_team_last_match_date_yy, national_team_last_match_date_mm_yy_fix)

glimpse(fechas)


#### Exportar datos en CSV y RDS

write_csv(precision_pases, "data/precision_pases.csv")
write_rds(precision_pases, "data/precision_pases.rds")


## TAREA: ver en la plataforma el enunciado con el detalle de los 2 ejercicios a desarrollar para evaluar este módulo, además del respectivo cuestionario
