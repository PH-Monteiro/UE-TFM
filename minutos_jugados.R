#Minutos por partido

require(jsonlite)
library(tidyverse)
library(dplyr)
library(stringr)
library(DT)
library(lubridate)
library(httr)
library(hms)

#Base de Datos
events <- read.csv("G:/Meu Drive/Máster en Big Data/TFM/R/Wyscout/PL/PL_Events.csv")
teste <- events %>% select(id, matchId, team.id, team.name, opponentTeam.id, opponentTeam.name,player.id, player.name, matchTimestamp)
teste$matchTimestamp2 <- as_hms(teste$matchTimestamp)

#Minutos Jogados
minutos_jugados <- teste %>%
  group_by(matchId ,player.id, player.name, team.id, team.name, opponentTeam.id, opponentTeam.name,) %>%
  summarise(min_min = min(matchTimestamp2) , max_min = max(matchTimestamp2))
minutos_jugados$minutos <- as.numeric((minutos_jugados$max_min - minutos_jugados$min_min)/60)
minutos_jugados <- minutos_jugados %>% filter(player.name != "0" & minutos > 0) %>% select(-min_min, -max_min)

#Titular
titular <- teste %>%
  filter(player.name != "0") %>%
  group_by(matchId, player.id, player.name, team.id, team.name) %>%
  summarise(min_min = min(matchTimestamp2)) %>%
  arrange(min_min) %>%
  group_by(matchId, team.id, team.name) %>%
  slice(1:11)
titular$titular <- TRUE

#Junção
players_minutes <- left_join(minutos_jugados, titular %>% ungroup() %>% select(matchId, player.id, titular), by = c("matchId", "player.id"))


#Jogos por Time
jogos_time <- events %>% group_by(team.id, team.name) %>% summarise(matches = n_distinct(matchId))

rm(minutos_jugados, teste, titular, events)

#CSV
write.csv(players_minutes, "G:/Meu Drive/Máster en Big Data/TFM/R/Wyscout/PL/players_match.csv", row.names = FALSE, na = "0")
write.csv(jogos_time, "G:/Meu Drive/Máster en Big Data/TFM/R/Wyscout/PL/teams_match.csv", row.names = FALSE, na = "0")
