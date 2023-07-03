
# Bibliotecas -------------------------------------------------------------


require(jsonlite)
library(tidyverse)
library(dplyr)
library(stringr)
library(DT)
library(lubridate)
library(GET)
library(httr)


# Authorization -----------------------------------------------------------

HOST <- "https://apirest.wyscout.com/v2/"
API_KEY <- "od3j832-qrjbsfwsr-jiq33j7-gdbshhna2x"
API_SECRET <- "+Xo52v,tjcTugE9._zc(sW=m9ui4Rn"

key = paste(API_KEY, API_SECRET, sep = ":")
key = charToRaw(key)
key = base64enc::base64encode(key)
key = paste("Basic", key)


# Search League -----------------------------------------------------------

partidos <- tibble()

for (i in 1:1000) {
  tryCatch({
    a <- i
    print(a)
    df_aux <- paste0('https://apirest.wyscout.com/v2/competitions/',a,'/matches')
    df_aux <- GET(url=df_aux,add_headers(Authorization=key))
    df_aux <- rawToChar(df_aux$content)
    df_aux <- fromJSON(df_aux, flatten = T)
    df_aux <- df_aux[["matches"]]
    df_aux$league_id <- a
    partidos <- bind_rows(partidos,df_aux)
  }, error=function(e){})
}

partidos_v2 <- partidos %>% filter(grepl("Palmeiras",partidos$label) & date > "2023-01-01")
partidos <- partidos_v2


# League Matches ----------------------------------------------------------

ligas = c("8","82","84","85","223","229","244","795", "255", "364")
ligas <- c("364")
partidos <- tibble()
#Lista de partidos
for (i in 1:length(ligas)) {
  a <- ligas[i]
  print(a)
  df_aux <- paste0('https://apirest.wyscout.com/v2/competitions/',a,'/matches')
  df_aux <- get(url=df_aux,add_headers(Authorization=key))
  df_aux <- rawToChar(df_aux$content)
  df_aux <- fromJSON(df_aux, flatten = T)
  df_aux <- df_aux[["matches"]]
  df_aux$league_id <- a
  partidos <- bind_rows(partidos,df_aux)
}
rm(df_aux)

#filtramos únicamente partidos finalizados
partidos <- partidos %>%
  filter(status=="Played" & seasonId == 188095 & gameweek == 27)

partidos <- partidos %>%
  filter(status=="Played")

# partidos[c('home_team', 'delete_this_column')] <- str_split_fixed(partidos$label, ' - ', 2)
# partidos[c('away_team', 'result')] <- str_split_fixed(partidos$delete_this_column, ', ', 2)
# partidos[c('home_goals', 'away_goals')] <- str_split_fixed(partidos$result, '-', 2)
# partidos <- subset( partidos, select = -c(delete_this_column))
# #partidos_Segunda <- partidos 
# 
# #contar partidos extraidos
# nrow(partidos) 


# Eventing ----------------------------------------------------------------

partidos_eventing <- tibble()
s = 1
#partidos$matchId
for (x in partidos$matchId) {
  print(x)
  print(s)
  df_aux <- paste0('https://apirest.wyscout.com/v3/matches/',x,'/events')
  df_aux <- GET(url=df_aux, add_headers(Authorization=key))
  df_aux <- rawToChar(df_aux$content)
  df_aux <- fromJSON(df_aux, flatten = T)
  df_aux <- df_aux[["events"]]
  df_aux <- data.frame(df_aux)
  #df_aux <- subset(df_aux, select= -c(type.secondary))
  partidos_eventing <- bind_rows(partidos_eventing,df_aux)
  s = s+1
}

rm(df_aux)


# Stats -------------------------------------------------------------------

#Maestro de equipos únicos
maestro_teams = partidos %>%
  select(home_team) %>%
  rename(team_name = home_team)

maestro_teams1 = partidos %>%
  select(away_team) %>%
  rename(team_name = away_team)

equipos <- bind_rows(maestro_teams,maestro_teams1) %>%
  distinct()

#elimino dataframe innecesario para evitar acumulacion de tablas
rm(maestro_teams)
rm(maestro_teams1)

# filtro solo los partidos de mi equipo

list1 = c("general","possession","openPlay","attacks","transitions","passes","defence",
          "duels","flanks","teams")
list2 = c("home","away")
partidos_stats <- tibble()
partidos_stats_1 <- tibble()
estadisticas_partido_br <- tibble()
for (i in 1:length(list2)) {
  print(list2[i])
  for (x in partidos$matchId) {
    print(x)
    for (j in 1:length(list1)) {
      print(list1[j])
      df_aux <- paste0('https://apirest.wyscout.com/v2/matches/',x,'/advancedstats?useSides=1')
      df_aux <- GET(url=df_aux, add_headers(Authorization=key))
      df_aux <- rawToChar(df_aux$content)
      df_aux <- fromJSON(df_aux, flatten = T)
      df_aux <- df_aux[[list1[j]]][[list2[i]]]
      df_aux <- data.frame(df_aux)
      df_aux$team_status <- list2[i]
      df_aux$matchID <- x
      df_aux$llave <- paste0(x,list2[i])
      if (j == 1) {
        partidos_stats_1 <- df_aux
      } else {
        partidos_stats_1 <- merge(x=partidos_stats_1,y=df_aux,by="llave",all.x=TRUE)
      }
    }
    partidos_stats <- bind_rows(partidos_stats,partidos_stats_1)
  }
  if (i == 1) {
    estadisticas_partido_br <- partidos_stats
  } else {
    estadisticas_partido_br <- bind_rows(estadisticas_partido_br,partidos_stats_br)
  }
}
rm(partidos_stats,partidos_stats_1,df_aux)


# Tests -------------------------------------------------------------------

partidos_eventing_la_liga <- partidos_eventing_outros %>% filter(team.name == "Getafe")
df_aux <- partidos_eventing_la_liga$opponentTeam.name
partidos_eventing_la_liga <- partidos_eventing_la_liga$team.name
partidos_eventing_la_liga <- c(partidos_eventing_la_liga, df_aux)
partidos_eventing_la_liga <- partidos_eventing_la_liga[!duplicated(partidos_eventing_la_liga)]

partidos_eventing_la_liga <- partidos_eventing_outros %>% filter(partidos_eventing_outros$team.name %in% partidos_eventing_la_liga)

teste <- partidos_eventing_la_liga %>% select(id,type.secondary, possession.types)
teste_2 <- teste[,"type.secondary"]
teste_3 <- teste[,"possession.types"]

teste_4 <- data.frame()
for (i in 1:nrow(teste_2)){
  df_aux <- character(0)
  for (j in 0:20){
    tryCatch({
      df_aux <- paste(df_aux,teste_2[[1]][[i]][[j]],sep=",")
    }, error=function(e){})
  }
  df_aux <- bind_cols(event_id = teste[i,1],type.secondary = df_aux)
  teste_4 <- rbind(teste_4,df_aux)
  print(i)
}

teste_5 <- data.frame()
for (i in 1:nrow(teste_3)){
  df_aux <- character(0)
  for (j in 0:20){
    tryCatch({
      df_aux <- paste(df_aux,teste_3[[1]][[i]][[j]],sep=",")
    }, error=function(e){})
  }
  df_aux <- bind_cols(event_id = teste[i,1],possession.types = df_aux)
  teste_5 <- rbind(teste_5,df_aux)
  print(i)
}
teste_join <- left_join(teste %>% select(id),
                        teste_4,
                        by = c("id"="id"))
teste_join <- left_join(teste_join,
                        teste_5,
                        by = c("id"="id"))

rm(teste, teste_2, teste_3, teste_4, teste_5)

partidos_eventing_teste <- left_join(partidos_eventing_la_liga %>% select(-type.secondary,-possession.types),
                                     teste_join,
                                     by = c("id"="id"))

write.csv(partidos_eventing_teste, "G:/Meu Drive/Máster en Big Data/TFM/R/Wyscout/eventing_pr.csv", row.names = FALSE, na = "0")


#Teste 2

events_newcastle <- partidos_eventing %>% filter(team.name == "Newcastle United")
nrow(distinct(data.frame(events_newcastle$matchId)))
nrow(events_newcastle)/nrow(distinct(data.frame(events_newcastle$matchId)))
events_newcastle <- events_newcastle %>% unchop(possession.types)
teste <-  unnest(events_newcastle$type.secondary)
teste <- partidos_eventing[,"type.secondary"]
events_newcastle$possession.types <- events_newcastle[,"possession.types"]


teste <- read.csv("G:/Meu Drive/Máster en Big Data/TFM/R/Wyscout/PL_Events.csv")
man_city_events <- teste %>% filter(team.name == "Manchester City")
teste_2 <- distinct(data.frame(man_city_events$matchId))
write.csv(man_city_events, "G:/Meu Drive/Máster en Big Data/TFM/R/Wyscout/man_city_events.csv", row.names = FALSE, na = "0")

rm(df_aux, df_aux, teste, teste_2, teste_3, teste_4, teste_5, a, API_KEY, API_SECRET, HOST, i, j, key, ligas, s, x)


str(partidos_eventing_x)
partidos_eventing_x$class <- gsub('\\"',"",
                                       gsub('\\"\\)',"",
                                            gsub('c\\(\\"',"",as.character(partidos_eventing_x$type.secondary))))

partidos_eventing_x$class[length(partidos_eventing_x$class) == 0] <- NA

write.csv(partidos_eventing_br %>% select(-type.secondary, -possession.types), "G:/Meu Drive/Máster en Big Data/TFM/R/Wyscout/eventing_br.csv", row.names = FALSE, na = "0")
