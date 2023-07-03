# Red de Pases
library(dplyr)
library(hms)
library(fmsb)
library(BBmisc)

#Base de Dados
  #events <- read.csv("G:/Meu Drive/Máster en Big Data/TFM/R/Wyscout/PL/PL_Events.csv")
events <- read.csv("G:/Meu Drive/Máster en Big Data/TFM/R/Wyscout/events_top_5_leagues.csv")

events_br <- read.csv("G:/Meu Drive/Máster en Big Data/TFM/R/Wyscout/eventing_br.csv")
events_br$league <- "Brasileirao"
events <- rbind(events, events_br)

rm(events_br)

#Minutos Jogados
minutos_jugados <- events %>% select(id, matchId, team.id, team.name, opponentTeam.id, opponentTeam.name,player.id, player.name, matchTimestamp)
minutos_jugados$matchTimestamp2 <- as_hms(minutos_jugados$matchTimestamp)
minutos_jugados <- minutos_jugados %>%
  group_by(matchId ,player.id, player.name, team.id, team.name, opponentTeam.id, opponentTeam.name,) %>%
  summarise(min_min = min(matchTimestamp2) , max_min = max(matchTimestamp2))
minutos_jugados$minutos <- as.numeric((minutos_jugados$max_min - minutos_jugados$min_min)/60)
minutos_jugados <- minutos_jugados %>% filter(player.name != "0" & minutos > 0) %>% select(-min_min, -max_min)

#Amplitude e Profundidade
posicao_media <- minutos_jugados %>%
  filter(minutos > 40)

posicao_media <- inner_join(events, posicao_media, by = c("matchId" = "matchId", "player.id" = "player.id")) 

posicao_media <- posicao_media %>%
  filter(player.position != "GK" & type.primary == "pass" & pass.accurate == TRUE & pass.recipient.name != "NA" & pass.recipient.name != "0") %>%
  group_by(matchId, team.name.x, player.name.x) %>%
  summarise(x.medio = mean(location.x), y.medio = mean(location.y))

posicao_media <- posicao_media %>%
  group_by(matchId, team.name.x) %>%
  summarise(x.max = max(x.medio), x.min = min(x.medio), y.max = max(y.medio), y.min = min(y.medio))

posicao_media <- posicao_media %>%
  group_by(team.name.x) %>%
  summarise(x.max = mean(x.max), x.min = mean(x.min), y.max = mean(y.max), y.min = mean(y.min))

posicao_media$amplitude <- (posicao_media$y.max - posicao_media$y.min) * 0.68
posicao_media$profundidade <- (posicao_media$x.max - posicao_media$x.min) * 1.05

#Centroides Ofensivas e Defensivas
centroide_ofensiva <- events %>%
  filter(player.position != "GK" & type.primary == "pass" & pass.accurate == TRUE & pass.recipient.name != "NA") %>%
  group_by(team.name) %>%
  summarise(centroide_o_x = mean(pass.endLocation.x), centroide_o_y = mean(pass.endLocation.y))

centroide_defensiva <- events %>%
  filter(player.position != "GK" & (groundDuel.duelType == "defensive_duel" |
                                      type.primary == "interception")) %>%
  group_by(team.name) %>%
  summarise(centroide_d_x = mean(location.x), centroide_d_y = mean(location.y))
  
#PPDA
ppda_passes <- events %>%
  filter(type.primary == "pass" & pass.accurate == TRUE & pass.recipient.name != "NA" & location.x <= 3/5*100) %>%
  group_by(opponentTeam.name) %>%
  summarise(passes = n())

ppda_acoes <- events %>%
  filter(location.x >= 3/5*100 & (groundDuel.duelType == "defensive_duel" | type.primary == "interception")) %>%
  group_by(team.name) %>%
  summarise(acoes = n())

ppda <- left_join(ppda_passes, ppda_acoes, 
                  by = c('opponentTeam.name' = 'team.name'))
ppda$ppda <- ppda$passes/ppda$acoes

# pases_local <- eventing %>%
#   filter(type.primary %in% c("interception", "pass"), pass.accurate %in% c("TRUE", "FALSE"), location.x <= 60)
# 
# duelos_defensivos_local <- eventing %>%
#   filter((groundDuel.duelType == "defensive_duel", groundDuel.recoveredPossession == TRUE, location.x >=40) | 
#            (type.primary == "interception" | str_detect(type.secondary,"interception"), location.x >=40) |
#            (type.primary== "infraction", str_detect(type.secondary,"foul"), location.x >=40) |
#            (str_detect(type.secondary, "sliding_tackle"), location.x >=40))

#Objetividade
progressive_distance <- events %>%
  filter((pass.endLocation.x != "NA" & type.primary == "pass" & pass.accurate == "TRUE")
         & (pass.endLocation.x != 0 & pass.endLocation.y != 0)) %>%
  select(id, team.id, team.name, player.id, player.name,
         location.x, location.y, pass.endLocation.x, pass.endLocation.y)

progressive_distance$location.x <- progressive_distance$location.x * 1.05
progressive_distance$location.y <- progressive_distance$location.y * 0.68
progressive_distance$pass.endLocation.x <- progressive_distance$pass.endLocation.x * 1.05
progressive_distance$pass.endLocation.y <- progressive_distance$pass.endLocation.y * 0.68

progressive_distance$pass_distance <- sqrt((progressive_distance$pass.endLocation.x - progressive_distance$location.x)^2 + (progressive_distance$pass.endLocation.y - progressive_distance$location.y)^2) 
progressive_distance$progressive_distance <- sqrt((progressive_distance$location.x - 100)^2 + (progressive_distance$location.y - 50)^2) - sqrt((progressive_distance$pass.endLocation.x - 100)^2 + (progressive_distance$pass.endLocation.y - 50)^2)
progressive_distance[progressive_distance < 0] <- 0  

progressive_distance <- progressive_distance %>%
  group_by(team.name) %>%
  summarise(pass_distance = sum(pass_distance), progressive_distance = sum(progressive_distance))

progressive_distance$progressive <- progressive_distance$progressive_distance / progressive_distance$pass_distance

#xG

xG <- events %>%
  group_by(matchId, team.name) %>%
  summarise(xG = sum(shot.xg)) %>%
  group_by(team.name) %>%
  summarise(xG = sum(xG), matches = n())

xG$xG_per_match <- xG$xG / xG$matches

xGA <- events %>%
  group_by(matchId, opponentTeam.name) %>%
  summarise(xGA = sum(shot.xg)) %>%
  group_by(opponentTeam.name) %>%
  summarise(xGA = sum(xGA), matches = n())

xGA$xGA_per_match <- xGA$xGA / xGA$matches

xG <- left_join(xG %>% select(1,4), xGA %>% select(1,4), by = c("team.name" = "opponentTeam.name"))

#Possession

possession <- unique(events %>% filter(possession.team.name != "0") %>% select(matchId, possession.team.name, possession.id, possession.duration)) %>%
  group_by(matchId, possession.team.name) %>% summarise(poss_time = sum(possession.duration))
possession_aux <- possession %>% group_by(matchId) %>% summarise(poss_match = sum(poss_time))
possession <- left_join(possession, possession_aux, by = c("matchId" = "matchId")) %>%
  group_by(possession.team.name) %>% summarise(poss_time = sum(poss_time), poss_match = sum(poss_match))
possession$poss <-  round(possession$poss_time / possession$poss_match * 100,1)

#Junção

teams_similarity <- left_join(centroide_defensiva, centroide_ofensiva)
teams_similarity <- left_join(teams_similarity, posicao_media %>% select(1, 6, 7), by = c("team.name" = "team.name.x"))
teams_similarity <- left_join(teams_similarity, ppda %>% select(1,4), by = c("team.name" = "opponentTeam.name"))
teams_similarity <- left_join(teams_similarity, progressive_distance %>% select(1,4))
teams_similarity <- left_join(teams_similarity, xG)
teams_similarity <- left_join(teams_similarity, possession %>% select(possession.team.name, poss), by = c("team.name" = "possession.team.name"))


#Modelo Similaridade

team <- "Newcastle United"
team_2 <- "Newcastle United"
teste <- teams_similarity
teste <- normalize(teste[2:11], method="standardize")
teste <- cbind(teams_similarity$team.name, teste)
teste <- teste %>%
  select(-centroide_d_x, - centroide_d_y, -centroide_o_y, -xG_per_match, -xGA_per_match)

teste[nrow(teste) + 1,] <- list("Max.", max(teste$centroide_o_x), max(teste$amplitude), max(teste$profundidade), max(teste$ppda), max(teste$progressive))
teste[nrow(teste) + 1,] <- list("Min.", min(teste$centroide_o_x), min(teste$amplitude), min(teste$profundidade), min(teste$ppda), min(teste$progressive))

teste <- teste %>%
  filter(`teams_similarity$team.name` %in% c(team, team_2, "Min.", "Max."))

teste <- teste[match(c("Max.", "Min.", team, team_2), teste$`teams_similarity$team.name`),]

similarity <- 0

for (i in 2:ncol(teste)){
  aux <- 1/(ncol(teste)-1) * (1 - abs(teste[3,i] - teste[4,i]) / abs(teste[1,i] - teste[2,i]))
  similarity <- similarity + aux
  print(similarity)
}

#Gráfico

plot <- teams_similarity
plot[nrow(plot) + 1,] <- list("Max.", max(plot$centroide_d_x), max(plot$centroide_d_y), max(plot$centroide_o_x), max(plot$centroide_o_y), min(plot$amplitude), min(plot$profundidade), min(plot$ppda), max(plot$progressive), max(plot$xG_per_match), min(plot$xGA_per_match), max(plot$poss))
plot[nrow(plot) + 1,] <- list("Min.", min(plot$centroide_d_x), min(plot$centroide_d_y), min(plot$centroide_o_x), min(plot$centroide_o_y), max(plot$amplitude), max(plot$profundidade), max(plot$ppda), min(plot$progressive), min(plot$xG_per_match), max(plot$xGA_per_match), min(plot$poss))

plot <- plot %>%
  filter(team.name %in% c(team, team_2, "Min.", "Max."))

plot <- plot[match(c("Max.", "Min.", team, team_2), plot$team.name),]

plot <- plot[, c("team.name", "ppda", "xG_per_match", "centroide_o_x", "xGA_per_match", "centroide_d_y", "centroide_o_y", "centroide_d_x", "amplitude", "profundidade", "progressive", "poss")]

areas <- c(rgb(0/255, 97/255, 60/255, 0.25),
           rgb(108/255, 171/255, 221/255, 0.25))

line <- c(rgb(135/255, 10/255, 40/255),
         rgb(108/255, 171/255, 221/255))

par(bg = "white")
radarchart(plot %>% select(-team.name, -centroide_d_y, -centroide_o_y),
           cglty = 1,         # Grid line type
           cglcol = "grey85", # Grid line color
           pcol = line,       # Color for each line
           plwd = 3,          # Width for each line
           plty = 1,          # Line type for each line
           pfcol = areas,     # Color of the areas
           vlabels = c("-PPDA", "xG", "Offensive \n Centroid", "-xGA", "Defensive \n Centroid", "-Width", "-Depth", "Objectivity", "Poss%"),
           title = paste(team," vs ", team_2,": \n", round(similarity*100, digits = 0) ,"%",  sep = ""),
           cex.main = 2       # Title font Size
           )   
#cex.main - Title font size 
#vlcex	- Font size magnification for vlabels. If NULL, the font size is fixed at text()'s default. Default NULL.





#teste

correlation <- cor(teams_similarity %>% select(-team.name))

#Possession

possession <- unique(events %>% filter(possession.team.name != "0") %>% select(matchId, possession.team.name, possession.id, possession.duration)) %>%
  group_by(matchId, possession.team.name) %>% summarise(poss_time = sum(possession.duration))
possession_aux <- possession %>% group_by(matchId) %>% summarise(poss_match = sum(poss_time))
possession <- left_join(possession, possession_aux, by = c("matchId" = "matchId")) %>%
  group_by(possession.team.name) %>% summarise(poss_time = sum(poss_time), poss_match = sum(poss_match))
possession$poss <-  possession$poss_time / possession$poss_match