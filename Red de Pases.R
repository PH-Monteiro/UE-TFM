# Red de Pases

require(jsonlite)
library(tidyverse)
library(dplyr)
library(stringr)
library(DT)
library(lubridate)
library(GET)
library(httr)

teste <- read.csv("G:/Meu Drive/Máster en Big Data/TFM/R/Wyscout/PL/PL_Events.csv")

#c("GK", "LB", "LCB", "RCB", "RB", "LDMF", "RDMF", "LAMF", "AMF", "RAMF", "CF")

posicao_media <- teste %>% 
  filter(team.name == "Newcastle United" &
           #type.primary == "pass" &
           player.position %in% c("GK", "LB", "LCB", "RCB", "RB", "DMF", "LCMF3", "RCMF3", "LWF", "RWF", "CF")) %>%
  group_by(player.position) %>%
  summarise(x.medio = mean(location.x), y.medio = mean(location.y))

quantidade_passes <- teste %>% 
  filter(team.name == "Newcastle United" &
           #type.primary == "pass" &
           player.position %in% c("GK", "LB", "LCB", "RCB", "RB", "DMF", "LCMF3", "RCMF3", "LWF", "RWF", "CF") & 
           pass.recipient.position %in% c("GK", "LB", "LCB", "RCB", "RB", "DMF", "LCMF3", "RCMF3", "LWF", "RWF", "CF")) %>%
  group_by(player.position, pass.recipient.position) %>%
  summarise(passes = n()) %>%
  filter(player.position != pass.recipient.position)

rede_passes <- left_join(quantidade_passes, posicao_media, by = c("player.position"))
rede_passes <- left_join(rede_passes, posicao_media, by = c("pass.recipient.position" = "player.position"))

df_aux <- rede_passes

rede_passes$tipo <- "passador"
df_aux$tipo <- "receptor"

rede_passes <- bind_rows(rede_passes, df_aux)

write.csv(rede_passes, "G:/Meu Drive/Máster en Big Data/TFM/R/Wyscout/PL/red_pases.csv", row.names = FALSE)
