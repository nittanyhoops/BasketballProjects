library(tidyverse)
library(ggplot2)
library(ncaahoopR) #win prob charts
library(gamezoneR) #shot charts
library(hoopR)

psu_schedule <- gamezoneR::gamezone_mbb_team_schedule(team = "Penn State", season = "2022-23") #shot types using gamezone

psu_singlegame_pbp <- gamezoneR::gamezone_mbb_pbp(psu_schedule$game_id[3],sub_parse = T)

shot_types_by_game <- psu_singlegame_pbp %>% 
  group_by(event_team, three_pt) %>%
  count(shot_desc) %>% 
  filter(!is.na(shot_desc))

shot_types_by_game$shot_type <- paste(shot_types_by_game$three_pt, shot_types_by_game$shot_desc)

shot_types_by_game$shot_type[shot_types_by_game$shot_type=="TRUE Jump Shot"] <- "3 Pointer"
shot_types_by_game$shot_type[shot_types_by_game$shot_type=="FALSE Jump Shot"] <- "2 Pt Jumper"
shot_types_by_game$shot_type[shot_types_by_game$shot_type=="FALSE Layup Shot"] <- "Rim"
shot_types_by_game$shot_type[shot_types_by_game$shot_type=="FALSE Hook Shot"] <- "Rim"
shot_types_by_game$shot_type[shot_types_by_game$shot_type=="FALSE Dunk Shot"] <- "Rim"
shot_types_by_game$shot_type[shot_types_by_game$shot_type=="FALSE Tip Shot"] <- "Rim"


ggplot(data=shot_types_by_game, aes(x=shot_type, y=n, fill=event_team)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  scale_fill_manual("legend", values = c("Penn State" = "#041E42", "Butler" = "#747678"))+
  coord_flip()+
  labs(xlab = "Shot Type",
       ylab = "Total",
       title = "Shot Attempts by Type")
