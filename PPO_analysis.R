library(nflfastR)
library(tidyverse)
library(ggrepel)
library(ggimage)
library(furrr)
options(scipen = 9999)


#import dataframe
seasons <- 2009:2019
pbp <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

#Filter players that have x number of rush att in a season since the package doesn't provide positional values
rushers <- pbp %>% filter(play_type == 'run', qb_scramble == 0) %>% 
  group_by(rusher_player_name, season) %>% summarise(att = n()) %>% 
  filter(att >= 85)

#First half points and PPO (variable scm_player unifies receiver and rusher)
ppo_firstH <- pbp %>% filter((play_type == 'run' | play_type == 'pass') & week <= 8) %>% 
  mutate(scm_player = ifelse(is.na(receiver_player_name), rusher_player_name, receiver_player_name)) %>% 
  filter(scm_player %in% c(rushers$rusher_player_name)) %>% 
  group_by(scm_player, season) %>% summarise(opps = n(), PxTD = sum((yards_gained*0.1)+(complete_pass*0.5)), Pts = sum((yards_gained*0.1)+(complete_pass*0.5)+(touchdown*6))) %>%
  filter(opps >= 80) %>% 
  mutate(PPOxTD =PxTD/opps) %>% arrange(desc(PPOxTD))

#Second half points (ex. week 17) 
point_finish <- pbp %>% filter((play_type == 'run' | play_type == 'pass') & (week >= 8 & week <=16)) %>% 
  mutate(scm_player = ifelse(is.na(receiver_player_name), rusher_player_name, receiver_player_name)) %>% 
  filter(scm_player %in% c(rushers$rusher_player_name)) %>% 
  group_by(scm_player, season) %>% summarise(opps = n(), final_points = sum((yards_gained*0.1)+(complete_pass*0.5)+(touchdown*6))) %>% 
  filter(opps >= 80) %>% select(scm_player,season, final_points, opps)
                                             
#merge 1st and 2nd half
relation_ppo <- merge(ppo_firstH, point_finish, by = c('scm_player', 'season'))

dim(relation_ppo)

#correlation
corr <- round(cor(relation_ppo$PPOxTD, relation_ppo$final_points), 2)

#PPO week 8 vs ROS points regression and plots
PPOlm <- lm(final_points~PPOxTD,data=relation_ppo)

ggplot(data = relation_ppo, aes(PPOxTD, final_points)) + 
  geom_point(aes(colour = opps.x)) + 
  labs(y = 'Week 8+ points', x = 'PPOxTD until week 8', title = 'PPOxTD vs ROS performance', caption = paste('correlation = ', corr)) +
  geom_abline(intercept = PPOlm$coefficients[1], slope = PPOlm$coefficients[2])


summary(PPOlm)

#1st half points vs second half points
round(cor(relation_ppo$Pts, relation_ppo$final_points),2)
ggplot(data = relation_ppo, aes(PxTD, final_points)) + 
  geom_point()



