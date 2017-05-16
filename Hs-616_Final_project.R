library(ggplot2)
case_generatedata <- function(N){
  age <- round(rnorm(N,mean = 28.4, sd = 4.3))
  bodymassindex <- round(rnorm(N, mean = 27.1, sd = 4),2)
  Season_mlb_experience <- abs(round(rnorm(N, mean = 9.14, sd = 4.75)))
  Pitching_style <-  sample(c('overhead','sidearm'),N , replace = TRUE) #0 overhead 1 sidearm
  Pitching_position <- sample(c('starter', 'middle reliever', 'closer'),N, prob=c(0.399,0.493,0.108),replace = TRUE) #0 starter 1 middle reliever 2 closer
  Side_of_injury <- sample(c('right', 'left'),N, replace = TRUE) #0 right #1 left
  Strikeouts_per_season_before <- abs(round(rnorm(N, mean = 58.2 , sd = 36.9),2))  
  Games_played_per_season_before <- abs(round(rnorm(N, mean = 27.3 , sd = 15.2),2)) 
  Innings_pitched_per_game_before <- abs(round(rnorm(N, mean = 3.27 , sd = 2.13),2))
  Innings_pitched_per_season_before <- abs(round(rnorm(N, mean = 77.4 , sd = 51.7),2)) 
  Hits_allowed_per_season_before <- abs(round(rnorm(N, mean = 77.1 , sd = 51.5),2)) 
  Wins_per_season_before <- abs(round(rnorm(N, mean = 4.46 , sd = 3.38),2)) 
  Losses_per_season_before <- abs(round(rnorm(N, mean = 4.40 , sd = 3.05),2))
  ERA_per_season_before <- abs(round(rnorm(N, mean = 5.67 , sd = 4.01),2))
  Complete_games_per_season_before <- abs(round(rnorm(N, mean = 0.525 , sd = 0.985),2))
  Shutouts_per_season_before <- abs(round(rnorm(N, mean = 0.198 , sd = 0.426),2))
  Runs_allowed_per_season_before <- abs(round(rnorm(N, mean = 39.4 , sd = 26.0),2))
  Home_runs_allowed_per_season_before <- abs(round(rnorm(N, mean = 8.73 , sd = 6.07),2))
  Walks_allowed_per_season_before <- abs(round(rnorm(N, mean = 30.3 , sd = 19.8),2))
  Strikeouts_per_season_after <- abs(round(rnorm(N, mean = 45.6 , sd = 38.0),2))  
  Game_played_after_surgery_after <- abs(round(rnorm(N, mean = 27.1 , sd = 16.7),2))
  Innings_pitched_per_game_after <- abs(round(rnorm(N, mean = 2.67 , sd = 1.91),2))
  Innings_pitched_per_season_after <- abs(round(rnorm(N, mean = 58.7 , sd = 47.2),2)) 
  Hits_allowed_per_season_after <- abs(round(rnorm(N, mean = 57.8 , sd = 47.4),2))
  Wins_per_season_after <- abs(round(rnorm(N, mean = 3.33 , sd = 3.1),2))
  Losses_per_season_after <- abs(round(rnorm(N, mean = 3.08 , sd = 2.72),2))
  ERA_per_season_after <- abs(round(rnorm(N, mean = 4.18 , sd = 1.36),2))
  Complete_games_per_season_after <- abs(round(rnorm(N, mean = 0.224 , sd = 0.529),2))
  Shutouts_per_season_after <- abs(round(rnorm(N, mean = 0.091 , sd = 0.247),2))
  Runs_allowed_per_season_after <- abs(round(rnorm(N, mean = 30.2 , sd = 23.5),2))
  Home_runs_allowed_per_season_after <- abs(round(rnorm(N, mean = 6.7 , sd = 5.21),2))
  Walks_allowed_per_season <- abs(round(rnorm(N, mean = 21.6 , sd = 15.7),2))
  data.frame(age,bodymassindex,Season_mlb_experience,Pitching_style,Pitching_position,Side_of_injury,Strikeouts_per_season_before,
             Games_played_per_season_before,Innings_pitched_per_game_before,Innings_pitched_per_season_before,Hits_allowed_per_season_before,
             Wins_per_season_before,Losses_per_season_before,ERA_per_season_before,Complete_games_per_season_before,
             Shutouts_per_season_before,Runs_allowed_per_season_before,Home_runs_allowed_per_season_before,Walks_allowed_per_season_before,
             Strikeouts_per_season_after,Game_played_after_surgery_after,Innings_pitched_per_game_after,Innings_pitched_per_season_after,Hits_allowed_per_season_after,
             Wins_per_season_after,Losses_per_season_after,ERA_per_season_after,Complete_games_per_season_after,Shutouts_per_season_after,
             Runs_allowed_per_season_after,Home_runs_allowed_per_season_after,Walks_allowed_per_season)
}

control_generatedata <- function(N){
  age <- round(rnorm(N,mean = 28.3, sd = 4.23))
  bodymassindex <- round(rnorm(N, mean = 27.1, sd = 4),2)
  Season_mlb_experience <- abs(round(rnorm(N, mean = 8.74, sd = 3.84)))
  Pitching_style <-  sample(c('overhead','sidearm'),N , replace = TRUE) #0 overhead 1 sidearm
  Pitching_position <- sample(c('starter', 'middle reliever', 'closer'),N, prob=c(0.392,0.5,0.108),replace = TRUE) #0 starter 1 middle reliever 2 closer
  Side_of_injury <- sample(c('right', 'left'),N, replace = TRUE) #0 right #1 left
  Strikeouts_per_season_before <- abs(round(rnorm(N, mean = 62.3 , sd = 44.7),2))  
  Games_played_per_season_before <- abs(round(rnorm(N, mean = 25.9 , sd = 13.7),2)) 
  Innings_pitched_per_game_before <- abs(round(rnorm(N, mean = 3.8 , sd = 1.96),2))
  Innings_pitched_per_season_before <- abs(round(rnorm(N, mean = 85.3 , sd = 56.1),2)) 
  Hits_allowed_per_season_before <- abs(round(rnorm(N, mean = 86.2 , sd = 53.2),2)) 
  Wins_per_season_before <- abs(round(rnorm(N, mean = 5.14 , sd = 3.6),2)) 
  Losses_per_season_before <- abs(round(rnorm(N, mean = 4.57 , sd = 3.09),2))
  ERA_per_season_before <- abs(round(rnorm(N, mean = 5.66 , sd = 2.73),2))
  Complete_games_per_season_before <- abs(round(rnorm(N, mean = 0.625 , sd = 1.7),2))
  Shutouts_per_season_before <- abs(round(rnorm(N, mean = 0.202 , sd = 0.459),2))
  Runs_allowed_per_season_before <- abs(round(rnorm(N, mean = 39.5 , sd = 25.1),2))
  Home_runs_allowed_per_season_before <- abs(round(rnorm(N, mean = 10.1 , sd = 6.75),2))
  Walks_allowed_per_season_before <- abs(round(rnorm(N, mean = 33.1 , sd = 20.7),2))
  Strikeouts_per_season_after <- abs(round(rnorm(N, mean = 47.4 , sd = 38.9),2))  
  Game_played_after_surgery_after <- abs(round(rnorm(N, mean = 24.2 , sd = 16.7),2))
  Innings_pitched_per_game_after <- abs(round(rnorm(N, mean = 3.12 , sd = 1.88),2))
  Innings_pitched_per_season_after <- abs(round(rnorm(N, mean = 65.3 , sd = 51.7),2)) 
  Hits_allowed_per_season_after <- abs(round(rnorm(N, mean = 69.8 , sd = 49.4),2))
  Wins_per_season_after <- abs(round(rnorm(N, mean = 2.98 , sd = 2.75),2))
  Losses_per_season_after <- abs(round(rnorm(N, mean = 4.3 , sd = 2.94),2))
  ERA_per_season_after <- abs(round(rnorm(N, mean = 6.36 , sd = 3.31),2))
  Complete_games_per_season_after <- abs(round(rnorm(N, mean = 0.175 , sd = 0.335),2))
  Shutouts_per_season_after <- abs(round(rnorm(N, mean = 0.069 , sd = 0.164),2))
  Runs_allowed_per_season_after <- abs(round(rnorm(N, mean = 37.8 , sd = 26.0),2))
  Home_runs_allowed_per_season_after <- abs(round(rnorm(N, mean = 8.71 , sd = 6.54),2))
  Walks_allowed_per_season <- abs(round(rnorm(N, mean = 25.7 , sd = 16.4),2))
  data.frame(age,bodymassindex,Season_mlb_experience,Pitching_style,Pitching_position,Side_of_injury,Strikeouts_per_season_before,
             Games_played_per_season_before,Innings_pitched_per_game_before,Innings_pitched_per_season_before,Hits_allowed_per_season_before,
             Wins_per_season_before,Losses_per_season_before,ERA_per_season_before,Complete_games_per_season_before,
             Shutouts_per_season_before,Runs_allowed_per_season_before,Home_runs_allowed_per_season_before,Walks_allowed_per_season_before,
             Strikeouts_per_season_after,Game_played_after_surgery_after,Innings_pitched_per_game_after,Innings_pitched_per_season_after,Hits_allowed_per_season_after,
             Wins_per_season_after,Losses_per_season_after,ERA_per_season_after,Complete_games_per_season_after,Shutouts_per_season_after,
             Runs_allowed_per_season_after,Home_runs_allowed_per_season_after,Walks_allowed_per_season)
}

set.seed(123)
case_tjdataset <- case_generatedata(179)
control_tjdataset <- control_generatedata(179)
case_tjdataset <- case_tjdataset[order(case_tjdataset$age),] 
control_tjdataset <- control_tjdataset[order(control_tjdataset$age),] 

ggplot(tjdataset, aes(x = Innings_pitched_per_game_before, y = Strikeouts_per_season_before, color = Pitching_style))+
  geom_point(shape=1)+
  ggtitle("Innings v.s strikeouts (before)") +
  labs(x="Innings per game",y="Strikeouts per season") 

ggplot(tjdataset, aes(x = Innings_pitched_per_game_after, y = Strikeouts_per_season_after, color = Pitching_style))+
  geom_point(shape=1)+
  ggtitle("Innings v.s strikeouts (after)") +
  labs(x="Innings per game",y="Strikesout per season") 

ggplot(tjdataset, aes(x = Innings_pitched_per_season_before, y = Strikeouts_per_season_before, colour = 'blue')) + 
  geom_point() + geom_smooth(method = "lm")+
  ggtitle("Innings v.s strikeouts") +
  labs(x="Innings pitched per game",y="Strikesouts per season")
    
  ggplot(tjdataset, aes(x = Innings_pitched_per_season_after, y = Strikeouts_per_season_after, colour = 'blue')) + 
    geom_point() + geom_smooth(method = "lm")
    
    geom_point(aes(x = Innings_pitched_per_season_after, y = Strikeouts_per_season_after, colour = 'red')) +
    ggtitle("Innings v.s strikeouts") +
    labs(x="Innings pitched per game",y="Strikesouts per season") 

#######inning v.s hits
ggplot(tjdataset) + 
  geom_point(aes(x = Innings_pitched_per_season_before, y = Hits_allowed_per_season_before, colour = "9999CC")) + 
  geom_point(aes(x = Innings_pitched_per_season_after, y = Hits_allowed_per_season_after, colour = "CC6666")) +
  ggtitle("Innings v.s Hits") +
  labs(x="Innings pitched per game",y="Hits allowed per season") +
  scale_color_discrete(name ="Tommy Surgey", 
                       labels=c("before", "after"))  

######inning v.s strikeouts
ggplot(tjdataset) + 
  geom_point(aes(x = Innings_pitched_per_season_before, y = Strikeouts_per_season_before,colour = '9999CC')) + 
  geom_point(aes(x = Innings_pitched_per_season_after, y = Strikeouts_per_season_after,colour = 'CC6666')) +
  ggtitle("Innings v.s Strikeouts") +
  labs(x="Innings pitched per game",y="Strikeouts allowed per season") +
  scale_color_discrete(name ="Tommy Surgey", 
                       labels=c("before", "after"))

#############
case_tjdatasetfit <- case_tjdataset
control_tjdatasetfit <- control_tjdataset

#############
#Case
case_tjdatasetfit$Pitching_style <- ifelse(case_tjdatasetfit$Pitching_style == 'overhead',0,1)
case_tjdatasetfit$Pitching_position <- ifelse(case_tjdatasetfit$Pitching_position == 'starter',0,(ifelse(case_tjdatasetfit$Pitching_position == 'middle reliever' ,1,2)))
case_tjdatasetfit$Side_of_injury <- ifelse(case_tjdatasetfit$Side_of_injury == 'right',0,1)

###########################
#Control
control_tjdatasetfit$Pitching_style <- ifelse(control_tjdatasetfit$Pitching_style == 'overhead',0,1)
control_tjdatasetfit$Pitching_position <- ifelse(control_tjdatasetfit$Pitching_position == 'starter',0,(ifelse(control_tjdatasetfit$Pitching_position == 'middle reliever' ,1,2)))
control_tjdatasetfit$Side_of_injury <- ifelse(control_tjdatasetfit$Side_of_injury == 'right',0,1)


################


pitching_style_subset_before <- tjdataset[12:24]
cor(tjdataset$Pitching_style,pitching_style_subset_before)
cor(tjdataset$Pitching_position,pitching_style_subset_before)
cor(tjdataset$Side_of_injury,pitching_style_subset_before)

pitching_style_subset_after <- tjdataset[25:37]
cor(tjdataset$Pitching_style,pitching_style_subset_after)
cor(tjdataset$Pitching_position,pitching_style_subset_after)
cor(tjdataset$Side_of_injury,pitching_style_subset_after)

### t test
t.test(case_tjdataset$Innings_pitched_per_season_after,control_tjdataset$Innings_pitched_per_season_after)
t.test(tjdataset$Complete_games_per_season_before,tjdataset$Complete_games_per_season_after)
t.test(tjdataset$Runs_allowed_per_season_before,tjdataset$Runs_allowed_per_season_after)


ggplot(tjdataset) + 
  geom_point(aes(x = age, y = Innings_pitched_per_season_after,color = Pitching_style))

#### Pitching_style model
fit_pitching_style_after_all <- lm(tjdatasetfit$Pitching_style ~ tjdatasetfit$Innings_pitched_per_season_after + tjdatasetfit$Losses_per_season_after + tjdatasetfit$Runs_allowed_per_season_after, data = tjdatasetfit)
summary(fit_pitching_style_after_all)
#plot(fit_pitching_style_after_all)

fit_pitching_style_after_inn_runs <- lm(tjdatasetfit$Pitching_style ~ tjdatasetfit$Innings_pitched_per_season_after + tjdatasetfit$Runs_allowed_per_season_after, data = tjdatasetfit)
summary(fit_pitching_style_after_inn_runs)

fit_pitching_style_after_inn <- lm(tjdatasetfit$Pitching_style ~ tjdatasetfit$Innings_pitched_per_season_after, data = tjdatasetfit)
summary(fit_pitching_style_after_inn)


#### Pitching_position model
fit_pitching_position_after_all <- lm(tjdatasetfit$Pitching_position ~ tjdatasetfit$Innings_pitched_per_season_after + tjdatasetfit$Losses_per_season_after + tjdatasetfit$Runs_allowed_per_season_after, data = tjdatasetfit)
summary(fit_pitching_position_after_all)

fit_pitching_position_after_inn_runs <- lm(tjdatasetfit$Pitching_position ~ tjdatasetfit$Innings_pitched_per_season_after + tjdatasetfit$Runs_allowed_per_season_after, data = tjdatasetfit)
summary(fit_pitching_position_after_inn_runs)


fit_pitching_position_after_inn <- lm(tjdatasetfit$Pitching_position ~ tjdatasetfit$Innings_pitched_per_season_after, data = tjdatasetfit)
summary(fit_pitching_position_after_inn)

### Side_of_injury model
fit_side_injury_after_all <- lm(tjdatasetfit$Side_of_injury ~ tjdataset$Innings_pitched_per_season_after + tjdataset$Losses_per_season_after + tjdataset$Runs_allowed_per_season_after, data = tjdataset)
summary(fit_side_injury_after_all)

fit_side_injury_after_inn_runs <- lm(tjdatasetfit$Side_of_injury ~ tjdataset$Innings_pitched_per_season_after + tjdataset$Runs_allowed_per_season_after, data = tjdataset)
summary(fit_side_injury_after_inn_runs)

fit_side_injury_after_inn <- lm(tjdatasetfit$Side_of_injury ~ tjdataset$Innings_pitched_per_season_after , data = tjdataset)
summary(fit_side_injury_after_inn)

