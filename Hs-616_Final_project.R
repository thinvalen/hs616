library(ggplot2)
generatedata <- function(N){
  age <- round(rnorm(N,mean = 28.4, sd = 4.3))
  bodymassindex <- round(rnorm(N, mean = 27.1, sd = 4),2)
  Total_year_played <- round(rnorm(N, mean = 5.6, sd = 2))
  Year_of_surgery <-  sample(c(2013,2014,2015),N, replace = TRUE)
  Age_surgery <- sample(c(23,24,25),N, replace = TRUE)        
  Years_after_surgery <- sample(c(2016,2017),N, replace = TRUE)      
  Months_surgery_throwing <- round(rnorm(N, mean = 11, sd = 2),2)  
  Months_pitching <- abs(round(rnorm(N, mean = 20.5, sd = 9.72),2))  
  Pitching_style <-  sample(c('overhead','sidearm'),N , replace = TRUE) #0 overhead 1 sidearm
  Pitching_position <- sample(c('starter', 'middle reliever', 'closer'),N, replace = TRUE) #0 starter 1 middle reliever 2 closer
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
  Runs_allowed_per_season_after <- abs(round(rnorm(N, mean = 0.224 , sd = 0.529),2))
  Home_runs_allowed_per_season_after <- abs(round(rnorm(N, mean = 6.7 , sd = 5.21),2))
  Walks_allowed_per_season <- abs(round(rnorm(N, mean = 21.6 , sd = 15.7),2))
  data.frame(age,bodymassindex,Total_year_played,Year_of_surgery,Age_surgery,Years_after_surgery,Months_surgery_throwing,
             Months_pitching,Pitching_style,Pitching_position,Side_of_injury,Strikeouts_per_season_before,
             Games_played_per_season_before,Innings_pitched_per_game_before,Innings_pitched_per_season_before,Hits_allowed_per_season_before,
             Wins_per_season_before,Losses_per_season_before,ERA_per_season_before,Complete_games_per_season_before,
             Shutouts_per_season_before,Runs_allowed_per_season_before,Home_runs_allowed_per_season_before,Walks_allowed_per_season_before,
             Strikeouts_per_season_after,Game_played_after_surgery_after,Innings_pitched_per_game_after,Innings_pitched_per_season_after,Hits_allowed_per_season_after,
             Wins_per_season_after,Losses_per_season_after,ERA_per_season_after,Complete_games_per_season_after,Shutouts_per_season_after,
             Runs_allowed_per_season_after,Home_runs_allowed_per_season_after,Walks_allowed_per_season)
}


set.seed(123)
tjdataset <- generatedata(200)

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

tjdataset$Pitching_style <- ifelse(tjdataset$Pitching_style == 'overhead',0,1)
tjdataset$Pitching_position <- ifelse(tjdataset$Pitching_position == 'starter',0,(ifelse(tjdataset$Pitching_position == 'middle reliever' ,1,2)))
tjdataset$Side_of_injury <- ifelse(tjdataset$Side_of_injury == 'right',0,1)


pitching_style_subset_before <- tjdataset[12:24]
cor(tjdataset$Pitching_style,pitching_style_subset_before)
cor(tjdataset$Pitching_position,pitching_style_subset_before)
cor(tjdataset$Side_of_injury,pitching_style_subset_before)

pitching_style_subset_after <- tjdataset[25:37]
cor(tjdataset$Pitching_style,pitching_style_subset_after)
cor(tjdataset$Pitching_position,pitching_style_subset_after)
cor(tjdataset$Side_of_injury,pitching_style_subset_after)



fit_pitching_style_before <- lm(tjdataset$Pitching_style ~ tjdataset$Strikeouts_per_season_before + tjdataset$ERA_per_season_before + tjdataset$Innings_pitched_per_game_before, data = tjdataset)
summary(fit_pitching_style_before)
plot(fit_pitching_style_before)

fit_pitching_style_after <- lm(tjdataset$Pitching_style ~ tjdataset$Strikeouts_per_season_after + tjdataset$ERA_per_season_after + tjdataset$Innings_pitched_per_game_after, data = tjdataset)
summary(fit_pitching_style_after)
