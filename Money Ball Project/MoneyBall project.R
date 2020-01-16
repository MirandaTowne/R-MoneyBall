library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

batting <- read.csv('Money-Ball-Data-csv-files\\Batting.csv')
salaries <- read.csv('Money-Ball-Data-csv-files\\Salaries.csv')

batting$avg <- batting$H / batting$AB
batting$obp <- (batting$H + batting$BB + batting$HBP) / (batting$AB + batting$BB + batting$HBP + batting$SF)
batting$X1B <- batting$H - (batting$HR + batting$X2B + batting$X3B)
batting$slg <- ((batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR)) / batting$AB
batting$Old.Salaries <- sum(lost_players$salary)
batting$New.Salaries <- sum(new_players$salary)
batting$Salary.Diff <- batting$New.Salaries - batting$Old.Salaries

batting <- subset(batting, yearID >= 1985)
combo <- merge(batting, salaries, c('playerID','yearID'))
lost_players <- subset(combo, playerID %in% c('giambja01','damonjo01','saenzol01'))
lost_players <- subset(lost_players, yearID == 2001)
lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','obp','slg','avg','AB','salary','Old.Salaries','New.Salaries','Salary.Diff')]
replacement_players <- subset(combo, yearID == 2001 & obp >= 0.3638687 & AB >= 305 )
replacement_players <- replacement_players[,c('playerID','H','X2B','X3B','HR','obp','slg','avg','AB','salary','Old.Salaries','New.Salaries','Salary.Diff')]
new_players <- subset(replacement_players, playerID %in% c('berkmla01','bondsba01','pujolal01'))
new_players <- new_players[,c('playerID','H','X2B','X3B','HR','obp','slg','avg','AB','salary','Old.Salaries','New.Salaries','Salary.Diff')]
compare.players <- rbind(new_players,lost_players)


print(compare.players)

l_pplot <- ggplot(replacement_players,aes(salary,obp)) + geom_jitter(aes(color=playerID),size=3,shape=1)
l_pplot <- l_pplot + geom_smooth()


compare.players.plot <- ggplot(compare.players,aes(salary,obp)) + geom_point(aes(color=playerID),size=3,shape=1)
compare.players.plot <- compare.players.plot + scale_y_continuous(name = 'OBP',limits = c(0,0.7))


print(ggplotly(compare.players.plot))

