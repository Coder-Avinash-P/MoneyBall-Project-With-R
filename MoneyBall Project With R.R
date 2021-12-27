#MoneyBall Project

batting <- read.csv("C:\\Users\\admin\\Desktop\\Online courses\\Udemy\\6. Data Science & ML with R\\Github Projects\\Batting.csv")

#To check out the data
head(batting)

#To check structure of the data
str(batting)

#To get heads of the selected (coded) variables
head(batting[, 8:24])

#We need to create 3 more statistics that are being used:
#Batting Average(BA)      
#On Base Percentage(OBP)
#Slugging Percentage(SLG)

#Create Batting Average (AVG=H/AB)
batting$BA <- batting$H / batting$AB

tail(batting$BA,5)

#Create On Base Percentage (H+BB+HBP/AB+BB+HBP+SF)
batting$OBP <-(batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP +batting$SF)

tail(batting$OBP,5)

#Create X1B (Singles)
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

#Create Slugging Percentage (1B+2*2B+3*3B+4*4B+4*HR/AB)
batting$SLG <-((1*batting$X1B) + (2*batting$X2B) + (3*batting$X3B) + (4*batting$HR)) / batting$AB

tail(batting$SLG,5)

#Merging Salary Data with Batting Data

slry <- read.csv("C:\\Users\\admin\\Desktop\\Online courses\\Udemy\\6. Data Science & ML with R\\Github Projects\\Salaries.csv")

head(slry)

summary(batting)

#Slary(slry) data starts from 1985, but the batting data from 1871. We only need data from 1985 and onward

#Create a subset of batting data

batting <- subset(batting, yearID >= 1985)

summary(batting)

#Now, we get that year started from 1985

#Merge the batting data with salary data by player and year

bat_sal <- merge(batting, slry, by=c('playerID', 'yearID'))

summary(bat_sal)

#As we know, Oakland lost 3 main players during 2001 off-season. These were: Jason Giambi, Johnny Damon, and Olmedo Saenz

#Make a subset of lost players

lost_players <- subset(bat_sal, playerID %in% c('giambja01','damonjo01','saenzol01'))

lost_players

#Lost players in 2001
lost_players <- subset(lost_players, yearID == 2001)

#Reduce the lost_players data frame to certain columns
lost_players <- lost_players[,c('playerID', 'H', 'X2B', 'X3B', 'HR', 'OBP', 'SLG', 'BA', 'AB')]

head(lost_players)

#Replacement of the lost 3 players with new best players on 3 constraints (salary, AB and OBP )

#To replace, first find available players from year 2001
library(dplyr)
avail.players <- filter(bat_sal, yearID==2001)

#Plot the available players(as salary vs OBP)
library(ggplot2)

ggplot(avail.players, aes(x=OBP, y=salary)) + geom_point(color='green')

#Remove the players which salary is more than 8 million and OBP is zero
avail.players <- filter(avail.players, salary<8000000,OBP>0)

#Also, find players with AB greater than 500
avail.players <- filter(avail.players, AB >= 500)

#Sort by OBP to find possible new players
poss.players <- head(arrange(avail.players, desc(OBP)), 10)

#Sort possible players only with the selection parameters(variables)
poss.players <- poss.players[,c('playerID', 'OBP', 'AB', 'salary')]
poss.players

#The 1st player in the is 'Giambi', so we choose 2nd to 4th player
poss.players[2:4,]

#Salaries of the 3 best possible players on our selected parameters
sum(poss.players[2:4,]$salary)
