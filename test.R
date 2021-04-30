source("Mars.R")
source("summary.R")
source("print.R")
source("plot.R")
source("Predict.R")
source("anova.R")
library(MASS)
library(ISLR)

data(Wage)
mc <- mars.control(Mmax = 10)
mout <- mars(wage ~ age + education, data=Wage, control=mc)
ff <- fitted(mout)
p1 <- predict(mout)
p2 <- predict(mout,newdata=data.frame(age=Wage$age,education=Wage$education))
head(cbind(ff,p1,p2)) # columns should be identical
mout # tests print method
summary(mout) #test summary method
anova(mout) # test anova method
plot(mout) # test plot method


#test Weather dataset
s <- read.csv("weather.csv")
mc <- mars.control(Mmax = 10)
form <-as.formula("MXSPD ~ GUST + MAX")
fa <- mars(formula = form, data = s, control = mc)
ff <- fitted(fa)
p1 <- predict(fa)
p2 <- predict(fa,newdata=data.frame(GUST=s$GUST,MAX=s$MAX))
head(cbind(ff,p1,p2))
anova.mars(fa)
plot.mars(fa)
summary.mars(fa)
print.mars(fa)



#Test from NBAPlayerStatistics dataset
library(SportsAnalytics)
s <- fetch_NBAPlayerStatistics(season = "09-10", what = c("", ".Home", ".Away"))
mc <- mars.control(Mmax = 10)
form <- as.formula("FieldGoalsMade ~ TotalPoints + Assists")
fa <- mars(formula = form, data = s, control = mc)
ff <- fitted(fa)
p1 <- predict(fa)
p2 <- predict(fa,newdata=data.frame(TotalPoints= s$TotalPoints, Assists = s$Assists))
head(cbind(ff,p1,p2))
anova.mars(fa)
plot.mars(fa)
summary.mars(fa)
print.mars(fa)




