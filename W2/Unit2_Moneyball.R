# VIDEO 2
rm (list = ls(all=T))

# Read in data
baseball = read.csv("baseball.csv")
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# Scatterplot to check for linear relationship
plot(moneyball$RD, moneyball$W)

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

# Quick Question
# If a baseball team scores 713 runs and allows 614 runs, how many games do we expect the team to win?
# Using the linear regression model constructed during the lecture, enter the number of games we expect the team to win:
qq = data.frame(RD = 99)
predict(WinsReg, newdata =  qq)
# VIDEO 3

str(moneyball)

# Regression model to predict runs scored
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)


# Quick Question
#If a baseball team's OBP is 0.311 and SLG is 0.405, how many runs do we expect the team to score?
predict(RunsReg, newdata = data.frame(OBP = 0.311, SLG = 0.405))
# If a baseball team's opponents OBP (OOBP) is 0.297 and oppenents SLG (OSLG) is 0.370, how many runs do we expect the team to allow?
RunsAll = lm(RA ~ OOBP + OSLG, data = moneyball)
summary(RunsAll)
predict (RunsAll, newdata = data.frame(OOBP = 0.297, OSLG = 0.370))

# Quick Question
#Suppose you are the General Manager of a baseball team, and you are selecting TWO players for your team. 
#You have a budget of $1,500,000, and you have the choice between the following players:

players = read.csv("players.csv", sep = "-")
predict(RunsReg, newdata = players)

