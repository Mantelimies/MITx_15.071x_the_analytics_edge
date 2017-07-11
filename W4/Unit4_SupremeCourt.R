# Unit 4 - "Judge, Jury, and Classifier" Lecture


# VIDEO 4

# Read in the data
stevens = read.csv("stevens.csv")
str(stevens)

# Split the data
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)

# Install rpart library
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# CART model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=25)

prp(StevensTree)plot(StevensTree)

# Make predictions
PredictCART = predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, PredictCART)
(41+71)/(41+36+22+71)

# ROC curve
library(ROCR)

PredictROC = predict(StevensTree, newdata = Test)
PredictROC

pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)

#Quick question 
#Compute the AUC of the CART model from the previous video, using the following command in your R console:
    
as.numeric(performance(pred, "auc")@y.values)
StevensTreeQ = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=5)
prp(StevensTreeQ, tweak = 5)
install.packages("rattle")

StevensTreeQ2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=100)
prp(StevensTreeQ2, tweak = 2)


library(rattle)
fancyRpartPlot(StevensTreeQ)
fancyRpartPlot(StevensTreeQ2)

# VIDEO 5 - Random Forests

# Install randomForest package
install.packages("randomForest")
library(randomForest)

# Build random forest model
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

# Convert outcome to factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

# Try again

StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )
# Make predictions
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)

#accuracy
(40+74)/(40+37+19+74)

(43+68)/(43+34+24+69)

#Quick question. First, set the seed to 100, and the re-build the random forest model, exactly like we did in the previous video (Video 5). Then make predictions on the test set. What is the accuracy of the model on the test set?
set.seed(100)
StevensForestQuick = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )
PredictQuick = predict(StevensForestQuick, newdata = Test)
table(Test$Reverse, PredictQuick)
(43+74)/(43+34+19+74)

set.seed(200)
StevensForestQuick2 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )
PredictQuick2 = predict(StevensForestQuick2, newdata = Test)
table(Test$Reverse, PredictQuick2)
(44+76)/(44+33+17+76)

# VIDEO 6

# Install cross-validation packages
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)


# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )

cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 


# Perform the cross validation
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# Create a new CART model
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", cp = 0.18)

# Make predictions
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")
table(Test$Reverse, PredictCV)
(59+64)/(59+18+29+64)
prp(StevensTreeCV)

