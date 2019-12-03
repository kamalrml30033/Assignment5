library(caret)
library(gbm)

data(scat)
scat_orig[,]


#1
scat_orig$Species<-ifelse(scat_orig$Species=='bobcat',0,ifelse(scat$Species=='coyote',1,ifelse(scat$Species=='failed',2,ifelse(scat$Species=='gray_fox',3,ifelse(scat$Species=='mixed',4,5)))))
scat_orig$ID<-NULL


#2
scat_orig<-subset(scat_orig, select = -c(Month, Year, Site, Location) )
str(scat_orig)

#3
sum(is.na(scat_orig))
preProcValues <- preProcess(scat_orig, method = c("knnImpute","center","scale"))

library('RANN')
scat_orig <- predict(preProcValues, scat_orig)
sum(is.na(scat_orig))


#4

dmy <- dummyVars(" ~ .", data = scat_orig,fullRank = T)
train_transformed <- data.frame(predict(dmy, newdata = scat_orig))
train_transformed[,]




#5

#With a seed of 100, 75% training, 25% testing . Build the following models: randomforest, 
#neuralnet, naive bayes and GBM.
#Spliting training set into two parts based on outcome: 75% and 25%
set.seed(100)
index <- createDataPartition(scat_orig$scrape, p=0.75, list=FALSE)
trainSet <- scat_orig[ index,]
testSet <- scat_orig[-index,]
#Checking the structure of trainSet
str(trainSet)

############ Variable import######### Feature selection using Caret #############

#Feature selection using rfe in caret
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)
outcomeName<-'scrape'
predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]
scrape_Pred_Profile <- rfe(trainSet[,predictors], trainSet[,outcomeName],rfeControl = control)
scrape_Pred_Profile
#Taking only the top 5 predictors
predictors<-c("CN", "segmented", "d15N", "d13C", "Mass")
names(getModelInfo())

# For example, to apply, GBM, Random forest, Neural net:
model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm', importance=T)
model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf', importance=T)
model_nnet<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet', importance=T)

####### Parameter tuning using Caret ###########

fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5)

### Using tuneGrid ####
modelLookup(model='gbm')

#Creating grid
grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))

# training the model
model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneGrid=grid)

# summarizing the model
print(model_gbm)

# Visualizing the models
plot(model_gbm)
model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneLength=10)
print(model_gbm)

#using tune length
model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneLength=10)
print(model_gbm)

# visualize the models
plot(model_gbm)
#ance estimation using caret ##################
#Checking variable importance for GBM
#Variable Importance
varImp(object=model_gbm)


#Plotting Varianle importance for GBM
plot(varImp(object=model_gbm),main="GBM - Variable Importance")

#Checking variable importance for RF
varImp(object=model_rf)


#Plotting Varianle importance for Random Forest
plot(varImp(object=model_rf),main="RF - Variable Importance")

#Checking variable importance for NNET
varImp(object=model_nnet)
#nnet variable importance

#Plotting Variable importance for Neural Network
plot(varImp(object=model_nnet),main="NNET - Variable Importance")

############ Predictions using Caret #################
#Predictions
predictions<-predict.train(object=model_gbm,testSet[,predictors],type="raw")
table(predictions)
#Confusion Matrix and Statistics
confusionMatrix(predictions,testSet[,outcomeName])

#6

library(caretEnsemble)

# Stacking Algorithms - Run multiple algos in one call.
trainControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE)

algorithmList <- c('rf', 'gbm', 'nnet')

set.seed(100)
models <- caretList(trainSet ~ ., data=trainData, trControl=trainControl, methodList=algorithmList) 
results <- resamples(models)
summary(results)


# 7

model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneLength=20)
print(model_gbm)

# visualize the models
plot(model_gbm)


# 8 Using GGplot and gridExtra to plot all variable of importance plots into one single plot.
# cited:https://datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html
library(gridExtra)

boxplot <- ggplot(data = scat_orig, 
                             mapping = aes(x = d13C, y = d15N)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(x = "D13c", y = "D15N") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

mass_cnplot <- ggplot(data = scat_orig, 
                         mapping = aes(x = CN, y = Mass, color = genus)) +
  geom_line() + 
  labs(x = "cn", y = "Mass")

grid.arrange(boxplot, mass_cnplot, ncol = 2, widths = c(4, 6))




# 9  I my result to compairing to other models I think Random forest model
# is performed the best. I found out the variable importance is higher in the random forest model.
# I think it handeled It can handle binary features, categorical features, and numerical features to provide better result.
#I dont think so we can accurately predict species in this dataset because the 
#result ia kind of off and not really close on variable importance.
# following the result of the variable importance.

#rf variable importance
#Overall
#d13C       100.00
#d15N        89.45
#segmented   75.30
#Mass        40.72
#CN           0.00




#10 a    Using feature selection with rfe in caret and the repeatedcv method: Find the top 3 predictors and build the same models as in 6 and 8 with the same parameters. 


fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated ten times
                           repeats = 10)

gbmFit1 <- train(Species ~ ., data =scat_orig , 
                 method = "gbm", 
                 trControl = fitControl,
                 
                 verbose = FALSE)
nnetFit1 <- train(Species ~ ., data = scat_orig, 
                 method = "nnet", 
                 trControl = fitControl,
                 
                 verbose = FALSE)

rfFit1 <- train(Species ~ ., data = scat_orig, 
                 method = "rf", 
                 trControl = fitControl,
                 
                 verbose = FALSE)


