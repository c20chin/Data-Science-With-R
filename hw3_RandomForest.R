setwd("./R_working_directory")
library(rpart)
source("./src/trainingSamples.R")
dataset = read.csv("./data/Project_data.csv", stringsAsFactors = TRUE)

dataset$subSample = trainingSamples(dataset, Training=0.6, Validation=0.15)
table(dataset$subSample)

head(dataset)

#Method 1. Decision Tree
EQ=as.formula(performance_group ~ yrs_employed + manager_hire + test_score + group_size + concern_flag
              + mobile_flag + customers + high_hours_flag + transfers + reduced_schedule+city)
treeModel=rpart(EQ,
                data=dataset, subset=subSample=="Training", cp=0.01)

dev.new();rattle::fancyRpartPlot(treeModel,sub=NULL,palettes=c("Greys", "Oranges")[2],type=1) 

treeModel$variable.importance

dataTest=subset(dataset,subSample=="Testing")

pred =predict(treeModel,newdata=dataTest,type="class")
CFMatrix.tree=table(Pred =pred, Actual = dataTest[,"performance_group"])  #Confusion matrix


#caret::confusionMatrix(CFMatrix.tree, positive="No")
caret::confusionMatrix(CFMatrix.tree, positive="Yes")


names(dataset)
#Method 2. Random Forest

library(randomForest)

rfModel <- randomForest(EQ, 
                        data=dataset, subset=subSample=="Training",ntrees=500,importance = TRUE)

pred2 = predict(rfModel,newdata=dataTest,type="class")

importance(rfModel)
sort(importance(rfModel)[,3],decreasing = TRUE)

CFMatrix.rf=table(Pred =pred2, Actual = dataTest[,"performance_group"])  #Confusion matrix
caret::confusionMatrix(CFMatrix.rf, positive="Yes")

# Method 3. Nnet
library(nnet)

nnetModel=nnet(EQ,
               data=dataset, subset=subSample=="Training",
               size = 5,
               decay = 5e-4,
               trace = FALSE,
               maxit = 500, 
               MaxNWts = 100)
predict(nnetModel,newdata=dataTest,type="class")

nnetAvg = caret::avNNet(EQ,data=dataset, subset=subSample=="Training",
                        repeats = 10,## Specify how many models to average
                        size = 5,
                        decay = 5e-4,
                        trace = FALSE,
                        maxit = 500, 
                        MaxNWts = 100)
pred3 = predict(nnetAvg, newdata=dataTest,type="class")

CFMatrix.nnet=table(Pred =pred3, Actual = dataTest[,"performance_group"])  #Confusion matrix

caret::confusionMatrix(CFMatrix.nnet, positive="Yes")

#Method 4. SVM
library(kernlab)
dataTrain=subset(dataset,subSample=="Training")
svmModel <- ksvm(EQ,
                 data=dataTrain,kpar = "automatic",
                 C = 1, epsilon = 0.1)
pred4 = predict(svmModel, newdata=dataTest,type="response")
CFMatrix.svm=table(Pred =pred4, Actual = dataTest[,"performance_group"])  #Confusion matrix

caret::confusionMatrix(CFMatrix.svm, positive="Yes")

