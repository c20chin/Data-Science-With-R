setwd("./R_working_directory")
library(caret)
source("./src/trainingSamples.R")
dataset=read.csv("./data/CCS.csv",stringsAsFactors = TRUE)

dataset$subSample = trainingSamples(dataset, Training=0.7, Validation=0.3)
table(dataset$subSample)

EQ=as.formula(MonthGive ~ DonPerYear + AveDonAmt + AveIncEA + SomeUnivP+LastDonAmt+YearsGive)
methods=c("glm","rpart","rf","svmRadial","nnet")   #rpart: decision tree

#Case 1.
output=train(EQ,
               data=dataset, 
               subset=subSample=="Training",
               method=methods[3])    #random forest
dataValid=subset(dataset,subSample=="Validation")

pred =predict(output,newdata=dataValid,type=c("raw","prob")[1])

CFMatrix.tab=table(Pred =pred, Actual = dataValid[,"MonthGive"])  #Confusion matrix

confusionMatrix(CFMatrix.tab, positive=c("No","Yes")[2])

#Case 2. Resampling via trainControl
output=train(EQ,
             data=dataset, 
             subset=subSample=="Training",
             method=methods[2],
             trControl = trainControl(method = c("cv","boot")[1],number=10,savePredictions =TRUE)
             )

output$pred


# Programming homework: 
#1. Explain what are you going to do to extract the 10-fold predictions for K-Fold CV.
#   Hint: Use output$bestTune
#2. Explain what are you going to do to extract the predictions for boot.
#3. Given SVM, compare the performance of two re-sampling method: "cv" vs. "boot". 
#   Using both training data and validation to show your results.
#4. Among the above-mentioned five methods, do  "cv" and "boot" select the same model?

# Make an appointment with me (Wed.~Fri. 1100-1130) to show me your work at my office.
# Email me your code before.


