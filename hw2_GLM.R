setwd("./R_working_directory")
dat = read.csv("./data/Project_data.csv", stringsAsFactors = TRUE)
head(dat)
nRow = nrow(dat)
# turn bottom, middle, top into two factors
levels(dat$performance_group)[c(2)] <- "Top"
ID = sample(1:nRow , 285)
TrainData <- dat[ID,]
newData = dat[-ID,]



eq1 = as.formula(performance_group~test_score+yrs_employed+concern_flag)
output1 = glm(eq1,data=TrainData,family="binomial")
summary(output1)
A1 = predict(output1, newdata = newData)
A = ifelse(A1>0.5,1,0)
A

output2 = glm(eq1,data=TrainData,family=binomial("probit"))
B1=predict(output2, newdata = newData)
B = ifelse(B1>0.5,1,0)
identical(A,B)

cbind(A,B)



source("./src/trainingSamples.R")

dat$subSample = trainingSamples(dat, Training=0.7, Validation=0.15)
table(dat$subSample)



##=== Step 1. Model Building by glm
linearModel<-glm(performance_group ~ yrs_employed + manager_hire + test_score + group_size + concern_flag
               + mobile_flag + customers + high_hours_flag + transfers + reduced_schedule+city,
               family=binomial(logit),data=dat,
               subset=subSample=="Training")

summary(linearModel)

##=== 1.1 Check non-linearity
##=== (A) By scatter plot
dat$group.num<- ifelse(dat$performance_group == 'Top', 1, 0)

car::scatterplot(group.num~yrs_employed, 
                 regLine=TRUE, smooth=list(span=0.5, spread=TRUE,col="red"), boxplots='xy', data=dat)

##=== (B) By Plot of Means
dat$yrs_employed.Bin <- RcmdrMisc::binVariable(dat$yrs_employed, bins=4, 
                                                method='proportions', labels=c('1','2','3','4'))

RcmdrMisc::plotMeans(dat$group.num, dat$yrs_employed.Bin, error.bars="se",connect=TRUE)

##=== 1.2 Check the importance
car::Anova(linearModel, type="II", test="LR")


### Solution 1. For nonlinearity. Use log(AveDonAmt) to replace AveDonAmt
dat$log.yrs_employed=log(dat$yrs_employed)
logModel<-glm(performance_group ~ log.yrs_employed + manager_hire + test_score + group_size + concern_flag
            + mobile_flag + customers + high_hours_flag + transfers + reduced_schedule + city,
            family=binomial(logit),data=dat,
            subset=subSample=="Training")
summary(logModel)



##==== Mixed1, relabel city into New York and Others
dat$city.New=dat$city
levels(dat$city.New)
levels(dat$city.New)[c(1,2,4,5,6,7)] <- "Others"
Mixed1<-glm(performance_group ~ log.yrs_employed + manager_hire + test_score + group_size + concern_flag
            + mobile_flag + customers + high_hours_flag + transfers + reduced_schedule +city.New,
            family=binomial,data=dat,
            subset=subSample=="Training")
summary(Mixed1)


#===== Mixed2
dat$log.test_score=log(dat$test_score)
Mixed2<-glm(performance_group ~ log.yrs_employed + manager_hire + log.test_score + group_size + concern_flag
            + mobile_flag + customers + high_hours_flag + transfers + reduced_schedule +city.New,
            family=binomial(logit),data=dat,
            subset=subSample=="Training")

ID=c("Testing","Training","Validation")[1]

##=== Step 2. Generate Prediction: predicted probabilities
dataUsed4Pred=dat[dat$subSample==ID,]
Prob= predict(logModel,type="response", newdata=dataUsed4Pred)
new_Data=data.frame(performance_group=dataUsed4Pred$performance_group, Prob=Prob)
head(new_Data)
newDataSorted=new_Data[order(new_Data[,2],decreasing=TRUE),]

new_Data$Pred=ifelse(newData$Prob<0.5,"Bottom","Top")

CM.table=table(Predicted =new_Data$Pred, Observed = new_Data$performance_group)

#Confusion matrix
CM.table

##===Step 3. Lift Chart Analysis 
## 3-1.  Plot lift chart
data4LCA=subset(dat, subSample==ID)
dim(data4LCA)
dev.new();lift.chart("Mixed2", data=data4LCA, targLevel="Top", trueResp=0.01, type="incremental",sub=ID)
dev.new();lift.chart("Mixed2", data=data4LCA, targLevel="Top", trueResp=0.01, type="cumulative",sub=ID)

## 3-2.  Comparing Models
dev.new();lift.chart(c("linearModel", "logModel","Mixed1","Mixed2"), data=data4LCA, targLevel="Top", trueResp=0.01, type="incremental",sub=ID)
dev.new();lift.chart(c("linearModel", "logModel","Mixed1","Mixed2"), data=data4LCA, targLevel="Top", trueResp=0.01, type="cumulative",sub=ID)

##Step 4. Performance based upon Confusion Matrix

CM=caret::confusionMatrix(CM.table, positive="Top")
CM

##Step 5. ROC curve
library(pROC)
test_roc = roc(dataUsed4Pred$performance_group ~ Prob, plot = FALSE, print.auc = TRUE)
dev.new();plot(test_roc, print.auc = TRUE)
as.numeric(test_roc$auc)


