install.packages("rpart")
install.packages("C50")
install.packages("ROCR")
install.packages("randomForest")

library('rpart')
library(xlsx)
library(readxl)

MyData <- read_excel("/Volumes/AJAY/Grad School/SEM 1/Data Mining For Business/ASSGN 1/GermanCredit_assgt1_F18.xls")
summary(MyData)
attributes(MyData)

str(MyData)

cols <- c("RESPONSE", "FOREIGN", "TELEPHONE", "OWN_RES","CHK_ACCT","HISTORY","SAV_ACCT","EMPLOYMENT","PRESENT_RESIDENT","MALE_DIV","MALE_SINGLE","MALE_MAR_or_WID","GUARANTOR","REAL_ESTATE","PROP_UNKN_NONE","OTHER_INSTALL","RENT","JOB")
MyData[cols] <- lapply(MyData[cols], factor)
sapply(MyData, class)

MyData$X <- NULL
summary(MyData)

plot(MyData$RESPONSE)

cols1<-c("NEW_CAR","USED_CAR","FURNITURE","RADIO/TV","EDUCATION","RETRAINING","OBS","CO-APPLICANT")

cols2<-c(MyData$`CO-APPLICANT`)
MyData<-MyData[-c(1,5:10,18)]

MyData<-MyData[-c(1001,1002,1003,1004,1005,1006),]

dim(MyData)

library(rpart)
ctr=rpart.control(maxdepth = 7)
Model1 = rpart(RESPONSE~.,data=MyData,method="class",parms = list(split = 'information'),control=ctr)
print(Model1)
summary(Model1)

plot(Model1, uniform=TRUE,  main="Decision Tree")
text(Model1, use.n=TRUE, all=TRUE, cex=.7)

library(rpart.plot)
rpart.plot::prp(Model1, type=1, extra=1)


library(rpart)
View(MyData)
set.seed(123)

predTrn_whole=predict(Model1, data=MyData, type='class')
table(pred = predTrn_whole, true=MyData$RESPONSE)
mean(predTrn_whole==MyData$RESPONSE)


library(ggplot2)
library(gridExtra)
plot(MyData$RESPONSE)
dat <- data.frame(table(MyData$EMPLOYMENT,MyData$RESPONSE))
names(dat) <- c("EMPLOYMENT","RESPONSE", 'Count')
p1 <- ggplot(data=dat, aes(x=EMPLOYMENT, y=Count, fill= RESPONSE)) + geom_bar(stat = "identity")

dat <- data.frame(table(MyData$JOB,MyData$RESPONSE))
names(dat) <- c("JOB", "RESPONSE", "Count")
p2 <- ggplot(data=dat, aes(x=JOB, y=Count, fill= RESPONSE)) + geom_bar(stat = "identity")

#dat <- data.frame(table(MyData$SAV_ACCT,MyData$RESPONSE))
#names(dat) <- c("sAV_ACCT", "RESPONSE", "Count")
#p3 <- ggplot(data=dat, aes(x=SAV_ACCT, y=Count, fill= RESPONSE)) + geom_bar(stat = "identity")

dat <- data.frame(table(MyData$OTHER_INSTALL,MyData$RESPONSE))
names(dat) <- c("OTHER_INSTALL", "RESPONSE", "Count")
p4 <- ggplot(data=dat, aes(x=OTHER_INSTALL, y=Count, fill= RESPONSE)) + geom_bar(stat = "identity")

dat <- data.frame(table(MyData$RENT,MyData$RESPONSE))
names(dat) <- c("RENT", "RESPONSE", "Count")
p5 <- ggplot(data=dat, aes(x=RENT, y=Count, fill= RESPONSE)) + geom_bar(stat = "identity")

dat <- data.frame(table(MyData$CHK_ACCT,MyData$RESPONSE))
names(dat) <- c("CHK_ACCT", "RESPONSE", "Count")
p6 <- ggplot(data=dat, aes(x=CHK_ACCT, y=Count, fill= RESPONSE)) + geom_bar(stat = "identity")

dat <- data.frame(table(MyData$HISTORY,MyData$RESPONSE))
names(dat) <- c("HISTORY", "RESPONSE", "Count")
p7 <- ggplot(data=dat, aes(x=HISTORY, y=Count, fill= RESPONSE)) + geom_bar(stat = "identity")

dat <- data.frame(table(MyData$OWN_RES,MyData$RESPONSE))
names(dat) <- c("OWN_RES", "RESPONSE", "Count")
p8 <- ggplot(data=dat, aes(x=OWN_RES, y=Count, fill= RESPONSE)) + geom_bar(stat = "identity")
grid.arrange(p1,p2,p4,p5,p6,p7,p8)



nr=nrow(MyData)
trnIndex = sample(1:nr, size = round(0.5*nr), replace=FALSE) 
MyTrn=MyData[trnIndex,]   
MyTst = MyData[-trnIndex,]  

library(C50)
set.seed(123)
cModel1=C5.0(RESPONSE ~ ., data=MyTrn, method="class")
summary(cModel1)

#TREE ON TRAINING DATA 

Model2=rpart(RESPONSE ~ ., data=MyTrn, method="class")
predTrn=predict(Model2, MyTrn, type='class')
table(pred = predTrn, true=MyTrn$RESPONSE)
mean(predTrn==MyTrn$RESPONSE)

#CREATING CONFUSION MATRIX 

cm <- table(pred=predict(Model2,MyTst, type="class"), true=MyTst$RESPONSE)
n = sum(cm) 
diag = diag(cm)  
rowsums = apply(cm, 2, sum) 
colsums = apply(cm, 1, sum) 
p = rowsums / n
q = colsums / n 
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall)
accuracy
precision
recall
f1


#Plotting the best obtained model

plot(Model2, uniform=TRUE,  main="Decision Tree")
text(Model2, use.n=TRUE, all=TRUE, cex=.7)
rpart.plot::prp(rpModel2, type=1, extra=1)
summary(Model2)

library(rpart.plot)
rpart.plot::prp(Model2, type=2, extra=1)


#CHANGING THRESHOLD AND OBSERVING EFFECTS ON TRAINING DATA 

CTHRESH=0.8

predProbTrn=predict(Model2, MyTrn, type='prob')

#Confusion table

predTrn = ifelse(predProbTrn[,'1'] >= CTHRESH, '1', '0')
ct = table( pred = predTrn, true=MyTrn$RESPONSE)
ct

#Accuracy

mean(predTrn==MyTrn$RESPONSE)

#ROC ON TEST DATA 

library(ROCR)
MyTst$score<-predict(rfModel,type='prob',MyTst)
pred<-prediction(MyTst$score[,2],MyTst$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf)

#COST MATRIX 

costMatrix <- matrix(c(0,1,5, 0), byrow=TRUE, nrow=2)
colnames(costMatrix) <- c('Predict Good','Predict Bad')
rownames(costMatrix) <- c('Actual Good','Actual Bad')
costMatrix

library(rpart)

rpTree = rpart(RESPONSE ~ ., data=MyTrn, method="class", parms = list( prior = c(.70,.30), loss = costMatrix, split = "information"))

#CALCULATING THRESHOLD 

th = costMatrix[2,1]/(costMatrix[2,1] + costMatrix[1,2])
th

#CREATING A TREE USING RANDOM FOREST , PLOT ROC & CHECK PERFORMANCE 

library('randomForest')

set.seed(123)
MyTrn.imputed <- rfImpute(RESPONSE ~ ., MyTrn)
rfModel = randomForest(factor(RESPONSE) ~ ., data=MyTrn.imputed, ntree=200, importance=TRUE )


View(MyTrn)
a <- importance(rfModel)
varImpPlot(rfModel)
summary(a)

#PERFORMANCE ON TEST DATA OF RANDOM FOREST TREE 

perf_rf=performance(prediction(predict(rfModel,MyTst, type="prob")[,2], MyTst$RESPONSE), "tpr", "fpr")
plot(perf_rf)

predTrnProb=predict(rfModel, MyTrn, type='prob')
head(predTrnProb)

MyDataB <- subset(MyTrn, select=c("RESPONSE"))  
MyDataB$score<-predTrnProb[, 1] 

View(MyDataB)

#PROFITS AND CUMULATIVE PROFITS 

library(dplyr)

PROFITVAL=100
COSTVAL=-500

scoreTst=predict(rfModel,MyTst, type="prob")[,'1'] 
prLifts=data.frame(scoreTst)
prLifts=cbind(prLifts, MyTst$RESPONSE)

prLifts=prLifts[order(-scoreTst) ,]  

prLifts<-prLifts %>% mutate(profits=ifelse(prLifts$`MyTst$RESPONSE`=='1', PROFITVAL, COSTVAL), cumProfits=cumsum(profits))

plot(prLifts$cumProfits)

maxProfit= max(prLifts$cumProfits)
maxProfit_Ind = which.max(prLifts$cumProfits)
maxProfit_score = prLifts$scoreTst[maxProfit_Ind]
print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))

View(MyTst)

View(prLifts)
View(scoreTst)
