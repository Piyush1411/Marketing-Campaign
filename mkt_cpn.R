setwd("C:/Users/User/Downloads")
Mkt_cpn <- read.csv("market campaign.csv")
#variable identification
dim(Mkt_cpn)#4483 rows of 10 variables
names(Mkt_cpn)#the variable names
str(Mkt_cpn)#variable types
View(Mkt_cpn)
#Data Distribution
summary(Mkt_cpn) #inference- no negative values but have missing values and NAs
#remove missing Values
y <- Mkt_cpn$job
table(y)
names(table(y))[table(y)==max(table(y))]
Mkt_cpn$job[is.na(Mkt_cpn$job)] = "management"

y1 <- Mkt_cpn$marital
table(y1)
names(table(y1))[table(y1)==max(table(y1))]
Mkt_cpn$marital[is.na(Mkt_cpn$marital)] = "married"

y2 <- Mkt_cpn$education
table(y2)
names(table(y2))[table(y2)==max(table(y2))]
Mkt_cpn$education[is.na(Mkt_cpn$education)] = "secondary"

y3 <- Mkt_cpn$contact
table(y3)
names(table(y3))[table(y3)==max(table(y3))]
Mkt_cpn$contact[is.na(Mkt_cpn$contact)] = "cellular"

y4 <- Mkt_cpn$Purchase_Made
table(y4)
names(table(y4))[table(y4)==max(table(y4))]
Mkt_cpn$Purchase_Made[is.na(Mkt_cpn$Purchase_Made)] = "no"

Mkt_cpn$age[is.na(Mkt_cpn$age)] = 39 #non_normal distribution ,median replacement
Mkt_cpn$days_since._signed_in[is.na(Mkt_cpn$days_since._signed_in)] = 16
Mkt_cpn$Time.spend.on.website[is.na(Mkt_cpn$Time.spend.on.website)] = 185 # median
Mkt_cpn$Number_of_campaigns[is.na(Mkt_cpn$Number_of_campaigns)] = 2
Mkt_cpn$previous_purchases[is.na(Mkt_cpn$previous_purchases)] = 0

#Histogram to see how the data is skewed
hist(Mkt_cpn$age)
#boxplot
boxplot(Mkt_cpn$age)
boxplot(Mkt_cpn$Time.spend.on.website)
boxplot(Mkt_cpn$Number_of_campaigns)
boxplot(Mkt_cpn$previous_purchases)
boxplot(Mkt_cpn$days_since._signed_in)

head(Mkt_cpn)
#divide the data into test and train
new_data = Mkt_cpn
t = new_data[,"Purchase_Made"] == "yes"
table(t)

classyes=new_data[t,] #class one
nrow(classyes)
classno=new_data[!t,] #class zero
nrow(classno)

set.seed(1)
t=sample(1:nrow(classyes),floor(0.7*nrow(classyes)))
length(t)
classyestrain=classyes[t,]
classyestest=classyes[-t,]

set.seed(1)
t=sample(1:nrow(classno),floor(0.7*nrow(classno)))
length(t)
classnotrain=classno[t,]
classnotest=classno[-t,]

PlIndex = which(names(Mkt_cpn) %in% "Purchase_Made")
xtrain = rbind(classyestrain[,-PlIndex],classnotrain[,-PlIndex])
xtest = rbind(classyestest[,-PlIndex],classnotest[,-PlIndex])

ytrain = c(classyestrain[,PlIndex],classnotrain[,PlIndex])
ytest = c(classyestest[,PlIndex],classnotest[,PlIndex])

#1.Naive Bayes algorithm
library(e1071)

Baye = naiveBayes(as.factor(ytrain) ~ ., data = xtrain)
Baye

#Confusionmatrix to check accuracy

ConfMat =table(predict(Baye, xtrain), ytrain)
ConfMat
Error = (ConfMat[1,2]+ConfMat[2,1])/nrow(xtrain)
Error*100

#mis-classification(train data)= 11.918%

ConfMatTest = table(predict(Baye, xtest), ytest) # testing on test dataset
ConfMatTest
Error = (ConfMatTest[1,2]+ConfMatTest[2,1])/nrow(xtest)
Error*100

#misclassification(test data)=12.639%

#2.CART Algorithm
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

#model fitting
set.seed(1)
ClassTree = rpart(as.factor(ytrain) ~ ., method = "class", data = xtrain, control = rpart.control(minsplit=10, cp=0.001)) #complexity parameter assumed at 0.001

#Best complexity parameter (cp) to reduce error & prune the tree

bestcp = ClassTree$cptable[which.min(ClassTree$cptable[,"xerror"]), "CP"]
bestcp #best cp =0.011

#Prune the tree with best cp derieved
tree.pruned = prune(ClassTree, cp = bestcp)

#plotting tree
prp(tree.pruned, box.col= c("red", "green")[ClassTree$frame$yval])

ClassTreeTrain = predict(tree.pruned, xtrain)
TrainClassify = apply(ClassTreeTrain,1,which.max)
ClassTreeMat = table(TrainClassify,ytrain)
ErrorTreeTrain = (ClassTreeMat[1,2]+ClassTreeMat[2,1])/nrow(xtrain)
ErrorTreeTrain*100

#mis classification (train data) = 9.878%
ClassTreeTest = predict(tree.pruned, xtest)
TestClassify = apply(ClassTreeTest,1,which.max)
ClassTreeMat = table(TestClassify,ytest)
ErrorTreeTest = (ClassTreeMat[1,2]+ClassTreeMat[2,1])/nrow(xtest)
ErrorTreeTest*100

#misclassification (test data) = 11.524%
# The misclassification is better than naives - hence CART score better than Naive Bayes model

#3.logistic regression
#dummy variables
Mkt_cpn$management <- ifelse(Mkt_cpn$job == "management",1,0)
Mkt_cpn$blue_collar <- ifelse(Mkt_cpn$job == "blue-collar",1,0)
Mkt_cpn$Technician <- ifelse(Mkt_cpn$job == "technician",1,0)
Mkt_cpn$admin <- ifelse(Mkt_cpn$job == "admin.",1,0)
Mkt_cpn$services <- ifelse(Mkt_cpn$job == "services",1,0)
Mkt_cpn$retired <- ifelse(Mkt_cpn$job == "retired",1,0)

Mkt_cpn$divorced <- ifelse(Mkt_cpn$marital == "divorced",1,0)
Mkt_cpn$married <- ifelse(Mkt_cpn$marital == "married",1,0)

Mkt_cpn$primary <- ifelse(Mkt_cpn$education == "primary",1,0)
Mkt_cpn$secondary <- ifelse(Mkt_cpn$education == "secondary",1,0)
Mkt_cpn$tertiary <- ifelse(Mkt_cpn$education == "tertiary",1,0)

Mkt_cpn$cellular <- ifelse(Mkt_cpn$contact == "cellular",1,0)
Mkt_cpn$telephone <- ifelse(Mkt_cpn$contact == "telephone",1,0)

Mkt_cpn$Purchased <- ifelse(Mkt_cpn$Purchase_Made == "yes",1,0)

head(Mkt_cpn)
summary(Mkt_cpn)

final_data <- Mkt_cpn[ -c(2,3,4,5,10)]
head(final_data)
summary(final_data)


#univariate approach

#[bx = boxplot(final_data$age)
#bx$stats
#quantile(final_data$age, seq(0,1,0.02))
#final_data$age<-ifelse(final_data$age>=70,70,final_data$age) 
#final_data$age<-ifelse(final_data$age<=19,19,final_data$age)
#bx = boxplot(final_data$age)

#bx1 = boxplot(final_data$days_since._signed_in)
#bx1$stats
#quantile(final_data$days_since._signed_in, seq(0,1,0.02))
#final_data$days_since._signed_in<-ifelse(final_data$days_since._signed_in>=31,31,final_data$days_since._signed_in) 
#final_data$days_since._signed_in<-ifelse(final_data$days_since._signed_in<=1,1,final_data$days_since._signed_in)
#bx1 = boxplot(final_data$days_since._signed_in)

#bx2 = boxplot(final_data$Time.spend.on.website)
#bx2$stats
#quantile(final_data$Time.spend.on.website, seq(0,1,0.02))
#final_data$Time.spend.on.website<-ifelse(final_data$Time.spend.on.website>=665,665,final_data$Time.spend.on.website) 
#final_data$Time.spend.on.website<-ifelse(final_data$Time.spend.on.website<=4,4,final_data$Time.spend.on.website)
#bx2 = boxplot(final_data$Time.spend.on.website)

#bx3 = boxplot(final_data$Number_of_campaigns)
#bx3$stats
#quantile(final_data$Number_of_campaigns, seq(0,1,0.02))
#final_data$Number_of_campaigns<-ifelse(final_data$Number_of_campaigns>=6,6,final_data$Number_of_campaign)
#final_data$Number_of_campaigns<-ifelse(final_data$Number_of_campaigns<=1,1,final_data$Number_of_campaigns)
#bx3 = boxplot(final_data$Number_of_campaigns)

#bx4 = boxplot(final_data$previous_purchases)
#bx4$stats
#quantile(final_data$previous_purchases, seq(0,1,0.02))
#final_data$previous_purchases<-ifelse(final_data$previous_purchases>=0,0,final_data$previous_purchases)
#final_data$previous_purchases<-ifelse(final_data$previous_purchases<=0,0,final_data$previous_purchases)
#bx4 = boxplot(final_data$previous_purchases)
x <- final_data$age
bx = boxplot(x)
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
bx$stats
bx = boxplot(x)

x1 <- final_data$days_since._signed_in
bx1 = boxplot(x1)
qnt <- quantile(x1, probs=c(.25, .75), na.rm = T)
caps <- quantile(x1, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x1[x1 < (qnt[1] - H)] <- caps[1]
x1[x1 > (qnt[2] + H)] <- caps[2]
bx1$stats
bx1 = boxplot(x1)

x2 <- final_data$Time.spend.on.website
bx2 = boxplot(x2)
qnt <- quantile(x2, probs=c(.25, .75), na.rm = T)
caps <- quantile(x2, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x2[x2 < (qnt[1] - H)] <- caps[1]
x2[x2 > (qnt[2] + H)] <- caps[2]
bx2$stats
bx2 = boxplot(x2)

x3 <- final_data$Number_of_campaigns
bx3 = boxplot(x3)
qnt <- quantile(x3, probs=c(.25, .75), na.rm = T)
caps <- quantile(x3, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x3[x3 < (qnt[1] - H)] <- caps[1]
x3[x3 > (qnt[2] + H)] <- caps[2]
bx3$stats
bx3 = boxplot(x3)

x4 <- final_data$previous_purchases
bx4 = boxplot(x4)
qnt <- quantile(x4, probs=c(.25, .75), na.rm = T)
caps <- quantile(x4, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x4[x4 < (qnt[1] - H)] <- caps[1]
x4[x4 > (qnt[2] + H)] <- caps[2]
bx4$stats
bx4 = boxplot(x4)

#bivariate analysis
library(car)
library(ggplot2)
plot(final_data$age,final_data$Purchase_Made)
plot(final_data$Number_of_campaigns,final_data$Purchase_Made)
plot(final_data$days_since._signed_in,final_data$age)#no correlation
plot(final_data$Time.spend.on.website,final_data$age)#ages 25 to 60 spend less time on website
plot(final_data$Number_of_campaigns,final_data$age)#ages 30 to 55 have higher no. of campaigns

#divide the data into test and train
set.seed(1)
t=sample(1:nrow(final_data),0.7*nrow(final_data))
t_train=final_data[t,]
t_test=final_data[-t,]

#methods


#checking the multi-collinearity
library(car)
mod<- lm(Purchased ~ ., data = t_train)
t = vif(mod)
sort(t, decreasing = T)

#Since all the variable are below the threshold of 5, we can proceed with the model
mod1 <- glm(as.factor(Purchased) ~ ., family="binomial", data = t_train)
summary(mod1)

#instead of removing all these variables one by one, we use the step function, which automatically calculated the best equation
stpmod = step(mod1, direction = "both")
formula(stpmod)
summary(stpmod)

#checking the probability for each observation by creating a variable names score
mod2 <- glm(as.factor(Purchased) ~ Time.spend.on.website + Number_of_campaigns + 
              previous_purchases + retired + tertiary + cellular + telephone, family="binomial", data=t_train)
summary(mod2)
t_train$score=predict(mod2,newdata=t_train,type = "response")
head(t_train$score)
tail(t_train$score)

#Lets try to analyse the confusion matrix and model accuracy
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
prediction<-ifelse(t_train$score>=0.6,1,0)
confusionMatrix(table(prediction,t_train$Purchased))
confusionMatrix(table(t_train$Purchased, prediction, dnn=list("actual","predicted")))

##Mcfadden test
library(pscl)
pR2(mod2)

# Concordance Test #
library(InformationValue)
library(caret)
concor <- Concordance(t_train$Purchased,t_train$score)
concor

#lets check the AUC and ROC
##AUC
library(InformationValue)
plotROC(actuals = t_train$Purchased,predictedScores = as.numeric(fitted(mod2)))
ks_plot(actuals = t_train$Purchased,predictedScores = as.numeric(fitted(mod2)))
ks_stat(actuals = t_train$Purchased,predictedScores = as.numeric(fitted(mod2)))
t_test$score2= predict(mod2, t_test, type="response")
View(t_test)


