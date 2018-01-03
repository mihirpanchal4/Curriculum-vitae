test=read.csv("test.csv")
train=read.csv("train.csv")


dim(train)
dim(test)

colnames(train)
colnames(test)

sum(is.na(train))
sum(is.na(test))

View(test)
View(train)

test$SalePrice=c(NA)
summary(test)
Complete_Data=rbind(train,test)
dim(Complete_Data)
View(Complete_Data)

colSums(is.na(Complete_Data))

Complete_Data$Alley=NULL
Complete_Data$FireplaceQu=NULL
Complete_Data$PoolQC=NULL
Complete_Data$Fence=NULL
Complete_Data$MiscFeature=NULL

dim(Complete_Data)
sum(is.na(Complete_Data))
colnames(Complete_Data)

summary(Complete_Data)
sapply(Complete_Data,class)


install.packages('moments')
library(moments)
install.packages('nortest')
library(nortest)

qqnorm(Complete_Data$SalePrice)
qqline(Complete_Data$SalePrice)
shapiro.test(Complete_Data$SalePrice)

words1=rnorm(Complete_Data$SalePrice)
plot(density(words1))
shapiro.test(words1)
qqnorm(words1)
qqline(words1,col=2)

describe(Complete_Data$SalePrice)
plot(Complete_Data$SalePrice)

summary(Complete_Data$SalePrice)
median(na.omit(Complete_Data$SalePrice))
Complete_Data$SalePrice[is.na(Complete_Data$SalePrice)]=median(na.omit(Complete_Data$SalePrice))
colSums(is.na(Complete_Data))

Mode=function(x, na.rm=FALSE){
  if(na.rm){
    x=x[!is.na(x)]
  }
  ux=unique(x)
  return(ux[which.max(tabulate(match(x,ux)))])
}

Complete_Data$MSZoning[is.na(Complete_Data$MSZoning)]=Mode(na.omit(Complete_Data$MSZoning))


plot(Complete_Data$LotFrontage)

Complete_Data$LotFrontage[is.na(Complete_Data$LotFrontage)]=mean(na.omit(Complete_Data$LotFrontage))
Complete_Data$Utilities[is.na(Complete_Data$Utilities)]=Mode(na.omit(Complete_Data$Utilities))
Complete_Data$Exterior1st[is.na(Complete_Data$Exterior1st)]=Mode(na.omit(Complete_Data$Exterior1st))
Complete_Data$Exterior2nd[is.na(Complete_Data$Exterior2nd)]=Mode(na.omit(Complete_Data$Exterior2nd))
Complete_Data$MasVnrType[is.na(Complete_Data$MasVnrType)]=Mode(na.omit(Complete_Data$MasVnrType))


plot(Complete_Data$MasVnrArea)
Complete_Data$MasVnrArea[is.na(Complete_Data$MasVnrArea)]=mean(na.omit(Complete_Data$MasVnrArea))

plot(Complete_Data$BsmtQual)
Complete_Data$BsmtQual[is.na(Complete_Data$BsmtQual)]=Mode(na.omit(Complete_Data$BsmtQual))

Complete_Data$BsmtCond[is.na(Complete_Data$BsmtCond)]=Mode(na.omit(Complete_Data$BsmtCond))
Complete_Data$BsmtExposure[is.na(Complete_Data$BsmtExposure)]=Mode(na.omit(Complete_Data$BsmtExposure))
Complete_Data$BsmtFinType1[is.na(Complete_Data$BsmtFinType1)]=Mode(na.omit(Complete_Data$BsmtFinType1))

plot(Complete_Data$BsmtFinSF1)
Complete_Data$BsmtFinSF1[is.na(Complete_Data$BsmtFinSF1)]=mean(na.omit(Complete_Data$BsmtFinSF1))

Complete_Data$BsmtFinType2[is.na(Complete_Data$BsmtFinType2)]=Mode(na.omit(Complete_Data$BsmtFinType2))


plot(Complete_Data$BsmtFinSF2)
qqnorm(Complete_Data$BsmtFinSF2)
qqline(Complete_Data$BsmtFinSF2, col=2)

install.packages('psych')
library(psych)
describe(Complete_Data$BsmtFinSF2)


Complete_Data$BsmtFinSF2[is.na(Complete_Data$BsmtFinSF2)]=median(na.omit(Complete_Data$BsmtFinSF2))

plot(Complete_Data$BsmtUnfSF)
qqnorm(Complete_Data$BsmtUnfSF)
qqline(Complete_Data$BsmtUnfSF, col=2)
describe(Complete_Data$BsmtUnfSF)
Complete_Data$BsmtUnfSF[is.na(Complete_Data$BsmtUnfSF)]=mean(na.omit(Complete_Data$BsmtUnfSF))

plot(Complete_Data$TotalBsmtSF)
qqnorm(Complete_Data$TotalBsmtSF)
qqline(Complete_Data$TotalBsmtSF, col=2)
describe(Complete_Data$TotalBsmtSF)
Complete_Data$TotalBsmtSF[is.na(Complete_Data$TotalBsmtSF)]=mean(na.omit(Complete_Data$TotalBsmtSF))

Complete_Data$Electrical[is.na(Complete_Data$Electrical)]=Mode(na.omit(Complete_Data$Electrical))

plot(Complete_Data$BsmtFullBath)
qqnorm(Complete_Data$BsmtFullBath)
qqline(Complete_Data$BsmtFullBath, col=2)
describe(Complete_Data$BsmtFullBath)
Complete_Data$BsmtFullBath[is.na(Complete_Data$BsmtFullBath)]=mean(na.omit(Complete_Data$BsmtFullBath))

plot(Complete_Data$BsmtHalfBath)
qqnorm(Complete_Data$BsmtHalfBath)
qqline(Complete_Data$BsmtHalfBath, col=2)
describe(Complete_Data$BsmtHalfBath)
Complete_Data$BsmtHalfBath[is.na(Complete_Data$BsmtHalfBath)]=median(na.omit(Complete_Data$BsmtHalfBath))

Complete_Data$KitchenQual[is.na(Complete_Data$KitchenQual)]=Mode(na.omit(Complete_Data$KitchenQual))
Complete_Data$Functional[is.na(Complete_Data$Functional)]=Mode(na.omit(Complete_Data$Functional))
Complete_Data$GarageType[is.na(Complete_Data$GarageType)]=Mode(na.omit(Complete_Data$GarageType))

plot(Complete_Data$GarageYrBlt)
qqnorm(Complete_Data$GarageYrBlt)
qqline(Complete_Data$GarageYrBlt, col=2)
describe(Complete_Data$GarageYrBlt)
Complete_Data$GarageYrBlt[is.na(Complete_Data$GarageYrBlt)]=mean(na.omit(Complete_Data$GarageYrBlt))

Complete_Data$GarageFinish[is.na(Complete_Data$GarageFinish)]=Mode(na.omit(Complete_Data$GarageFinish))

plot(Complete_Data$GarageCars)
qqnorm(Complete_Data$GarageCars)
qqline(Complete_Data$GarageCars, col=2)
describe(Complete_Data$GarageCars)
Complete_Data$GarageCars[is.na(Complete_Data$GarageCars)]=mean(na.omit(Complete_Data$GarageCars))

plot(Complete_Data$GarageArea)
qqnorm(Complete_Data$GarageArea)
qqline(Complete_Data$GarageArea, col=2)
describe(Complete_Data$GarageArea)
Complete_Data$GarageArea[is.na(Complete_Data$GarageArea)]=mean(na.omit(Complete_Data$GarageArea))

Complete_Data$GarageQual[is.na(Complete_Data$GarageQual)]=Mode(na.omit(Complete_Data$GarageQual))
Complete_Data$GarageCond[is.na(Complete_Data$GarageCond)]=Mode(na.omit(Complete_Data$GarageCond))
Complete_Data$SaleType[is.na(Complete_Data$SaleType)]=Mode(na.omit(Complete_Data$SaleType))

colSums(is.na(Complete_Data))
sum(is.na(Complete_Data))
complete.cases(Complete_Data)

summary(Complete_Data)
View(Complete_Data)

New_Complete_Data=Complete_Data

New_Complete_Data$CentralAir=as.factor(ifelse(New_Complete_Data$CentralAir=='Y',1,0))
levels(New_Complete_Data$MSZoning)[1]='C'
summary(New_Complete_Data)

View(New_Complete_Data)
New_Complete_Data$SaleCondition

str(New_Complete_Data)
write.csv(New_Complete_Data,file = 'New_Complete_Data.csv',row.names = F)

NewCompleteData=New_Complete_Data[,2:76]
dim(NewCompleteData)



set.seed(100)
sample=sample(1:nrow(NewCompleteData), size = 0.3*nrow(NewCompleteData))
Test=NewCompleteData[sample,]
Train=NewCompleteData[-sample,]
colnames(Train)

#Decision Tree
install.packages("tree")
library(tree)

Tree=tree(SalePrice~.,Train)
summary(Tree)

cor(OverallQual,GrLivArea)
b=data.frame(NewCompleteData$OverallQual,NewCompleteData$GrLivArea,NewCompleteData$GarageArea,NewCompleteData$BsmtFinSF1,NewCompleteData$X2ndFlrSF,NewCompleteData$BsmtUnfSF)
cor(b)
pairs(b)

par(mar=c(2,2,2,2))
plot(Tree)
text(Tree,pretty = 0)

CVTree=cv.tree(Tree)
plot(CVTree)

prunedTree=prune.tree(Tree,best = 5)
plot(prunedTree)
text(prunedTree,pretty=0)
#Here, best 5 variables were used to prune the tree, as at this point the tree has the lowest fall when plotted
#and it performed well, as including other variables raised the peak in plot and increased the disorderness of tree.
cv=cv.tree(prunedTree)
plot(cv)

predTree=predict(Tree,data=Test)
SaleTest=Test$SalePrice
mean(sum(predTree-SaleTest)^2)
# Accuracy is 100-8.1713= 91.8287

describe(SaleTest)
describe(predTree)
A=ifelse(Test$SalePrice<305828,0,1)
B=ifelse(predTree<274506,0,1)

Misclass_Error=mean(A!=B)
Accuracy_Mod=print(paste("Accuracy (%) of Model is ",(1-Misclass_Error)*100))
# Accuracy of Model is  94.5205479452055. Here, the accuracy of model is better compared
#to the previous one of 91.82. After making some essential changes like giving the values
# in the form of 0 and 1 to the actual and predicted data depending on some criteria
# the model performs well and fits the data well.


#Linear Regression
set.seed(1)

cor(OverallQual,SalePrice)
cor(LotArea,SalePrice)
#First we will just check for the simple linear regression by using the LotArea and OverAllQuall variables separately,
#as these variables are significant and has a good positive correlation with the Saleprice
#and can be directly linked to the Saleprice.
lm.fit=lm(SalePrice~LotArea,data=Train)
summary(lm.fit)
SaleTest=Test$SalePrice
mean(sum(SaleTest-predict(lm.fit,Test))^2)

lm.fit1=lm(SalePrice~poly(LotArea,2),data=Train)
summary(lm.fit1)
mean(sum(SaleTest-predict(lm.fit1,Test))^2)

lm.fit2=lm(SalePrice~poly(LotArea,3),data=Train)
summary(lm.fit2)
mean(sum(SaleTest-predict(lm.fit2,Test))^2)
plot(lm.fit1)
plot(lm.fit2)
#Here, model lm.fit2 gives better Rsqd than other 2 but it increases the MSE as well.
#also after plotting the model, we can see there is not much changes happened between 
#all the models. So its better to keep the lm.fit1 model as it gives the decent Rsqd and
#good MSE compared to the remaining ones.

#Now, we will build the simple linear model by using the variable of OverallQual.
lm1.fit=lm(SalePrice~OverallQual,data=Train)
summary(lm1.fit)
SaleTest=Test$SalePrice
mean(sum(SaleTest-predict(lm.fit1,Test))^2)

lm2.fit=lm(SalePrice~(LotArea)^2,data=Train)
summary(lm2.fit)
mean(sum(SaleTest-predict(lm2.fit,Test))^2)

lm3.fit=lm(SalePrice~log(LotArea),data=Train)
summary(lm3.fit)
mean(sum(SaleTest-predict(lm3.fit,Test))^2)

lm4.fit=lm(SalePrice~poly(LotArea,2),data=Train)
summary(lm4.fit)
mean(sum(SaleTest-predict(lm4.fit,Test))^2)
plot(lm1.fit)
#Here, we tried using different interaction terms like poly,taking square and log,
#but none of them performed better than the lm1.fit model. lm1.fit model had the better
# Rsqd of 0.284 and the lowest MSE of 5.922 compared to the remaining 3 models. 

#From, the above 2 best simple linear model(built using OverallQual and LotArea),
#the model lm1.fit ha a better Rsqd and lowest MSE.

#Now, we will try to build the multiple linear model using the variables OverallQual
#and LotArea.
MulLM.fit=lm(SalePrice~ OverallQual +LotArea,data=Train)
summary(MulLM.fit)
SaleTest=Test$SalePrice
mean(sum(SaleTest-predict(MulLM.fit,Test))^2)

MulLM.fit1=lm(SalePrice~ log(OverallQual) +LotArea,data=Train)
summary(MulLM.fit1)
mean(sum(SaleTest-predict(MulLM.fit1,Test))^2)

MulLM.fit2=lm(SalePrice~ (OverallQual)^2 +LotArea,data=Train)
summary(MulLM.fit2)
mean(sum(SaleTest-predict(MulLM.fit2,Test))^2)

MulLM.fit3=lm(SalePrice~ poly(OverallQual,2) +LotArea,data=Train)
summary(MulLM.fit3)
mean(sum(SaleTest-predict(MulLM.fit3,Test))^2)
plot(MulLM.fit3)

#Here, model MulLM.fit3 is the better model, as it has the highest Rsqd of 0.3442 and the lowest MSE 2.640
#compared to the other models.Also, after plotting,it shows that the response data and fitted data 
#coordinates pretty well and fits nicely in the graph. Normal Q-Q plot in it shows that the
#the data passes through the straight line most of the time and deviates less.
#Tried using different interaction terms to build different models and concluded that the
#model MulLM.fit3 fits the data nicely.

#Building linear models using all the variables
set.seed(10)

MulLM=lm(SalePrice~.,data = Train)
summary(MulLM)
mean(sum(Test$SalePrice-predict(MulLM,Test)))
plot(MulLM)

PredMul=predict(MulLM,Test)
describe(Test$SalePrice)
describe(PredMul)
M=ifelse(Test$SalePrice<288378,0,1)
N=ifelse(PredMul<193054,0,1)

Misclass_Error2=mean(M!=N)
Accuracy_Mod2=print(paste("Accuracy (%) of Model is ",(1-Misclass_Error2)*100))

#Model MulLM gives good accuracy of fitting the data with 79.88 % when used all the variables to build the model.
#Some variables generates error due to the empty values in the dataframe of test and train after splitting them.
#So just imputed the values in all of them one by one by taking the Mode.

Test$RoofMatl=as.factor(Test$RoofMatl)
Train$RoofMatl=as.factor(Train$RoofMatl)
Mode(Test$RoofMatl)
levels(Test$RoofMatl)[2]='CompShg'

levels(Test$Exterior2nd)
levels(Train$Exterior2nd)
Mode(Train$Exterior2nd)
Mode(Test$Exterior2nd)
levels(Test$Exterior2nd)[10]='VinylSd'
levels(Train$Exterior2nd)[10]='VinylSd'

levels(Test$Heating)
levels(Train$Heating)
Mode(Train$Heating)
Mode(Test$Heating)
levels(Test$Heating)[1]='GasA'
levels(Train$Heating)[1]='GasA'

levels(Test$Electrical)
levels(Train$Electrical)
Mode(Train$Electrical)
Mode(Test$Electrical)
levels(Test$Electrical)[4]='SBrkr'
levels(Train$Electrical)[4]='SBrkr'

FullData=rbind(Test,Train)
write.csv(FullData,file = 'FullData.csv',row.names = F)

#Now we will try to drop some insignificant variables and build the model and check the accuracy of it.
#We will keep only significant ones by creating new Test and Train dataframes.
NewFullData=FullData[,c("LotArea","Neighborhood","OverallQual","YearBuilt","RoofMatl","Exterior1st","Exterior2nd","BsmtFinType2",
                        "X2ndFlrSF","BedroomAbvGr","KitchenAbvGr","KitchenQual","TotRmsAbvGrd","SalePrice")]
indexes=sample(1:nrow(NewFullData),size=0.3*nrow(NewFullData))
NewTrain=NewFullData[-indexes,]
NewTest=NewFullData[indexes,]

dim(NewTrain)
dim(NewTest)
colnames(NewTrain)
colnames(NewTest)
sapply(NewTrain,class)

NewMulLM=lm(SalePrice~.,data=NewTrain)
summary(NewMulLM)
NewSaleTest=NewTest$SalePrice
mean((predict(NewMulLM,NewTest)-NewSaleTest))
plot(predict(NewMulLM,NewTest)-NewSaleTest)
abline(0,0)
resid=residuals(NewMulLM,type="deviance")
mean(resid)
plot(resid)

LMpred=predict(NewMulLM,NewTest)
describe(NewSaleTest)
describe(LMpred)
C=ifelse(NewSaleTest<372500,0,1)
D=ifelse(LMpred<171702,0,1)

Misclass_Error1=mean(C!=D)
Accuracy_Mod1=print(paste("Accuracy (%) of Model is ",(1-Misclass_Error1)*100))

#Above model NewMulLM is not performing as per the expectation. since it gives the accuracy of only 60.34 % which is not good fit for the data in model,

levels(NewTest$RoofMatl)
levels(NewTrain$RoofMatl)
summary(NewTrain)
summary(NewTest)
Mode(NewTrain$RoofMatl)
Mode(NewTest$RoofMatl)
levels(NewTest$RoofMatl)[5]='CompShg'
levels(NewTrain$RoofMatl)[5]='CompShg'
levels(NewTest$RoofMatl)[6]='CompShg'
levels(NewTrain$RoofMatl)[6]='CompShg'
levels(NewTest$RoofMatl)[7]='CompShg'
levels(NewTrain$RoofMatl)[7]='CompShg'

levels(NewTest$Exterior1st)
levels(NewTrain$Exterior1st)
Mode(NewTrain$Exterior1st)
Mode(NewTest$Exterior1st)
levels(NewTest$Exterior1st)[5]='VinylSd'
levels(NewTrain$Exterior1st)[5]='VinylSd'


#Random Forest
install.packages("randomForest")
library(randomForest)
RanFor=randomForest(SalePrice~.,data = NewTrain,mtry=11,importance=TRUE)
summary(RanFor)

importance(RanFor)
varImpPlot(RanFor)

RanPred=predict(RanFor,newdata = NewTest)
summary(RanPred)
plot(RanPred)
plot(NewTest$SalePrice)
plot(RanPred,NewTest$SalePrice)
abline(0,1)
mean((RanPred-NewTest$SalePrice))

#Accuracy of model RanFor fitting the data is good.
#Now we will try to check what happens with the model when we increase or decrease the mtry and ntree.
RanFor1=randomForest(SalePrice~.,data = NewTrain,mtry=11,ntree=25,importance=TRUE)
RanFor1
RanPred1=predict(RanFor1,newdata = NewTest)
mean((RanPred1-NewTest$SalePrice))


RanFor2=randomForest(SalePrice~.,data = NewTrain,mtry=8,importance=TRUE)
RanFor2
RanPred2=predict(RanFor2,newdata = NewTest)
mean((RanPred2-NewTest$SalePrice))

RanFor3=randomForest(SalePrice~.,data = NewTrain,mtry=8,ntree=25,importance=TRUE)
RanFor3
RanPred3=predict(RanFor3,newdata = NewTest)
mean((RanPred3-NewTest$SalePrice))

RanFor4=randomForest(SalePrice~.,data = NewTrain,mtry=6,ntree=25,importance=TRUE)
RanFor4
RanPred4=predict(RanFor4,newdata = NewTest)
mean((RanPred4-NewTest$SalePrice))

importance(RanFor4)
varImpPlot(RanFor4)

max(RanPred4)/2
max(NewTest$SalePrice)/2
K=ifelse(NewTest$SalePrice<372500,0,1)
L=ifelse(RanPred4<252581,0,1)
Misclass_Error3=mean(K!=L)
Accuracy_Mod3=print(paste("Accuracy (%) of Model is ",(1-Misclass_Error3)*100))
table(K,L)

#From these all models, Model RanFor4 consistently gives the better accuracy of 97% approximately of fitting the data.
#Here, we can see that as we decrease the number of variables and the size of tree together in a same model, the model gives less error everytime.
#Since, the value of error in output changes everytime as it selects the different different best 6 variables
#while building a randomForest during each and every process. But Model RanFor4 gives the least error and
#hence better accuracy of around 97 % of fitting the data. hence, RanFor4 is the best model so far.



#Support Vector Machine

install.packages("e1071")
library(e1071)
attach(NewTrain)

SVM_mod=svm(SalePrice~.,data=NewTrain)
summary(SVM_mod)

cor(NewTrain$LotArea,NewTrain$SalePrice)
#Since, LotArea is considered to be the most important and the first factor while buying any land or predicting the salepprice of land,
#We will try to build the SVM model using this single independent variable as it has a good positive correlation of 0.24 with saleprice as well.
#We will split LotArea and SalePrice in terms of X1 and Y1 respectively.
X1=subset(NewTest, select = LotArea)
Y1=subset(NewTest, select= SalePrice)

SVM_mod1=svm(SalePrice~LotArea,data = NewTrain, kernel="radial")
summary(SVM_mod1)
SVM_pred1=predict(SVM_mod1,X1)
MSE=(sum(SVM_pred1-Y1)/length(Y1))^2
MSE
#This model gives the less error of only 5.24, which shows that this model is good for fitting the data
#and there is not much difference between the actual and predicted Saleprice.

cor(NewTrain$OverallQual,NewTrain$LotArea)
cor(NewTrain$OverallQual,NewTrain$SalePrice)
#now we will try to build another model using LotArea and OverallQual,
#as these both factors becaomes essential while determining the Saleprice of any land
#because LotArea decides the price directly but OverallQual determines the price indirecly
#and affects the price of land, as one always wants to live in a good quality of House.
#Also, OverallQual correlates well with the variables LotArea and Saleprice
#with values of 0.093 and 0.55 respectively.
#This makes OverallQual the important reason behind the decrease or increase in saleprice of house along with LotArea.

X2=NewTest[,c("LotArea","OverallQual")]
colnames(X2)
Y2=subset(NewTest, select= SalePrice)

SVM_mod2=svm(SalePrice~LotArea+OverallQual,data = NewTrain, kernel="radial")
summary(SvM_mod2)
SVM_pred2=predict(SVM_mod2,X2)
MSE1=(sum(SVM_pred2-Y2)/length(Y2))^2
MSE1

#Model SVM_mod2 gives the lowest error of 3.43 when build the model using 2 independent variables of LotArea and OverallQual,
#which shows that this model is good compared to the remaining 2 for fitting the data
#and there is very less difference between the actual and predicted Saleprice values.
#Here LotArea and OverallQual both has the significant impact on the Saleprice
#and shows that both of them should be always preferred  when model is built and saleprice is estimated.


#From these 4 different Model types, we conclude that the model made with Support Vector Machine
#named as SVM_mod2 is the best model compared to all of them, as it gives the less error and good accuracy.
#Also, since we have more number of Variables in our dataframe so it is better to use SVM model.
#Another reason for choosing this model is that we can tune SVM. Tuning helps as it allows us to specify the different parameters.
#Tuning trials all the combinations of parameters and locate the one combination that gives the best result.
#Best result is something where the model has the optimal performance on data and generates less error.
#Hence, due to these all factors and advantage of sVM, we choose SVM_mod2 model to train and test our data
#and to estimate the value of Saleprice.
#SVM_mod2 was choosen as it gave the least error compared to the other sVM models.
#Radial Kernel trick was used as it was more suitable for the data that we have and
#for the regression analysis of Saleprice it performed well as the number of support vectors it used that were good for my model and its performance.







