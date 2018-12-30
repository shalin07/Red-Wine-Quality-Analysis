## clear the enviornment
rm(list=ls())

#load the relevant packages
library(C50)
library(rpart)
library(neuralnet)
library(rpart.plot)  		
library(RColorBrewer)
library(rattle)
library(class)
library(randomForest)

#loading the csv file
wine=read.csv("C:/Users/shali/OneDrive/Desktop/Winequality-red.csv", header= TRUE)

#view dataset
View(wine)

#functio to normalize
mnorm <-function(x)
{
  z<-((x-min(x))/(max(x)-min(x)))
return(z)                              
}

#making new dataset with updated attribiutes
wine_normalize<-as.data.frame (         
  cbind(Fixed_Acidity = mnorm(wine[1]),
        Volatile_Acidity = wine[2],
        Citric_Acidity = wine[3],
        Residual_Sugar = mnorm(wine[4]),
        Chlorides = wine[5],
        Free_Sulphur_Dioxide = mnorm(wine[6]),
        Total_Sulphur_Dioxide = mnorm(wine[7]),
        Density = wine[8],
        Ph = wine[9],
        Sulpahte = wine[10],
        Alcohol = wine[11],
        Quality = ifelse(wine$quality>6,1,0)))

#view updated dataset
View(wine_normalize)

#dividing the data for traning and testing into 75-25 respectively 
index<-sample(nrow(wine_normalize), 0.25*nrow(wine_normalize))
training<-wine_normalize[-index,]
test<-wine_normalize[index,]

#using randomForest to train our model
wine_RF<-randomForest(factor(Quality)~. ,wine_normalize,ntree=150)

#getting results of Random Forest
wine_RF

#using cart to train model
wine_cart<-rpart(Quality~., data=training)

#plotting tree of our model
prp(wine_cart)
fancyRpartPlot(wine_cart)

#scoring test dataset
?predict()
Predict<-predict(wine_cart, test)
Predict

#checking accuracy of our cart
confMat <- table(test$Quality, Predict)
accuracy<-sum(diag(confMat))/sum(confMat)
accuracy

#using Ann to train our model
wine_ann<-neuralnet(Quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol
                    ,training, hidden = 3, threshold = 0.01)
#plot ann
plot(wine_ann)

#accuracy of ann model
accuracy = compute(wine_ann, test[,-12])
predict_=as.numeric(accuracy$net.result)
predict<-round(predict_)
table(Actual=test$Quality,predict)

#accuracy test
wrong<- (test$Quality!=predict)
rate<-sum(wrong)/length(wrong)
accu = (1-rate)*100
accu