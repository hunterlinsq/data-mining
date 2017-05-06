library(ISLR)

train <- (Smarket$Year <2005)
TrainD <- Smarket[train,]
TestD<-Smarket[!train,]

svmf1<-svm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,
           data=TrainD,kernel="linear", gamma=0,cost=1, scale=FALSE)
summary(svmf1)
svmf2<-svm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,
           data=TrainD,kernel="radial", gamma=0,cost=1, scale=FALSE)
summary(svmf2)
svmf3<-svm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,
           data=TrainD,kernel="polynomial", gamma=0,cost=1, scale=FALSE)
summary(svmf3)
plot(svmf1,TrainD,Lag1~Lag2)
plot(svmf1,TrainD,Lag1~Volume)

##choosing best cost
set.seed(1234)
tune.out1=tune(svm,Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=TrainD,kernel="linear",
              ranges=list(cost=c(0.1,1,100,100,1000)),tune.control=(cross=5))
summary(tune.out1)
bestmod.linear=tune.out1$best.model
summary(bestmod.linear)

tune.out2=tune(svm,Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=TrainD,kernel="radial",
               ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out2)
bestmod.radial=tune.out2$best.model
summary(bestmod.radial)

tune.out3=tune(svm,Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=TrainD,kernel="polynomial",
               ranges=list(cost=c(0.1,1,10,100,1000),gamma=1,degree=3))
summary(tune.out3)
bestmod.poly=tune.out3$best.model
summary(bestmod.poly)

ypred=predict(bestmod.radial,TestD)
table(predict=ypred, truth=TestD$Direction)

#ROC
rocplot=function(pred, truth, ...){
        predob = prediction(pred, truth)
        perf = performance (predob , "tpr", "fpr")
        plot(perf ,...)}

#linear
##training data
fitted=attributes(predict(bestmod.linear,TrainD,decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted ,TrainD$Direction,main="Training Data")

fitted=attributes(predict(bestmod.radial,TR,decision.values=T))$decision.values
rocplot(fitted ,dat[train,"y"],add=T,col="red")
##testing data
fitted=attributes(predict(svmfit.opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted ,dat[-train,"y"],main="Test Data")
fitted=attributes(predict(svmfit.flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted ,dat[-train,"y"],add=T,col="red")
#####SVM with multiple classes


