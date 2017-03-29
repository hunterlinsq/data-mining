library(ISLR)
data(Hitters)
Hitters <- na.omit(Hitters)

#最优子集法
library(leaps)
regfit.full <- regsubsets(Salary~.,Hitters)
summmary(regfit.full)
regfit.full <- regsubsets(Salary~.,Hitters,nvmax=19) #子集数从1变量到19变量
reg.summarry <- summmary(regfit.full)
plot(reg.summarry$rss,xlab = "Number of variables", ylab = "RSS",type = "l")
plot(reg.summarry$bic,xlab = "Number of variables", ylab = "BIC",type = "l")
plot(reg.summarry$adjr2,xlab = "Number of variables", ylab = "Adjusted RSq",type = "l")
which.max(reg.summarry$adjr2)
points(11,reg.summarry$adjr2[11],col="red",cex=2,pch=20)
coef(regfit.full,11)  #11个变量时的系数

#stepwise
regfit.fwd = regsubsets(Salary~.,data=Hitters ,nvmax =19,method ="forward")
summary(regfit.fwd)
coef(regfit.fwd,7)   #7个变量时的系数
regfit.bwd = regsubsets(Salary~.,data=Hitters ,nvmax =19,method ="backward")
summary(regfit.bwd)

##have different results
##choosing models using Validation and CV
set.seed(1)
train=sample(c(TRUE,FALSE),nrow(Hitters),replace = TRUE)
test=(!train)
regfit.best <- regsubsets(Salary~.,data = Hitters[train,],nvmax = 19)
test.mat=model.matrix(Salary~.,data = Hitters[test,])
#model.matrix自动把一些分类数据自动转化成虚拟变量，把x构造成design matrix
val.error <- rep(NA,19)
for(i in 1:19){
        coefi=coef(regfit.best,id=i)
        pred=test.mat[,names(coefi)]%*%coefi
        val.error[i]=mean((Hitters$Salary[test]-pred)^2)
        
}
val.error
which.min(val.error)   #10
plot(1:19,val.error,type="b")
coef(regfit.best,10)
#以上其实是validation test的方法

#CV
#writing function predict
library(glmnet)
predict.regfit <- function(object,newdata,id,...){
        form=as.formula(object$call[[2]]) #变量名
        mat=model.matrix(form,newdata)
        coefi=coef(object,id=id)
        xvars=names(coefi)
        mat[,xvars] %*% coefi
}

regfit.best=regsubsets(Salary~.,data=Hitters,nvmax = 19)
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace = TRUE)
cv.error=matrix(NA,k,19,dimnames = list(NULL,paste(1:19)))

for(j in 1:k){
        best.fit <- regsubsets(Salary~.,data = Hitters[folds!=j])
        for(i in 1:19){
                pred=predict(best.fit,Hitters[folds==j,],id=i)
                cv.error[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
        }
}
mean.cv.errors=apply(cv.error,2,mean)
plot(mean.cv.errors,type = "b")
which.min(mean.cv.errors)  #11
reg.best=regsubsets(Salary~.,data=Hitters,nvmax = 19)
coef(reg.best,11)


############################
##ridge
library(glmnet)
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary
grid=10^seq(10,-2,length=100)
grid
ridge.mod=glmnet(x,y,alpha = 0,lambda = grid)   #alpha=0代表ridge,=1为lasso，取0到1中间是elastic—net
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

plot(ridge.mod)
cv.out <- cv.glmnet(x,y,alpha=0,lambda = grid)
plot(cv.out)   #第二条竖线代表往前一个standard error(精简变量数角度)
coef(cv.out)  
bestridge=cv.out$lambda.min

##lasso
lasso.mod=glmnet(x,y,alpha = 1,lambda = grid)
plot(lasso.mod)
par(mfrow=c(1,1))
lasso2 <- cv.glmnet(x,y,alpha=1,lambda = grid)
plot(lasso2)

#MCP
library(ncvreg)
fit1 <- ncvreg(x,y,family = "gaussian",penalty = "MCP")
plot(fit1)
fit11 <- cv.ncvreg(x,y,family = "gaussian",penalty = "MCP")
plot(fit11)
coef(fit11)
