library(ISLR)
attach(Auto)
library(MASS)
library(boot)

#(a)
mse <- matrix(NA,10,10)
for(i in 1:10){
        train=sample(392,196)
        for(j in 1:10){
                lm.fit=lm(mpg~poly(horsepower,j),data=Auto,subset=train) 
                mse[i,j]<- mean((mpg-predict(lm.fit,Auto))[-train]^2)
        }
}
plot(1:10,mse[1,],xlab = "degree of poly",ylab = "mse",ylim = c(15,30),type = "l",col=1)
for(i in 2:10){
        lines(1:10,mse[i,],col=i)
}


#(b)
mse.10 <- matrix(NA,10,10)
for(i in 1:10){
        for (j in 1:10){
                glm.fit <- glm(mpg~poly(horsepower,j),data=Auto)
                mse.10[i,j]=cv.glm(Auto,glm.fit,K=10)$delta[1]
                }
}

plot(1:10,mse.10[1,],xlab = "degree of poly",ylab = "mse",ylim = c(18,25),type = "l",col=1)
for(i in 2:10){
        lines(1:10,mse.10[i,],col=i)
}


#(c)
library(MASS)
sigma <- matrix(c(1,0.5,0.5,1.25),2)

alpha.fn=function(data){
        X=data$X
        Y=data$Y
        return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y))) 
}
alpha_sim <- rep(NA,1000)
for(i in 1:1000){
        dat <- data.frame(mvrnorm(n=100,mu=c(0,0),Sigma = sigma))
        colnames(dat) <- c("X","Y")
        alpha_sim[i] <- alpha.fn(dat)
}

alpha_boot <- rep(NA,1000)
colnames(dat) <- c("X","Y")
for(i in 1:1000){
        dat <- Portfolio[sample(100,100,replace=T),]
        colnames(dat) <- c("X","Y")
        alpha_boot[i] <- alpha.fn(dat)
}

par(mfrow=c(1,3))
hist(alpha_sim,xlab = "alpha");abline(v=0.6,col="blue")
hist(alpha_boot,xlab = "alpha");abline(v=0.6,col="blue")
boxplot(alpha_sim,alpha_boot,names = c("TRUE","boot"));abline(h=0.6,col="blue")
