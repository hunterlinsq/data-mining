#1
n <- 100
x <- rnorm(n,2,sqrt(2))
e <- rnorm(n)
y <- 2+3*x+e
plot(x,y)
lm.model <- lm(y~x)
abline(coef = c(lm.model$coefficients))
abline(coef = c(2,3),col="red")


#2
m <- 1000
coef1 <- rep(0,m)
coef2 <- rep(0,m)
for(i in 1:m){
        n <- 100
        x <- rnorm(n,2,sqrt(2))
        e <- rnorm(n)
        y <- 2+3*x+e
        lm.model <- lm(y~x)
        coef1[i] <- lm.model$coefficients[1]
        coef2[i] <- lm.model$coefficients[2]
}

boxplot(coef1)
boxplot(coef2)

mean <- c(mean(coef1),mean(coef2))  #均值
med <- c(median(coef1),median(coef2)) #中位数
mean
med
