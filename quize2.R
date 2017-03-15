library(MASS)
attach(Boston)
barplot(medv)
boxplot(medv)
plot(density(medv))
min <- min(medv)
max <- max(medv)
median(medv)
quantile(medv,0.25)  #下四分位数
quantile(medv,0.75)  #上四分位数
sd(medv)

#(2)
plot(medv,lstat)
cor(medv,lstat)  #相关系数

#(3)
lm.model <- lm(medv~lstat)
summary(lm.model)
#可以看到系数都显著

#(4)
predict(lm.model,data.frame(lstat=c(5,10,15)))

detach(Boston)

