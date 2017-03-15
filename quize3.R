#6
b0 <- -6
b1 <- 0.05
b2 <- 1
#(a)
x1 <- 40
x2 <- 3.5
p <- exp(b0+ b1*x1 +b2*x2)/(1+exp(b0+b1*x1 +b2*x2))
p
#(b)
p <- 0.5
link <- log(p/(1-p))
x1 <- (link-b0-b2*x2)/b1
x1

#12
#(a)
power <- function(){
        print(2^3)
}
power()

#(b)
Power2 = function(x, a) {
        x^a
}
power2(3,8)

#(c)
power2(10,3)
power2(8,17)
power2(131,3)

#(d)
power3 = function(x, a) {
        result = x^a
        return(result)
}

#(e)
x <- 1:10
y <- power3(x,2)
plot(x, y, log = "xy", ylab = "Log of y = x^2", xlab = "Log of x", 
     main = "Log of x^2 versus Log of x")

#(f)
PlotPower = function(x, a) {
        plot(x, Power3(x, a))
}
PlotPower(1:10, 3)



#simulate
a <- data.frame(x1=rnorm(30,0,1),x2=rnorm(30,0,1),class=rep("a",30))
b <- data.frame(x1=rnorm(30,1,1),x2=rnorm(30,1,1),class=rep("b",30))

data <- rbind(a,b)
train <- rbind(a[1:20,],b[1:20,])
test <- rbind(a[-c(1:20),],b[-c(1:20),])

##KNN
library(class)
knn.pre <- knn(train[,1:2],test[,1:2],train[,3],k=1)
table(knn.pre,test[,3])

knn.pre2 <- knn.cv(train[,1:2],train[,3],k=3)
table(knn.pre2,train[,3])

#lda
ldamodel <- lda(class~x1+x2,data=train)
ldamodel
lda.pred=predict(ldamodel,test[,1:2])
table(test[,3],lda.pred$class)

#logstic
logmodel <- glm(class~x1+x2,data=train,family = binomial)
glm.probs=predict(logmodel,test[,1:2],type="response") 
glm.pre <- ifelse(glm.probs>0.5,"b","a")
table(glm.pre,test[,3])

#qda
qdamodel <- qda(class~x1+x2,data=train)
qdamodel
qda.pred=predict(qdamodel,test[,1:2])
table(test[,3],qda.pred$class)
