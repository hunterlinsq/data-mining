library(ncvreg)
data(heart)
x <- as.matrix(heart[,1:9])
y <- heart$chd

#lasso
lasso.mod <- ncvreg(x,y,family = "binomial",penalty = "lasso")
plot(lasso.mod)
lasso.mod1 <- cv.ncvreg(x,y,family = "binomial",penalty = "lasso")
plot(lasso.mod1)
coef(lasso.mod1)
#mcp
mcp.mod <- ncvreg(x,y,family = "binomial",penalty = "MCP")
plot(mcp.mod)
mcp.mod1 <- cv.ncvreg(x,y,family = "binomial",penalty = "MCP")
plot(mcp.mod1)
coef(mcp.mod1)
#SCAD
scad.mod <- ncvreg(x,y,family = "binomial",penalty = "SCAD")
plot(scad.mod)
scad.mod1 <- cv.ncvreg(x,y,family = "binomial",penalty = "SCAD")
plot(scad.mod1)
coef(scad.mod1)


#2
library(MASS)
#(1)
fun1 <- function(n,p,rou){
        sigma <- matrix(NA,p,p)
        for(i in 1:p){
                for(j in 1:p){
                        sigma[i,j] <- rou^abs(i-j)
                        sigma[j,i] <- rou^abs(i-j)
                }
        }
        x <- mvrnorm(n=n,mu=rep(0,p),Sigma = sigma)
        return(x)
}

#(2)
fun2 <- function(n,p,rou){
        sigma <- matrix(rou,p,p)
        for(i in 1:p{
                sigma[i,i] <- 1
        }
        x <- mvrnorm(n=n,mu=rep(0,p),Sigma = sigma)
        return(x)
}
