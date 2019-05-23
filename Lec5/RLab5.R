# R Lab 5

# Today we will mostly iintroduce the Diagnostic graphics we talked about today.
# We will also do briefly introduce R Markdown (just to get you curious)

yfun <- function(x,b0,b1){
  y <- b0+b1*x + runif(length(x),-5,5)
  return(y)
}

n <- 50
b0 <- 5
b1 <- 2
x <- runif(n,0,20)

y <- yfun(x,b0,b1)


model <- lm(y~x)
par(mfrow=c(1,1))
plot(x,y)
abline(model$coefficients[1],model$coefficients[2],col='red')
# You can acutally plot a linear model to produce the important graph for diagnostics:
plot(model)

# You can tell r to produce an image containing multiple plots 
par(mfrow=c(2,2))
plot(lm(model))


yfun <- function(x,b0,b1){
  y <- b0+b1*x + rnorm(length(x),0,5)
  return(y)
}


y <- yfun(x,b0,b1)




model <- lm(y~x)
par(mfrow=c(1,1))
plot(x,y)
abline(model$coefficients[1],model$coefficients[2],col='red')

# You can tell r to produce an image containing multiple plots 
par(mfrow=c(2,2))
plot(lm(model))


yfun <- function(x,b0,b1){
  y <- b0+b1*x^2 + rnorm(length(x),0,5)
  return(y)
}


y <- yfun(x,b0,b1)
model <- lm(y~x)
par(mfrow=c(1,1))
plot(x,y)
abline(model$coefficients[1],model$coefficients[2],col='red')

par(mfrow=c(2,2))
plot(lm(model))


yfun <- function(x,b0,b1){
  y <- b0+b1*x + rnorm(length(x),0,5)
  return(y)
}


y <- yfun(x,b0,b1)
x <- c(x,25)
y <- c(y,100)
model <- lm(y~x)
par(mfrow=c(1,1))
plot(x,y)
abline(model$coefficients[1],model$coefficients[2],col='red')

par(mfrow=c(2,2))
plot(lm(model))

# To use rmarkdown, install the package
# Use online ressources to learn how to use
# Let me show you a quick project I did
