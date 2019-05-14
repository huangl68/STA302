#Welcome to our third Rlab!


#Iv'e had questions regarding boxplots
#Sincethey are usefull blots to visualize the data, let's talk about them quickly
# https://towardsdatascience.com/understanding-boxplots-5e2df7bcbd51
?boxplot
x <- rnorm(n=1000,0,1)
boxplot(x)

#Look at Craig Burkets boxplots for ANOVA

#I'd like to introduce to you how to write simple functions

add <- function(a,b){
  to_return <- a+b
  return(to_return)
}

add(2,3)

#Ok, that was not very helpfull

#Setting a default value for some parameter
mult <- function(a,b=1){
  to_return <- a*b
  return(to_return)
}

mult(2,2)
mult(3)

#We can easily define function using other functions

fact <- function(a){
  to_return <-1
  while(a>1){
    to_return <- mult(to_return,a)
    a <- a-1
  }
  
  return(to_return)
}

factorial(5)
fact(5)

#Ok, let's talk about today topic, why no coding a function that 
#return beta hat ?

#Let's generate a data set first

yfun <- function(x,b0,b1){
  y <- b0+b1*x + runif(length(x),-5,5)
  return(y)
}

#When you define variable try to use short names with clear meaning
#add comment otherwise
n <- 50
b0 <- 5
b1 <- 2
x <- runif(n,0,20)

y <- yfun(x,b0,b1)

#Let's plot it !
?plot
plot(x,y,main="Our little simulated example")
plot(x,y,main="Our little simulated example",ylim=c(0,50),xlim=c(0,25))

#Beta hat estimator
#Proper way to define a function is to add an heading that indicates
#what the function does and what are the inputs and ouputs.

#You can also add comments in the function


#linreg produces LSE estimates for beta0 and beta1
#Input : x the vector of observed predictor and y the vector of observed output
#Output : a vector of size 2 containing both estimates
linreg <- function(x,y) {
  #Add a column of 1 to the vector to x
  X <- matrix(c(rep(1,length(x)),x),nrow=length(x),ncol=2)
  #Get the LSE estimates
  beta <- solve((t(X)%*%X))%*%t(X)%*%y
  return(beta)
}  

beta <- linreg(x,y)
abline(a=beta[1],b=beta[2],col='red')


# Let's conclude with a quick look at where R^2 is found in the lm output
# lm stands for linear model and will be explained next week
summary(lm(y~x))




#What if there was more "randomness"
yfun <- function(x,b0,b1){
  y <- b0+b1*x + runif(length(x),-20,20)
  return(y)
}

#When you define variable try to use short names with clear meaning
#add comment otherwise
n <- 50
b0 <- 5
b1 <- 2
x <- runif(n,0,20)

y <- yfun(x,b0,b1)

plot(x,y,main="Our little simulated example")
plot(x,y,main="Our little simulated example",ylim=c(0,50),xlim=c(0,25))
summary(lm(y~x))
