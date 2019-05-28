# R Lab 6

# Today we will breifly discuss transformation (mostly Box-Cox)
# We will then introduce tricks to create Dummy variable

#Set working directory

#When you define variable try to use short names with clear meaning
#add comment otherwise
n <- 75
b0 <- 5
b1 <- 2
x <- runif(n,0,50)

y <- b0 +b1*x+rnorm(n,0,10)
y <- y^2

plot(x,y)

#Fit the model and plot the fitted line
model <- lm(y~x)
par(mfrow=c(1,1))
plot(x,y)
abline(model$coefficients[1],model$coefficients[2],col='red')

par(mfrow=c(2,2))
plot(lm(model))


#Transformation
model <- lm(sqrt(y)~x)
par(mfrow=c(1,1))
plot(x,sqrt(y))
abline(model$coefficients[1],model$coefficients[2],col='red')

par(mfrow=c(2,2))
plot(lm(model))

#Well we knew the exact transformation needed, in real life it's not going to be the case.
#If we did not we could have given Box-Cox a try!

par(mfrow=c(1,1))
#Install library MASS!
library(MASS)
model <- lm(y~x)
?boxcox
boxcox(model)
boxcox(model,plotit=T,lambda=seq (0.3, 0.8, by=0.05))

# Box-Cox was pretty much succesfull at identifying lambda as 0.5!
# See it's not alllll bad.

#Alright, let's import Krafft data set again, and see how to create dummy variables
#We will also check how ANOVA and lm output relates
data <- read.table('krafft.txt')
head(data) #Thank you head!

data <- read.table('krafft.txt',head=TRUE)
head(data) 

anova(lm(data$RA~as.factor(data$GROUP)))
# Notice first that there is a "choice" in the base group
# Sometimes you there is a logical choice (A new technique\drug)
# Sometimes it is harder to justify it
x1 <- as.integer(data$GROUP==2)
x2 <- as.integer(data$GROUP==3)
x3 <- as.integer(data$GROUP==4)
summary(lm(data$RA~x1+x2+x3))

#Let's pick a continuous variable 
plot(data$RA~data$VTINV)
summary(lm(data$RA~data$VTINV))
anova(lm(data$RA~data$VTINV))
#IMPORTANT : Remember the problem we had :
anova(lm(data$RA~data$GROUP))
# Well in R consider group a numerical variable, performed lm regression on it
# and the ANOVA output is the (SSReg/p-1)/(SSE/n-p)!
# we really don't need it anymore!


#Finally let's take a long look at a typical MLR output
G2 <- as.integer(data$GROUP==2)
G3 <- as.integer(data$GROUP==3)
G4 <- as.integer(data$GROUP==4)
summary(lm(data$RA~data$VTINV+data$DIPINV+data$HEAT+data$KPOINT+G2+G3+G4))
