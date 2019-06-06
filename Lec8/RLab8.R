# R Lab8

library(faraway)
# A simple example
# Generating a data set

n = 15
b0 = 5
b1 = 2
b2 = 10
b3 = 5

x = runif(n,-5,5)

y =  b0 +b1*x + b2*x^2 + b3*x^3 +rnorm(n,0,100)

# plot it and add the "true" mean 
plot(x,y)

xl = seq(-10,10,by=0.1)
yl = b0 +b1*xl + b2*xl^2 + b3*xl^3

lines(xl,yl,type='l',col='blue')

x2 <- x^2
x3 <- x^3
x4 <- x^4
x5 <- x^5

# Too much polynomial terms may lead to collinearity problems
cor(cbind(x,x2,x3,x4,x5))
vif(model)
X <- cbind(rep(1,n),x,x2,x3,x4,x5)
eigen(t(X)%*%X)$values

#Try multiple models
#?AIC ?BIC
model <- lm(y~x)
sum <- summary(model)
sum
sum$adj.r.squared
AIC(model)
BIC(model)
model <- lm(y~x+x2)
sum <- summary(model)
sum
sum$adj.r.squared
AIC(model)
BIC(model)
model <- lm(y~x+x2+x3)
sum <- summary(model)
sum
sum$adj.r.squared
AIC(model)
BIC(model)
model <- lm(y~x+x2+x3+x4)
sum <- summary(model)
sum
sum$adj.r.squared
AIC(model)
BIC(model)
model <- lm(y~x+x2+x3+x4+x5)
sum <- summary(model)
sum
sum$adj.r.squared
AIC(model)
BIC(model)



#Finally let's try to solve seatpos collinearity issue
data(seatpos)
head(seatpos)

# ~. mean explained by all the rest
model <- lm(hipcenter~.,data=seatpos)
summary(model)
AIC(model)
BIC(model)

#backward selection, take out Ht
model <- lm(hipcenter~Age+Weight+HtShoes+Seated+Arm+Thigh+Leg,data=seatpos)
summary(model)
AIC(model)
BIC(model)

#backward selection, take out weight
model <- lm(hipcenter~Age+HtShoes+Seated+Arm+Thigh+Leg,data=seatpos)
summary(model)
AIC(model)
BIC(model)

#backward selection, take out Seated
model <- lm(hipcenter~Age+HtShoes+Arm+Thigh+Leg,data=seatpos)
summary(model)
AIC(model)
BIC(model)

#backward selection, take out Arm
model <- lm(hipcenter~Age+HtShoes+Thigh+Leg,data=seatpos)
summary(model)
AIC(model)
BIC(model)

#backward selection, take out Arm
model <- lm(hipcenter~Age+HtShoes+Leg,data=seatpos)
summary(model)
AIC(model)
BIC(model)

#backward selection, take out Age
model <- lm(hipcenter~HtShoes+Leg,data=seatpos)
summary(model)
AIC(model)
BIC(model)

#Notice BIC, AdjR2 and AIC tell different stories




