#R-lab9 !!!

# I'm not sure those R-lab were a good idea in the end
# Perhaps I should have progressively introduced the functionand output
#directly in the course.

#Let's end up the class by introducing functions and ouputs related to PCA,Ridge and Lasso.

#If you ever need them you can use the Rlab

#By the way, you can use anything from the course you'd like to 
#(just give me some credit hehehe)


#Let's get going

##################################################
#PCA
##################################################



# Restuarant data (because I felt uninspired)

NYC <- read.csv('nyc.csv',head=TRUE,sep=',')

pairs(NYC[,c(3,4,5,6)])

model <- lm(Price~Food+Decor+Service, data=NYC)
sum <- summary(model)
sum

?prcomp
pr <- prcomp(NYC[,c(4,5,6)],center=TRUE)
pr

# Or we can easily compute it manually.
n <- nrow(NYC)
X <- as.matrix(NYC[,c(4,5,6)])
eig <- eigen(cov(X))
eig
sqrt(eig$values)

#OK I love food but the data is too small
library(faraway)
data(seatpos)
head(seatpos)


X <- as.matrix(seatpos[,1:8])
y <- as.matrix(seatpos$hipcenter)

model <- lm(y~X)
sum <- summary(model)
sum

pr = prcomp(X)
# Proportion of variance
sdev <- pr$sdev
sdev/sum(sdev)

#Selecte the first three components
Proj_Mat = pr$rotation[,c(1,2,3)]
dim(Proj_Mat)
dim(X)
Z  = X%*%Proj_Mat
dim(Z)

model <- lm(y~Z)
sum <- summary(model)
sum


#Collinearity issues completly solved, better Adjusted R2, perfect



##################################################
#Ridge and Lasso
##################################################

# install.packages("glmnet")
library(glmnet)

#OK I love food but the data is too small

##################################################
#Ridge
##################################################


#glmnet fit
n = nrow(seatpos)
y  <- as.matrix(seatpos$hipcenter)-mean(seatpos$hipcenter)
XC <- as.matrix(scale(X,scale=FALSE))

fit.ridge <- glmnet(XC,y,alpha=0,family='gaussian')
plot(fit.ridge,xvar="lambda",label=TRUE)

ridge_cv <- cv.glmnet(XC,y,alpha=0,family='gaussian')
plot(ridge_cv)
lam <- ridge_cv$lambda.min

#lambda.1se largest value of lambda such that error is within 1 standard error of the minimum
log(ridge_cv$lambda.1se)

model <- glmnet(XC,y,alpha=0,family='gaussian',lambda=lam)
print(model)
coef(model)

# By thr way we can also easily code our own beta estimator 

#Ridge Regression parameter estimator
#Input : X predictors matrix, Y Response Matrix and l the size of the penalty
#Output : \hat\beta_ridge
linregR <- function(X,Y,l) {
  I <- diag(x=1,nrow= ncol(X))
  beta <- solve((t(X)%*%X)+ l*I )%*%t(X)%*%Y
  return(beta)
}

##################################################
#Lasso
##################################################


fit.lasso <- glmnet(XC,y,alpha=1,family='gaussian')
plot(fit.lasso,xvar="lambda",label=TRUE)

lasso_cv <- cv.glmnet(XC,y,alpha=1,family='gaussian',nfolds=15)
plot(lasso_cv)
lam <- lasso_cv$lambda.min

model <- glmnet(XC,y,alpha=1,family='gaussian',lambda=lam)
print(model)
coef(model)
coef <- coef(lasso_cv,s=lam)[-1]


# Finally, quickly note the existence of selectiveInference package!
library(selectiveInference)

fixedLassoInf(XC,y,coef,lambda=lam,family='gaussian')

# Can also do Forward stepwise regression and post-selc inference :D


